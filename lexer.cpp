#include <string.h>
#include <stdlib.h>

#include "gmqcc.h"
#include "lexer.h"

/*
 * List of Keywords
 */

/* original */
static const char *keywords_qc[] = {
    "for", "do", "while",
    "if", "else",
    "local",
    "return",
    "const"
};
/* For fte/gmgqcc */
static const char *keywords_fg[] = {
    "switch", "case", "default",
    "break", "continue",
    "typedef",
    "goto",

    "__builtin_debug_printtype"
};

/*
 * Lexer code
 */
static char* *lex_filenames;

static void lexerror(lex_file *lex, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    if (lex)
        con_vprintmsg(LVL_ERROR, lex->name, lex->sline, lex->column, "parse error", fmt, ap);
    else
        con_vprintmsg(LVL_ERROR, "", 0, 0, "parse error", fmt, ap);
    va_end(ap);
}

static bool lexwarn(lex_file *lex, int warntype, const char *fmt, ...)
{
    bool      r;
    lex_ctx_t ctx;
    va_list   ap;

    ctx.file   = lex->name;
    ctx.line   = lex->sline;
    ctx.column = lex->column;

    va_start(ap, fmt);
    r = vcompile_warning(ctx, warntype, fmt, ap);
    va_end(ap);
    return r;
}

static void lex_token_new(lex_file *lex)
{
    lex->tok.value.shrinkto(0);

    lex->tok.constval.t  = TYPE_VOID;
    lex->tok.ctx.line    = lex->sline;
    lex->tok.ctx.file    = lex->name;
    lex->tok.ctx.column  = lex->column;
}

static void lex_ungetch(lex_file *lex, Token ch);
static Token lex_getch(lex_file *lex);

lex_file* lex_open(const char *file)
{
    lex_file  *lex;
    FILE *in = fopen(file, "rb");
    uint32_t   read;

    if (!in) {
        lexerror(nullptr, "open failed: '%s'\n", file);
        return nullptr;
    }

    lex = (lex_file*)mem_a(sizeof(*lex));
    if (!lex) {
        fclose(in);
        lexerror(nullptr, "out of memory\n");
        return nullptr;
    }

    memset(lex, 0, sizeof(*lex));

    lex->file    = in;
    lex->name    = util_strdup(file);
    lex->line    = 1; /* we start counting at 1 */
    lex->column  = 0;
    lex->peekpos = 0;
    lex->eof     = false;

    /* handle BOM */
    if ((read = (lex_getch(lex) << 16) | (lex_getch(lex) << 8) | lex_getch(lex)) != 0xEFBBBF) {
        lex_ungetch(lex, static_cast<Token>((read & 0x0000FF)));
        lex_ungetch(lex, static_cast<Token>((read & 0x00FF00) >> 8));
        lex_ungetch(lex, static_cast<Token>((read & 0xFF0000) >> 16));
    } else {
        /*
         * otherwise the lexer has advanced 3 bytes for the BOM, we need
         * to set the column back to 0
         */
        lex->column = 0;
    }

    vec_push(lex_filenames, lex->name);
    return lex;
}

lex_file* lex_open_string(const char *str, size_t len, const char *name)
{
    lex_file *lex;

    lex = (lex_file*)mem_a(sizeof(*lex));
    if (!lex) {
        lexerror(nullptr, "out of memory\n");
        return nullptr;
    }

    memset(lex, 0, sizeof(*lex));

    lex->file = nullptr;
    lex->open_string        = str;
    lex->open_string_length = len;
    lex->open_string_pos    = 0;

    lex->name    = util_strdup(name ? name : "<string-source>");
    lex->line    = 1; /* we start counting at 1 */
    lex->peekpos = 0;
    lex->eof     = false;
    lex->column  = 0;

    vec_push(lex_filenames, lex->name);

    return lex;
}

void lex_cleanup()
{
    for (size_t i = 0; i < vec_size(lex_filenames); ++i)
        mem_d(lex_filenames[i]);
    vec_free(lex_filenames);
}

void lex_close(lex_file *lex)
{
    vec_free(lex->frames);

    if (lex->file)
        fclose(lex->file);

    /* mem_d(lex->name); collected in lex_filenames */
    mem_d(lex);
}



static Token lex_fgetc(lex_file *lex)
{
    if (lex->file) {
        lex->column++;
        auto c = fgetc(lex->file);
        return c == EOF ? Token::END : static_cast<Token>(c);
    }
    if (lex->open_string) {
        if (lex->open_string_pos >= lex->open_string_length)
            return Token::END;
        lex->column++;
        auto c = lex->open_string[lex->open_string_pos++];
        return static_cast<Token>(c);
    }
    return Token::END;
}

/* Get or put-back data
 * The following to functions do NOT understand what kind of data they
 * are working on.
 * The are merely wrapping get/put in order to count line numbers.
 */
static Token lex_try_trigraph(lex_file *lex, Token old)
{
    auto c2 = lex_fgetc(lex);
    if (!lex->push_line && c2 == Token::LF) {
        lex->line++;
        lex->column = 0;
    }

    if (c2 != Token::QUESTION) {
        lex_ungetch(lex, c2);
        return old;
    }

    auto c3 = lex_fgetc(lex);
    if (!lex->push_line && c3 == Token::LF) {
        lex->line++;
        lex->column = 0;
    }

    switch (c3) {
        case Token::EQ: return Token::HASH;
        case Token::DIV: return Token::BACKSLASH;
        case Token::QUOT_SINGLE: return Token::XOR;
        case Token::PAREN_OPEN: return Token::BRACKET_OPEN;
        case Token::PAREN_CLOSE: return Token::BRACKET_CLOSE;
        case Token::NOT: return Token::OR;
        case Token::LT: return Token::BRACE_OPEN;
        case Token::GT: return Token::BRACE_CLOSE;
        case Token::SUB: return Token::BITNOT;
        default:
            lex_ungetch(lex, c3);
            lex_ungetch(lex, c2);
            return old;
    }
}

static Token lex_try_digraph(lex_file *lex, Token ch)
{
    auto c2 = lex_fgetc(lex);
    /* we just used fgetc() so count lines
     * need to offset a \n the ungetch would recognize
     */
    if (!lex->push_line && c2 == Token::LF)
        lex->line++;
    if      (ch == Token::LT && c2 == Token::COLON)
        return Token::BRACKET_OPEN;
    else if (ch == Token::COLON && c2 == Token::GT)
        return Token::BRACKET_CLOSE;
    else if (ch == Token::LT && c2 == Token::MOD)
        return Token::BRACE_OPEN;
    else if (ch == Token::MOD && c2 == Token::GT)
        return Token::BRACE_CLOSE;
    else if (ch == Token::MOD && c2 == Token::COLON)
        return Token::HASH;
    lex_ungetch(lex, c2);
    return ch;
}

static Token lex_getch(lex_file *lex)
{
    if (lex->peekpos) {
        lex->peekpos--;
        if (!lex->push_line && lex->peek[lex->peekpos] == Token::LF) {
            lex->line++;
            lex->column = 0;
        }
        return lex->peek[lex->peekpos];
    }

    auto ch = lex_fgetc(lex);
    if (!lex->push_line && ch == Token::LF) {
        lex->line++;
        lex->column = 0;
    }
    else if (ch == Token::QUESTION)
        return lex_try_trigraph(lex, ch);
    else if (!lex->flags.nodigraphs && (ch == Token::LT || ch == Token::COLON || ch == Token::MOD))
        return lex_try_digraph(lex, ch);
    return ch;
}

static void lex_ungetch(lex_file *lex, Token ch)
{
    lex->peek[lex->peekpos++] = ch;
    lex->column--;
    if (!lex->push_line && ch == Token::LF) {
        lex->line--;
        lex->column = 0;
    }
}

/* classify characters
 * some additions to the is*() functions of ctype.h
 */

/* Idents are alphanumberic, but they start with alpha or _ */
static bool isident_start(int ch)
{
    return util_isalpha(ch) || ch == '_';
}

static bool isident(int ch)
{
    return isident_start(ch) || util_isdigit(ch);
}

/* isxdigit_only is used when we already know it's not a digit
 * and want to see if it's a hex digit anyway.
 */
static bool isxdigit_only(int ch)
{
    return (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F');
}

/* Append a character to the token buffer */
static void lex_tokench(lex_file *lex, int ch)
{
    lex->tok.value.push(ch);
}

/* Append a trailing null-byte */
static void lex_endtoken(lex_file *lex)
{
    lex->tok.value.push(0);
    lex->tok.value.shrinkby(1);
}

static bool lex_try_pragma(lex_file *lex)
{
    char *pragma  = nullptr;
    char *command = nullptr;
    char *param   = nullptr;
    size_t line;

    if (lex->flags.preprocessing)
        return false;

    line = lex->line;

    auto ch = lex_getch(lex);
    if (ch != Token::HASH) {
        lex_ungetch(lex, ch);
        return false;
    }

    for (ch = lex_getch(lex); vec_size(pragma) < 8 && ch >= 'a' && ch <= 'z'; ch = lex_getch(lex))
        vec_push(pragma, ch);
    vec_push(pragma, 0);

    if (ch != Token::WS|| strcmp(pragma, "pragma")) {
        lex_ungetch(lex, ch);
        goto unroll;
    }

    for (ch = lex_getch(lex); vec_size(command) < 32 && ch >= 'a' && ch <= 'z'; ch = lex_getch(lex))
        vec_push(command, ch);
    vec_push(command, 0);

    if (ch != Token::PAREN_OPEN) {
        lex_ungetch(lex, ch);
        goto unroll;
    }

    for (ch = lex_getch(lex); vec_size(param) < 1024 && ch != Token::PAREN_CLOSE && ch != Token::LF; ch = lex_getch(lex))
        vec_push(param, ch);
    vec_push(param, 0);

    if (ch != Token::PAREN_CLOSE) {
        lex_ungetch(lex, ch);
        goto unroll;
    }

    if (!strcmp(command, "push")) {
        if (!strcmp(param, "line")) {
            lex->push_line++;
            if (lex->push_line == 1)
                --line;
        }
        else
            goto unroll;
    }
    else if (!strcmp(command, "pop")) {
        if (!strcmp(param, "line")) {
            if (lex->push_line)
                lex->push_line--;
            if (lex->push_line == 0)
                --line;
        }
        else
            goto unroll;
    }
    else if (!strcmp(command, "file")) {
        lex->name = util_strdup(param);
        vec_push(lex_filenames, lex->name);
    }
    else if (!strcmp(command, "line")) {
        line = strtol(param, nullptr, 0)-1;
    }
    else
        goto unroll;

    lex->line = line;
    while (ch != Token::LF && ch != Token::END)
        ch = lex_getch(lex);
    vec_free(command);
    vec_free(param);
    vec_free(pragma);
    return true;

unroll:
    if (command) {
        vec_pop(command);
        while (vec_size(command)) {
            lex_ungetch(lex, static_cast<Token>(vec_last(command)));
            vec_pop(command);
        }
        vec_free(command);
        lex_ungetch(lex, Token::WS);
    }
    if (param) {
        vec_pop(param);
        while (vec_size(param)) {
            lex_ungetch(lex, static_cast<Token>(vec_last(param)));
            vec_pop(param);
        }
        vec_free(param);
        lex_ungetch(lex, Token::WS);
    }
    if (pragma) {
        vec_pop(pragma);
        while (vec_size(pragma)) {
            lex_ungetch(lex, static_cast<Token>(vec_last(pragma)));
            vec_pop(pragma);
        }
        vec_free(pragma);
    }
    lex_ungetch(lex, Token::HASH);

    lex->line = line;
    return false;
}

/* Skip whitespace and comments and return the first
 * non-white character.
 * As this makes use of the above getch() ungetch() functions,
 * we don't need to care at all about line numbering anymore.
 *
 * In theory, this function should only be used at the beginning
 * of lexing, or when we *know* the next character is part of the token.
 * Otherwise, if the parser throws an error, the linenumber may not be
 * the line of the error, but the line of the next token AFTER the error.
 *
 * This is currently only problematic when using c-like string-continuation,
 * since comments and whitespaces are allowed between 2 such strings.
 * Example:
printf(   "line one\n"
// A comment
          "A continuation of the previous string"
// This line is skipped
      , foo);

 * In this case, if the parse decides it didn't actually want a string,
 * and uses lex->line to print an error, it will show the ', foo);' line's
 * linenumber.
 *
 * On the other hand, the parser is supposed to remember the line of the next
 * token's beginning. In this case we would want skipwhite() to be called
 * AFTER reading a token, so that the parser, before reading the NEXT token,
 * doesn't store teh *comment's* linenumber, but the actual token's linenumber.
 *
 * THIS SOLUTION
 *    here is to store the line of the first character after skipping
 *    the initial whitespace in lex->sline, this happens in lex_do.
 */
static Token lex_skipwhite(lex_file *lex, bool hadwhite)
{
    Token ch;
    bool haswhite = hadwhite;

    do
    {
        ch = lex_getch(lex);
        while (ch != Token::END && util_isspace(ch)) {
            if (ch == Token::LF) {
                if (lex_try_pragma(lex))
                    continue;
            }
            if (lex->flags.preprocessing) {
                if (ch == Token::LF) {
                    /* end-of-line */
                    /* see if there was whitespace first */
                    if (haswhite) { /* (vec_size(lex->tok.value)) { */
                        lex_ungetch(lex, ch);
                        lex_endtoken(lex);
                        return Token::WHITE;
                    }
                    /* otherwise return EOL */
                    return Token::EOL;
                }
                haswhite = true;
                lex_tokench(lex, ch);
            }
            ch = lex_getch(lex);
        }

        if (ch == Token::DIV) {
            ch = lex_getch(lex);
            if (ch == Token::DIV)
            {
                /* one line comment */
                ch = lex_getch(lex);

                if (lex->flags.preprocessing) {
                    haswhite = true;
                    lex_tokench(lex, Token::WS);
                    lex_tokench(lex, Token::WS);
                }

                while (ch != Token::END && ch != Token::LF) {
                    if (lex->flags.preprocessing)
                        lex_tokench(lex, Token::WS); /* ch); */
                    ch = lex_getch(lex);
                }
                if (lex->flags.preprocessing) {
                    lex_ungetch(lex, Token::LF);
                    lex_endtoken(lex);
                    return Token::WHITE;
                }
                continue;
            }
            if (ch == Token::MUL)
            {
                /* multiline comment */
                if (lex->flags.preprocessing) {
                    haswhite = true;
                    lex_tokench(lex, Token::WS);
                    lex_tokench(lex, Token::WS);
                }

                while (ch != Token::END)
                {
                    ch = lex_getch(lex);
                    if (ch == Token::MUL) {
                        ch = lex_getch(lex);
                        if (ch == Token::DIV) {
                            if (lex->flags.preprocessing) {
                                lex_tokench(lex, Token::WS);
                                lex_tokench(lex, Token::WS);
                            }
                            break;
                        }
                        lex_ungetch(lex, ch);
                    }
                    if (lex->flags.preprocessing) {
                        if (ch == Token::LF)
                            lex_tokench(lex, Token::LF);
                        else
                            lex_tokench(lex, Token::WS);
                    }
                }
                ch = Token::WS; /* cause TRUE in the isspace check */
                continue;
            }
            /* Otherwise roll back to the slash and break out of the loop */
            lex_ungetch(lex, ch);
            ch = Token::DIV;
            break;
        }
    } while (ch != Token::END && util_isspace(ch));

    if (haswhite) {
        lex_endtoken(lex);
        lex_ungetch(lex, ch);
        return Token::WHITE;
    }
    return ch;
}

/* Get a token */
static bool GMQCC_WARN lex_finish_ident(lex_file *lex)
{
    auto ch = lex_getch(lex);
    while (ch != Token::END && isident(ch))
    {
        lex_tokench(lex, ch);
        ch = lex_getch(lex);
    }

    /* last ch was not an ident ch: */
    lex_ungetch(lex, ch);

    return true;
}

/* read one ident for the frame list */
static int lex_parse_frame(lex_file *lex)
{
    lex_token_new(lex);

    auto ch = lex_getch(lex);
    while (ch != Token::END && ch != Token::LF && util_isspace(ch))
        ch = lex_getch(lex);

    if (ch == Token::LF)
        return 1;

    if (!isident_start(ch)) {
        lexerror(lex, "invalid framename, must start with one of a-z or _, got %c", ch);
        return -1;
    }

    lex_tokench(lex, ch);
    if (!lex_finish_ident(lex))
        return -1;
    lex_endtoken(lex);
    return 0;
}

/* read a list of $frames */
static bool lex_finish_frames(lex_file *lex)
{
    do {
        size_t i;
        int    rc;
        frame_macro m;

        rc = lex_parse_frame(lex);
        if (rc > 0) /* end of line */
            return true;
        if (rc < 0) /* error */
            return false;

        for (i = 0; i < vec_size(lex->frames); ++i) {
            if (lex->frames[i].name == lex->tok.value.c_str()) {
                lex->frames[i].value = lex->framevalue++;
                if (lexwarn(lex, WARN_FRAME_MACROS, "duplicate frame macro defined: `%s`", lex->tok.value))
                    return false;
                break;
            }
        }
        if (i < vec_size(lex->frames))
            continue;

        m.value = lex->framevalue++;
        m.name = util_strdup(lex->tok.value.c_str());
        lex->tok.value.shrinkto(0);
        vec_push(lex->frames, m);
    } while (true);

    return false;
}

static Token GMQCC_WARN lex_finish_string(lex_file *lex, int quote)
{
    utf8ch_t chr = 0;
    int ch = 0, texttype = 0;
    Token nextch = Token::NONE;
    bool hex;
    bool oct;
    char u8buf[8]; /* way more than enough */
    int  u8len, uc;

    while (ch != Token::END)
    {
        ch = lex_getch(lex);
        if (ch == quote)
            return Token::STRINGCONST;

        if (lex->flags.preprocessing && ch == '\\') {
            lex_tokench(lex, ch);
            ch = lex_getch(lex);
            if (ch == Token::END) {
                lexerror(lex, "unexpected end of file");
                lex_ungetch(lex, Token::END); /* next token to be Token::END */
                return (lex->tok.ttype = Token::ERROR);
            }
            lex_tokench(lex, ch);
        }
        else if (ch == '\\') {
            ch = lex_getch(lex);
            if (ch == Token::END) {
                lexerror(lex, "unexpected end of file");
                lex_ungetch(lex, Token::END); /* next token to be Token::END */
                return (lex->tok.ttype = Token::ERROR);
            }

            switch (ch) {
            case '\\': break;
            case '\'': break;
            case '"':  break;
            case 'a': ch = '\a'; break;
            case 'r': ch = '\r'; break;
            case 'n': ch = '\n'; break;
            case 't': ch = '\t'; break;
            case 'f': ch = '\f'; break;
            case 'v': ch = '\v'; break;
            case 'x':
            case 'X':
                /* same procedure as in fteqcc */
                ch = 0;
                nextch = lex_getch(lex);
                if      (nextch >= '0' && nextch <= '9')
                    ch += nextch - '0';
                else if (nextch >= 'a' && nextch <= 'f')
                    ch += nextch - 'a' + 10;
                else if (nextch >= 'A' && nextch <= 'F')
                    ch += nextch - 'A' + 10;
                else {
                    lexerror(lex, "bad character code");
                    lex_ungetch(lex, nextch);
                    return (lex->tok.ttype = Token::ERROR);
                }

                ch *= 0x10;
                nextch = lex_getch(lex);
                if      (nextch >= '0' && nextch <= '9')
                    ch += nextch - '0';
                else if (nextch >= 'a' && nextch <= 'f')
                    ch += nextch - 'a' + 10;
                else if (nextch >= 'A' && nextch <= 'F')
                    ch += nextch - 'A' + 10;
                else {
                    lexerror(lex, "bad character code");
                    lex_ungetch(lex, nextch);
                    return (lex->tok.ttype = Token::ERROR);
                }
                break;

            /* fteqcc support */
            case '0': case '1': case '2': case '3':
            case '4': case '5': case '6': case '7':
            case '8': case '9':
                ch = 18 + ch - '0';
                break;
            case '<':  ch = 29; break;
            case '-':  ch = 30; break;
            case '>':  ch = 31; break;
            case '[':  ch = 16; break;
            case ']':  ch = 17; break;
            case '{':
                chr = 0;
                nextch = lex_getch(lex);
                hex = (nextch == 'x');
                oct = (nextch == '0');
                if (!hex && !oct)
                    lex_ungetch(lex, nextch);
                for (nextch = lex_getch(lex); nextch != Token::BRACE_CLOSE; nextch = lex_getch(lex)) {
                    if (!hex && !oct) {
                        if (nextch >= '0' && nextch <= '9')
                            chr = chr * 10 + nextch - '0';
                        else {
                            lexerror(lex, "bad character code");
                            return (lex->tok.ttype = Token::ERROR);
                        }
                    } else if (!oct) {
                        if (nextch >= '0' && nextch <= '9')
                            chr = chr * 0x10 + nextch - '0';
                        else if (nextch >= 'a' && nextch <= 'f')
                            chr = chr * 0x10 + nextch - 'a' + 10;
                        else if (nextch >= 'A' && nextch <= 'F')
                            chr = chr * 0x10 + nextch - 'A' + 10;
                        else {
                            lexerror(lex, "bad character code");
                            return (lex->tok.ttype = Token::ERROR);
                        }
                    } else {
                        if (nextch >= '0' && nextch <= '9')
                            chr = chr * 8 + chr - '0';
                        else {
                            lexerror(lex, "bad character code");
                            return (lex->tok.ttype = Token::ERROR);
                        }
                    }
                    if (chr > 0x10FFFF || (!OPTS_FLAG(UTF8) && chr > 255))
                    {
                        lexerror(lex, "character code out of range");
                        return (lex->tok.ttype = Token::ERROR);
                    }
                }
                if (OPTS_FLAG(UTF8) && chr >= 128) {
                    u8len = utf8_from(u8buf, chr);
                    if (!u8len)
                        ch = 0;
                    else {
                        --u8len;
                        lex->column += u8len;
                        for (uc = 0; uc < u8len; ++uc)
                            lex_tokench(lex, u8buf[uc]);
                        /*
                         * the last character will be inserted with the tokench() call
                         * below the switch
                         */
                        ch = u8buf[uc];
                    }
                }
                else
                    ch = chr;
                break;

            /* high bit text */
            case 'b': case 's':
                texttype ^= 128;
                continue;

            case '\n':
                ch = '\n';
                break;

            default:
                lexwarn(lex, WARN_UNKNOWN_CONTROL_SEQUENCE, "unrecognized control sequence: \\%c", ch);
                /* so we just add the character plus backslash no matter what it actually is */
                lex_tokench(lex, '\\');
            }
            /* add the character finally */
            lex_tokench(lex, ch | texttype);
        }
        else
            lex_tokench(lex, ch);
    }
    lexerror(lex, "unexpected end of file within string constant");
    lex_ungetch(lex, Token::END); /* next token to be Token::END */
    return (lex->tok.ttype = Token::ERROR);
}

static Token GMQCC_WARN lex_finish_digit(lex_file *lex, Token lastch)
{
    bool ishex = false;

    Token ch = lastch;

    /* parse a number... */
    if (ch == Token::DOT)
        lex->tok.ttype = Token::FLOATCONST;
    else
        lex->tok.ttype = Token::INTCONST;

    lex_tokench(lex, ch);

    ch = lex_getch(lex);
    if (ch != Token::DOT && !util_isdigit(ch))
    {
        if (lastch != '0' || ch != 'x')
        {
            /* end of the number or EOF */
            lex_ungetch(lex, ch);
            lex_endtoken(lex);

            lex->tok.constval.i = lastch - '0';
            return lex->tok.ttype;
        }

        ishex = true;
    }

    /* EOF would have been caught above */

    if (ch != Token::DOT)
    {
        lex_tokench(lex, ch);
        ch = lex_getch(lex);
        while (util_isdigit(ch) || (ishex && isxdigit_only(ch)))
        {
            lex_tokench(lex, ch);
            ch = lex_getch(lex);
        }
    }
    /* NOT else, '.' can come from above as well */
    if (lex->tok.ttype != Token::FLOATCONST && ch == Token::DOT && !ishex)
    {
        /* Allow floating comma in non-hex mode */
        lex->tok.ttype = Token::FLOATCONST;
        lex_tokench(lex, ch);

        /* continue digits-only */
        ch = lex_getch(lex);
        while (util_isdigit(ch))
        {
            lex_tokench(lex, ch);
            ch = lex_getch(lex);
        }
    }
    /* put back the last character */
    /* but do not put back the trailing 'f' or a float */
    if (lex->tok.ttype == Token::FLOATCONST && ch == 'f')
        ch = lex_getch(lex);

    /* generally we don't want words to follow numbers: */
    if (isident(ch)) {
        lexerror(lex, "unexpected trailing characters after number");
        return (lex->tok.ttype = Token::ERROR);
    }
    lex_ungetch(lex, ch);

    lex_endtoken(lex);
    if (lex->tok.ttype == Token::FLOATCONST)
        lex->tok.constval.f = strtod(lex->tok.value.c_str(), nullptr);
    else
        lex->tok.constval.i = strtol(lex->tok.value.c_str(), nullptr, 0);
    return lex->tok.ttype;
}

Token lex_do(lex_file *lex)
{
    Token ch, nextch, thirdch;
    bool hadwhite = false;

    lex_token_new(lex);

    while (true) {
        ch = lex_skipwhite(lex, hadwhite);
        hadwhite = true;
        if (!lex->flags.mergelines || ch != Token::BACKSLASH)
            break;
        ch = lex_getch(lex);
        if (ch == Token::CR)
            ch = lex_getch(lex);
        if (ch != Token::LF) {
            lex_ungetch(lex, ch);
            ch = Token::BACKSLASH;
            break;
        }
        /* we reached a linemerge */
        lex_tokench(lex, '\n');
    }

    if (lex->flags.preprocessing && (ch == Token::WHITE || ch == Token::EOL || ch == Token::FATAL)) {
        return (lex->tok.ttype = ch);
    }

    lex->sline = lex->line;
    lex->tok.ctx.line = lex->sline;
    lex->tok.ctx.file = lex->name;

    if (lex->eof)
        return (lex->tok.ttype = Token::FATAL);

    if (ch == Token::END) {
        lex->eof = true;
        return (lex->tok.ttype = Token::END);
    }

    /* modelgen / spiritgen commands */
    if (ch == Token::DOLLAR && !lex->flags.preprocessing) {
        const char *v;
        size_t frame;

        ch = lex_getch(lex);
        if (!isident_start(ch)) {
            lexerror(lex, "hanging '$' modelgen/spritegen command line");
            return lex_do(lex);
        }
        lex_tokench(lex, ch);
        if (!lex_finish_ident(lex))
            return (lex->tok.ttype = Token::ERROR);
        lex_endtoken(lex);
        /* skip the known commands */
        v = lex->tok.value.c_str();

        if (!strcmp(v, "frame") || !strcmp(v, "framesave"))
        {
            /* frame/framesave command works like an enum
             * similar to fteqcc we handle this in the lexer.
             * The reason for this is that it is sensitive to newlines,
             * which the parser is unaware of
             */
            if (!lex_finish_frames(lex))
                 return (lex->tok.ttype = Token::ERROR);
            return lex_do(lex);
        }

        if (!strcmp(v, "framevalue"))
        {
            ch = lex_getch(lex);
            while (ch != Token::END && util_isspace(ch) && ch != Token::LF)
                ch = lex_getch(lex);

            if (!util_isdigit(ch)) {
                lexerror(lex, "$framevalue requires an integer parameter");
                return lex_do(lex);
            }

            lex_token_new(lex);
            lex->tok.ttype = lex_finish_digit(lex, ch);
            lex_endtoken(lex);
            if (lex->tok.ttype != Token::INTCONST) {
                lexerror(lex, "$framevalue requires an integer parameter");
                return lex_do(lex);
            }
            lex->framevalue = lex->tok.constval.i;
            return lex_do(lex);
        }

        if (!strcmp(v, "framerestore"))
        {
            int rc;

            lex_token_new(lex);

            rc = lex_parse_frame(lex);

            if (rc > 0) {
                lexerror(lex, "$framerestore requires a framename parameter");
                return lex_do(lex);
            }
            if (rc < 0)
                return (lex->tok.ttype = Token::FATAL);

            v = lex->tok.value.c_str();
            for (frame = 0; frame < vec_size(lex->frames); ++frame) {
                if (lex->frames[frame].name == v) {
                    lex->framevalue = lex->frames[frame].value;
                    return lex_do(lex);
                }
            }
            lexerror(lex, "unknown framename `%s`", v);
            return lex_do(lex);
        }

        if (!strcmp(v, "modelname"))
        {
            int rc;

            lex_token_new(lex);

            rc = lex_parse_frame(lex);

            if (rc > 0) {
                lexerror(lex, "$modelname requires a parameter");
                return lex_do(lex);
            }
            if (rc < 0)
                return (lex->tok.ttype = Token::FATAL);

            if (lex->modelname.size()) {
                frame_macro m;
                m.name = std::move(lex->modelname);
                m.value = lex->framevalue;
                vec_push(lex->frames, m);
            }
            lex->modelname = std::string(lex->tok.value.c_str());
            return lex_do(lex);
        }

        if (!strcmp(v, "flush"))
        {
            vec_free(lex->frames);
            /* skip line (fteqcc does it too) */
            ch = lex_getch(lex);
            while (ch != Token::END && ch != Token::LF)
                ch = lex_getch(lex);
            return lex_do(lex);
        }

        if (!strcmp(v, "cd") ||
            !strcmp(v, "origin") ||
            !strcmp(v, "base") ||
            !strcmp(v, "flags") ||
            !strcmp(v, "scale") ||
            !strcmp(v, "skin"))
        {
            /* skip line */
            ch = lex_getch(lex);
            while (ch != Token::END && ch != Token::LF)
                ch = lex_getch(lex);
            return lex_do(lex);
        }

        for (frame = 0; frame < vec_size(lex->frames); ++frame) {
            if (lex->frames[frame].name == v) {
                lex->tok.constval.i = lex->frames[frame].value;
                return (lex->tok.ttype = Token::INTCONST);
            }
        }

        lexerror(lex, "invalid frame macro");
        return lex_do(lex);
    }

    /* single-character tokens */
    switch (ch)
    {
        case Token::BRACKET_OPEN:
            nextch = lex_getch(lex);
            if (nextch == Token::BRACKET_OPEN) {
                lex_tokench(lex, ch);
                lex_tokench(lex, nextch);
                lex_endtoken(lex);
                return (lex->tok.ttype = Token::ATTRIBUTE_OPEN);
            }
            lex_ungetch(lex, nextch);
            /* FALL THROUGH */
        case Token::PAREN_OPEN:
        case Token::COLON:
        case Token::QUESTION:
            lex_tokench(lex, ch);
            lex_endtoken(lex);
            if (lex->flags.noops)
                return (lex->tok.ttype = ch);
            else
                return (lex->tok.ttype = Token::OPERATOR);

        case Token::BRACKET_CLOSE:
            if (lex->flags.noops) {
                nextch = lex_getch(lex);
                if (nextch == Token::BRACKET_CLOSE) {
                    lex_tokench(lex, ch);
                    lex_tokench(lex, nextch);
                    lex_endtoken(lex);
                    return (lex->tok.ttype = Token::ATTRIBUTE_CLOSE);
                }
                lex_ungetch(lex, nextch);
            }
            /* FALL THROUGH */
        case Token::PAREN_CLOSE:
        case Token::SEMICOLON:
        case Token::BRACE_OPEN:
        case Token::BRACE_CLOSE:

        case Token::HASH:
            lex_tokench(lex, ch);
            lex_endtoken(lex);
            return (lex->tok.ttype = ch);
        default:
            break;
    }

    if (ch == Token::DOT) {
        nextch = lex_getch(lex);
        /* digits starting with a dot */
        if (util_isdigit(nextch)) {
            lex_ungetch(lex, nextch);
            lex->tok.ttype = lex_finish_digit(lex, ch);
            lex_endtoken(lex);
            return lex->tok.ttype;
        }
        lex_ungetch(lex, nextch);
    }

    if (lex->flags.noops)
    {
        /* Detect characters early which are normally
         * operators OR PART of an operator.
         */
        switch (ch)
        {
            case Token::MUL:
            case Token::DIV:
            case Token::MOD:
            case Token::ADD:
            case Token::SUB:
            case Token::LT:
            case Token::GT:
            case Token::EQ:
            case Token::AND:
            case Token::OR:
            case Token::XOR:
            case Token::BITNOT:
            case Token::COMMA:
            case Token::NOT:
                lex_tokench(lex, ch);
                lex_endtoken(lex);
                return (lex->tok.ttype = ch);
            default:
                break;
        }
    }

    if (ch == Token::DOT)
    {
        lex_tokench(lex, ch);
        // peak ahead once
        nextch = lex_getch(lex);
        if (nextch != Token::DOT) {
            lex_ungetch(lex, nextch);
            lex_endtoken(lex);
            if (lex->flags.noops)
                return (lex->tok.ttype = ch);
            else
                return (lex->tok.ttype = Token::OPERATOR);
        }
        // peak ahead again
        nextch = lex_getch(lex);
        if (nextch != Token::DOT) {
            lex_ungetch(lex, nextch);
            lex_ungetch(lex, Token::DOT);
            lex_endtoken(lex);
            if (lex->flags.noops)
                return (lex->tok.ttype = ch);
            else
                return (lex->tok.ttype = Token::OPERATOR);
        }
        // fill the token to be "..."
        lex_tokench(lex, ch);
        lex_tokench(lex, ch);
        lex_endtoken(lex);
        return (lex->tok.ttype = Token::DOTS);
    }

    if (ch == Token::COMMA || ch == Token::DOT) {
        lex_tokench(lex, ch);
        lex_endtoken(lex);
        return (lex->tok.ttype = Token::OPERATOR);
    }

    if (ch == Token::ADD || ch == Token::SUB || /* ++, --, +=, -= */
        ch == Token::GT || ch == Token::LT|| /* <<, >>, <=, >=  and >< as well! */
        ch == Token::EQ || ch == Token::NOT || /* <=>, ==, !=                     */
        ch == Token::AND || ch == Token::OR || /* &&, ||, &=, |=                  */
        ch == Token::BITNOT || ch == Token::XOR   /* ~=, ~, ^                        */
    )  {
        lex_tokench(lex, ch);
        nextch = lex_getch(lex);

        if ((nextch == Token::EQ && ch != Token::LT) || (nextch == Token::LT && ch == Token::GT))
            lex_tokench(lex, nextch);
        else if (nextch == ch && ch != Token::NOT) {
            lex_tokench(lex, nextch);
            if ((thirdch = lex_getch(lex)) == Token::EQ)
                lex_tokench(lex, thirdch);
            else
                lex_ungetch(lex, thirdch);
        } else if (ch == Token::LT && nextch == Token::EQ) {
            lex_tokench(lex, nextch);
            if ((thirdch = lex_getch(lex)) == Token::GT)
                lex_tokench(lex, thirdch);
            else
                lex_ungetch(lex, thirdch);
        } else if (ch == Token::AND && nextch == Token::BITNOT) {
            thirdch = lex_getch(lex);
            if (thirdch != Token::EQ) {
                lex_ungetch(lex, thirdch);
                lex_ungetch(lex, nextch);
            }
            else {
                lex_tokench(lex, nextch);
                lex_tokench(lex, thirdch);
            }
        }
        else if (lex->flags.preprocessing &&
                 ch == Token::SUB && util_isdigit(nextch))
        {
            lex->tok.ttype = lex_finish_digit(lex, nextch);
            if (lex->tok.ttype == Token::INTCONST)
                lex->tok.constval.i = -lex->tok.constval.i;
            else
                lex->tok.constval.f = -lex->tok.constval.f;
            lex_endtoken(lex);
            return lex->tok.ttype;
        } else {
            lex_ungetch(lex, nextch);
        }

        lex_endtoken(lex);
        return (lex->tok.ttype = Token::OPERATOR);
    }

    if (ch == Token::MUL || ch == Token::DIV) /* *=, /= */
    {
        lex_tokench(lex, ch);

        nextch = lex_getch(lex);
        if (nextch == Token::EQ || nextch == Token::MUL) {
            lex_tokench(lex, nextch);
        } else
            lex_ungetch(lex, nextch);

        lex_endtoken(lex);
        return (lex->tok.ttype = Token::OPERATOR);
    }

    if (ch == Token::MOD) {
        lex_tokench(lex, ch);
        lex_endtoken(lex);
        return (lex->tok.ttype = Token::OPERATOR);
    }

    if (isident_start(ch))
    {
        const char *v;

        lex_tokench(lex, ch);
        if (!lex_finish_ident(lex)) {
            /* error? */
            return (lex->tok.ttype = Token::ERROR);
        }
        lex_endtoken(lex);
        lex->tok.ttype = Token::IDENT;

        v = lex->tok.value.c_str();
        if (!strcmp(v, "void")) {
            lex->tok.ttype = Token::TYPENAME;
            lex->tok.constval.t = TYPE_VOID;
        } else if (!strcmp(v, "int")) {
            lex->tok.ttype = Token::TYPENAME;
            lex->tok.constval.t = TYPE_INTEGER;
        } else if (!strcmp(v, "float")) {
            lex->tok.ttype = Token::TYPENAME;
            lex->tok.constval.t = TYPE_FLOAT;
        } else if (!strcmp(v, "string")) {
            lex->tok.ttype = Token::TYPENAME;
            lex->tok.constval.t = TYPE_STRING;
        } else if (!strcmp(v, "entity")) {
            lex->tok.ttype = Token::TYPENAME;
            lex->tok.constval.t = TYPE_ENTITY;
        } else if (!strcmp(v, "vector")) {
            lex->tok.ttype = Token::TYPENAME;
            lex->tok.constval.t = TYPE_VECTOR;
        } else if (!strcmp(v, "_length")) {
            lex->tok.ttype = Token::OPERATOR;
        } else {
            size_t kw;
            for (kw = 0; kw < GMQCC_ARRAY_COUNT(keywords_qc); ++kw) {
                if (!strcmp(v, keywords_qc[kw]))
                    return (lex->tok.ttype = Token::KEYWORD);
            }
            if (OPTS_OPTION_U32(OPTION_STANDARD) != COMPILER_QCC) {
                for (kw = 0; kw < GMQCC_ARRAY_COUNT(keywords_fg); ++kw) {
                    if (!strcmp(v, keywords_fg[kw]))
                        return (lex->tok.ttype = Token::KEYWORD);
                }
            }
        }

        return lex->tok.ttype;
    }

    if (ch == Token::QUOT_DOUBLE)
    {
        lex->flags.nodigraphs = true;
        if (lex->flags.preprocessing)
            lex_tokench(lex, ch);
        lex->tok.ttype = lex_finish_string(lex, Token::QUOT_DOUBLE);
        if (lex->flags.preprocessing)
            lex_tokench(lex, ch);
        while (!lex->flags.preprocessing && lex->tok.ttype == Token::STRINGCONST)
        {
            /* Allow c style "string" "continuation" */
            ch = lex_skipwhite(lex, false);
            if (ch != Token::QUOT_DOUBLE) {
                lex_ungetch(lex, ch);
                break;
            }

            lex->tok.ttype = lex_finish_string(lex, Token::QUOT_DOUBLE);
        }
        lex->flags.nodigraphs = false;
        lex_endtoken(lex);
        return lex->tok.ttype;
    }

    if (ch == Token::QUOT_SINGLE)
    {
        /* we parse character constants like string,
         * but return Token::CHARCONST, or a vector type if it fits...
         * Likewise actual unescaping has to be done by the parser.
         * The difference is we don't allow 'char' 'continuation'.
         */
        if (lex->flags.preprocessing)
            lex_tokench(lex, ch);
        lex->tok.ttype = lex_finish_string(lex, Token::QUOT_SINGLE);
        if (lex->flags.preprocessing)
            lex_tokench(lex, ch);
        lex_endtoken(lex);

        lex->tok.ttype = Token::CHARCONST;

        /* It's a vector if we can successfully scan 3 floats */
        if (util_sscanf(lex->tok.value.c_str(), " %f %f %f ",
                   &lex->tok.constval.v.x, &lex->tok.constval.v.y, &lex->tok.constval.v.z) == 3)

        {
             lex->tok.ttype = Token::VECTORCONST;
        }
        else
        {
            if (!lex->flags.preprocessing && strlen(lex->tok.value.c_str()) > 1) {
                utf8ch_t u8char;
                /* check for a valid utf8 character */
                if (!OPTS_FLAG(UTF8) || !utf8_to(&u8char, (const unsigned char *)lex->tok.value.c_str(), 8)) {
                    if (lexwarn(lex, WARN_MULTIBYTE_CHARACTER,
                                ( OPTS_FLAG(UTF8) ? "invalid multibyte character sequence `%s`"
                                                  : "multibyte character: `%s`" ),
                                lex->tok.value))
                        return (lex->tok.ttype = Token::ERROR);
                }
                else
                    lex->tok.constval.i = u8char;
            }
            else
                lex->tok.constval.i = lex->tok.value.c_str()[0];
        }

        return lex->tok.ttype;
    }

    if (util_isdigit(ch))
    {
        lex->tok.ttype = lex_finish_digit(lex, ch);
        lex_endtoken(lex);
        return lex->tok.ttype;
    }

    if (lex->flags.preprocessing) {
        lex_tokench(lex, static_cast<int>(ch));
        lex_endtoken(lex);
        return (lex->tok.ttype = ch);
    }

    lexerror(lex, "unknown token: `%c`", ch);
    return (lex->tok.ttype = Token::ERROR);
}
