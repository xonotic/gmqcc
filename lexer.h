#ifndef GMQCC_LEXER_HDR
#define GMQCC_LEXER_HDR

#include "gmqcc.h"

#define TOKENS(X) \
    /* Other tokens which we can return: */ \
    X(NONE, =0) \
    X(CR, = '\r') \
    X(LF, = '\n') \
    X(WS, = ' ') \
    X(BACKSLASH, = '\\') \
    X(HASH, = '#') \
    X(DOLLAR, = '$') \
    X(DOT, = '.') \
    X(COMMA, = ',') \
    X(COLON, = ':') \
    X(SEMICOLON, = ';') \
    X(AND, = '&') \
    X(OR, = '|') \
    X(XOR, = '^') \
    X(BITNOT, = '~') \
    X(NOT, = '!') \
    X(LT, = '<') \
    X(GT, = '>') \
    X(EQ, = '=') \
    X(MUL, = '*') \
    X(DIV, = '/') \
    X(MOD, = '%') \
    X(ADD, = '+') \
    X(SUB, = '-') \
    X(QUOT_SINGLE, = '\'') \
    X(QUOT_DOUBLE, = '"') \
    X(QUESTION, = '?') \
    X(BRACE_OPEN, = '{') \
    X(BRACE_CLOSE, = '}') \
    X(BRACKET_OPEN, = '[') \
    X(BRACKET_CLOSE, = ']') \
    X(PAREN_OPEN, = '(') \
    X(PAREN_CLOSE, = ')') \
    X(START, = 128) \
    X(IDENT, ) \
    X(TYPENAME, ) \
    X(OPERATOR, ) \
    /* loop */ \
    X(KEYWORD, ) \
    /* 3 dots, ... */ \
    X(DOTS, ) \
    /* [[ */ \
    X(ATTRIBUTE_OPEN, ) \
    /* ]] */ \
    X(ATTRIBUTE_CLOSE, ) \
    /* for the ftepp only */ \
    X(VA_ARGS, ) \
    /* for the ftepp only */ \
    X(VA_ARGS_ARRAY, ) \
    /* to get the count of vaargs */ \
    X(VA_COUNT, ) \
    /* not the typename but an actual "string" */ \
    X(STRINGCONST, ) \
    X(CHARCONST, ) \
    X(VECTORCONST, ) \
    X(INTCONST, ) \
    X(FLOATCONST, ) \
    X(WHITE, ) \
    X(EOL, ) \
    /* if we add additional tokens before this, the exposed API \
     * should not be broken anyway, but EOF/ERROR/... should \
     * still be at the bottom \
     */ \
    X(END, = 1024) \
    /* We use '< ERROR', so FATAL must come after it and any \
     * other error related tokens as well \
     */ \
    X(ERROR, ) \
    /* internal error, eg out of memory */ \
    X(FATAL, ) \
    /**/

enum Token : int { // todo: enum class
#define X(id, val) id val,
    TOKENS(X)
#undef X
};

inline const char *TokenName(Token t) {
    switch (t) {
        default:
            return "UNKNOWN";
#define X(id, val) case Token::id: return #id;
        TOKENS(X)
#undef X
    }
}

struct cvec {
    std::string value;

    explicit cvec() = default;

    char *mut() {
        return &value[0];
    }

    const char *c_str() {
        return value.c_str();
    }

    void shrinkto(size_t i) {
        value.resize(i);
    }

    void shrinkby(size_t i) {
        value.resize(value.size() - i);
    }

    void push(char it) {
        value.push_back(it);
    }
};

struct token {
    Token ttype;
    cvec value;
    union {
        vec3_t v;
        int i;
        qcfloat_t f;
        qc_type t; /* type */
    } constval;
    lex_ctx_t ctx;
};

struct frame_macro {
    std::string name;
    int value;
};

struct lex_file {
    FILE *file;
    const char *open_string;
    size_t open_string_length;
    size_t open_string_pos;

    char *name;
    size_t line;
    size_t sline; /* line at the start of a token */
    size_t column;

    std::array<Token, 256> peek;
    size_t peekpos;

    bool eof;

    token tok; /* not a pointer anymore */

    struct {
        bool noops:1;
        bool nodigraphs:1; /* used when lexing string constants */
        bool preprocessing:1; /* whitespace and EOLs become actual tokens */
        bool mergelines:1; /* backslash at the end of a line escapes the newline */
    } flags; /* sizeof == 1 */

    int framevalue;
    frame_macro *frames;
    std::string modelname;

    size_t push_line;
};

lex_file *lex_open(const char *file);

lex_file *lex_open_string(const char *str, size_t len, const char *name);

void lex_close(lex_file *lex);

Token lex_do(lex_file *lex);

void lex_cleanup();

/* Parser
 *
 */

enum {
    ASSOC_LEFT,
    ASSOC_RIGHT
};

#define OP_SUFFIX 1
#define OP_PREFIX 2

struct oper_info {
    const char *op;
    unsigned int operands;
    unsigned int id;
    unsigned int assoc;
    signed int prec;
    unsigned int flags;
    bool folds;
};

/*
 * Explicit uint8_t casts since the left operand of shift operator cannot
 * be negative, even though it won't happen, this supresses the future
 * possibility.
 */
#define opid1(a)     ((uint8_t)a)
#define opid2(a, b)   (((uint8_t)a<<8) |(uint8_t)b)
#define opid3(a, b, c) (((uint8_t)a<<16)|((uint8_t)b<<8)|(uint8_t)c)

static const oper_info c_operators[] = {
        {"(",       0, opid1('('),           ASSOC_LEFT,  99, OP_PREFIX, false}, /* paren expression - non function call */
        {"_length", 1, opid3('l', 'e', 'n'), ASSOC_RIGHT, 98, OP_PREFIX, true},

        {"++",      1, opid3('S', '+', '+'), ASSOC_LEFT,  17, OP_SUFFIX, false},
        {"--",      1, opid3('S', '-', '-'), ASSOC_LEFT,  17, OP_SUFFIX, false},
        {".",       2, opid1('.'),           ASSOC_LEFT,  17, 0,         false},
        {"(",       0, opid1('('),           ASSOC_LEFT,  17, 0,         false}, /* function call */
        {"[",       2, opid1('['),           ASSOC_LEFT,  17, 0,         false}, /* array subscript */

        {"++",      1, opid3('+', '+', 'P'), ASSOC_RIGHT, 16, OP_PREFIX, false},
        {"--",      1, opid3('-', '-', 'P'), ASSOC_RIGHT, 16, OP_PREFIX, false},

        {"**",      2, opid2('*', '*'),      ASSOC_RIGHT, 14, 0,         true},
        {"!",       1, opid2('!', 'P'),      ASSOC_RIGHT, 14, OP_PREFIX, true},
        {"~",       1, opid2('~', 'P'),      ASSOC_RIGHT, 14, OP_PREFIX, true},
        {"+",       1, opid2('+', 'P'),      ASSOC_RIGHT, 14, OP_PREFIX, false},
        {"-",       1, opid2('-', 'P'),      ASSOC_RIGHT, 14, OP_PREFIX, true},
/*  { "&",      1, opid2('&','P'),     ASSOC_RIGHT, 14, OP_PREFIX, false}, */

        {"*",       2, opid1('*'),           ASSOC_LEFT,  13, 0,         true},
        {"/",       2, opid1('/'),           ASSOC_LEFT,  13, 0,         true},
        {"%",       2, opid1('%'),           ASSOC_LEFT,  13, 0,         true},
        {"><",      2, opid2('>', '<'),      ASSOC_LEFT,  13, 0,         true},

        {"+",       2, opid1('+'),           ASSOC_LEFT,  12, 0,         true},
        {"-",       2, opid1('-'),           ASSOC_LEFT,  12, 0,         true},

        {"<<",      2, opid2('<', '<'),      ASSOC_LEFT,  11, 0,         true},
        {">>",      2, opid2('>', '>'),      ASSOC_LEFT,  11, 0,         true},

        {"<",       2, opid1('<'),           ASSOC_LEFT,  10, 0,         false},
        {">",       2, opid1('>'),           ASSOC_LEFT,  10, 0,         false},
        {"<=>",     2, opid3('<', '=', '>'), ASSOC_LEFT,  10, 0,         true},
        {"<=",      2, opid2('<', '='),      ASSOC_LEFT,  10, 0,         false},
        {">=",      2, opid2('>', '='),      ASSOC_LEFT,  10, 0,         false},

        {"==",      2, opid2('=', '='),      ASSOC_LEFT,  9,  0,         true},
        {"!=",      2, opid2('!', '='),      ASSOC_LEFT,  9,  0,         true},

        {"&",       2, opid1('&'),           ASSOC_LEFT,  8,  0,         true},

        {"^",       2, opid1('^'),           ASSOC_LEFT,  7,  0,         true},

        {"|",       2, opid1('|'),           ASSOC_LEFT,  6,  0,         true},

        {"&&",      2, opid2('&', '&'),      ASSOC_LEFT,  5,  0,         true},

        {"||",      2, opid2('|', '|'),      ASSOC_LEFT,  4,  0,         true},

        {"?",       3, opid2('?', ':'),      ASSOC_RIGHT, 3,  0,         true},

        {"=",       2, opid1('='),           ASSOC_RIGHT, 2,  0,         false},
        {"+=",      2, opid2('+', '='),      ASSOC_RIGHT, 2,  0,         false},
        {"-=",      2, opid2('-', '='),      ASSOC_RIGHT, 2,  0,         false},
        {"*=",      2, opid2('*', '='),      ASSOC_RIGHT, 2,  0,         false},
        {"/=",      2, opid2('/', '='),      ASSOC_RIGHT, 2,  0,         false},
        {"%=",      2, opid2('%', '='),      ASSOC_RIGHT, 2,  0,         false},
        {">>=",     2, opid3('>', '>', '='), ASSOC_RIGHT, 2,  0,         false},
        {"<<=",     2, opid3('<', '<', '='), ASSOC_RIGHT, 2,  0,         false},
        {"&=",      2, opid2('&', '='),      ASSOC_RIGHT, 2,  0,         false},
        {"^=",      2, opid2('^', '='),      ASSOC_RIGHT, 2,  0,         false},
        {"|=",      2, opid2('|', '='),      ASSOC_RIGHT, 2,  0,         false},

        {":",       0, opid2(':', '?'),      ASSOC_RIGHT, 1,  0,         false},

        {",",       2, opid1(','),           ASSOC_LEFT,  0,  0,         false}
};

static const oper_info fte_operators[] = {
        {"(",   0, opid1('('),           ASSOC_LEFT,  99, OP_PREFIX, false}, /* paren expression - non function call */

        {"++",  1, opid3('S', '+', '+'), ASSOC_LEFT,  15, OP_SUFFIX, false},
        {"--",  1, opid3('S', '-', '-'), ASSOC_LEFT,  15, OP_SUFFIX, false},
        {".",   2, opid1('.'),           ASSOC_LEFT,  15, 0,         false},
        {"(",   0, opid1('('),           ASSOC_LEFT,  15, 0,         false}, /* function call */
        {"[",   2, opid1('['),           ASSOC_LEFT,  15, 0,         false}, /* array subscript */

        {"!",   1, opid2('!', 'P'),      ASSOC_RIGHT, 14, OP_PREFIX, true},
        {"+",   1, opid2('+', 'P'),      ASSOC_RIGHT, 14, OP_PREFIX, false},
        {"-",   1, opid2('-', 'P'),      ASSOC_RIGHT, 14, OP_PREFIX, true},
        {"++",  1, opid3('+', '+', 'P'), ASSOC_RIGHT, 14, OP_PREFIX, false},
        {"--",  1, opid3('-', '-', 'P'), ASSOC_RIGHT, 14, OP_PREFIX, false},

        {"*",   2, opid1('*'),           ASSOC_LEFT,  13, 0,         true},
        {"/",   2, opid1('/'),           ASSOC_LEFT,  13, 0,         true},
        {"&",   2, opid1('&'),           ASSOC_LEFT,  13, 0,         true},
        {"|",   2, opid1('|'),           ASSOC_LEFT,  13, 0,         true},

        {"+",   2, opid1('+'),           ASSOC_LEFT,  12, 0,         true},
        {"-",   2, opid1('-'),           ASSOC_LEFT,  12, 0,         true},

        {"<<",  2, opid2('<', '<'),      ASSOC_LEFT,  11, 0,         true},
        {">>",  2, opid2('>', '>'),      ASSOC_LEFT,  11, 0,         true},

        {"<",   2, opid1('<'),           ASSOC_LEFT,  10, 0,         false},
        {">",   2, opid1('>'),           ASSOC_LEFT,  10, 0,         false},
        {"<=",  2, opid2('<', '='),      ASSOC_LEFT,  10, 0,         false},
        {">=",  2, opid2('>', '='),      ASSOC_LEFT,  10, 0,         false},
        {"==",  2, opid2('=', '='),      ASSOC_LEFT,  10, 0,         true},
        {"!=",  2, opid2('!', '='),      ASSOC_LEFT,  10, 0,         true},

        {"?",   3, opid2('?', ':'),      ASSOC_RIGHT, 9,  0,         true},

        {"=",   2, opid1('='),           ASSOC_RIGHT, 8,  0,         false},
        {"+=",  2, opid2('+', '='),      ASSOC_RIGHT, 8,  0,         false},
        {"-=",  2, opid2('-', '='),      ASSOC_RIGHT, 8,  0,         false},
        {"*=",  2, opid2('*', '='),      ASSOC_RIGHT, 8,  0,         false},
        {"/=",  2, opid2('/', '='),      ASSOC_RIGHT, 8,  0,         false},
        {"%=",  2, opid2('%', '='),      ASSOC_RIGHT, 8,  0,         false},
        {"&=",  2, opid2('&', '='),      ASSOC_RIGHT, 8,  0,         false},
        {"|=",  2, opid2('|', '='),      ASSOC_RIGHT, 8,  0,         false},
        {"&~=", 2, opid3('&', '~', '='), ASSOC_RIGHT, 8,  0,         false},

        {"&&",  2, opid2('&', '&'),      ASSOC_LEFT,  5,  0,         true},
        {"||",  2, opid2('|', '|'),      ASSOC_LEFT,  5,  0,         true},

        /* Leave precedence 3 for : with -fcorrect-ternary */
        {",",   2, opid1(','),           ASSOC_LEFT,  2,  0,         false},
        {":",   0, opid2(':', '?'),      ASSOC_RIGHT, 1,  0,         false}
};

static const oper_info qcc_operators[] = {
        {"(",  0, opid1('('),      ASSOC_LEFT,  99, OP_PREFIX, false}, /* paren expression - non function call */

        {".",  2, opid1('.'),      ASSOC_LEFT,  15, 0,         false},
        {"(",  0, opid1('('),      ASSOC_LEFT,  15, 0,         false}, /* function call */
        {"[",  2, opid1('['),      ASSOC_LEFT,  15, 0,         false}, /* array subscript */

        {"!",  1, opid2('!', 'P'), ASSOC_RIGHT, 14, OP_PREFIX, true},
        {"+",  1, opid2('+', 'P'), ASSOC_RIGHT, 14, OP_PREFIX, false},
        {"-",  1, opid2('-', 'P'), ASSOC_RIGHT, 14, OP_PREFIX, true},

        {"*",  2, opid1('*'),      ASSOC_LEFT,  13, 0,         true},
        {"/",  2, opid1('/'),      ASSOC_LEFT,  13, 0,         true},
        {"&",  2, opid1('&'),      ASSOC_LEFT,  13, 0,         true},
        {"|",  2, opid1('|'),      ASSOC_LEFT,  13, 0,         true},

        {"+",  2, opid1('+'),      ASSOC_LEFT,  12, 0,         true},
        {"-",  2, opid1('-'),      ASSOC_LEFT,  12, 0,         true},

        {"<",  2, opid1('<'),      ASSOC_LEFT,  10, 0,         false},
        {">",  2, opid1('>'),      ASSOC_LEFT,  10, 0,         false},
        {"<=", 2, opid2('<', '='), ASSOC_LEFT,  10, 0,         false},
        {">=", 2, opid2('>', '='), ASSOC_LEFT,  10, 0,         false},
        {"==", 2, opid2('=', '='), ASSOC_LEFT,  10, 0,         true},
        {"!=", 2, opid2('!', '='), ASSOC_LEFT,  10, 0,         true},

        {"=",  2, opid1('='),      ASSOC_RIGHT, 8,  0,         false},
        {"+=", 2, opid2('+', '='), ASSOC_RIGHT, 8,  0,         false},
        {"-=", 2, opid2('-', '='), ASSOC_RIGHT, 8,  0,         false},
        {"*=", 2, opid2('*', '='), ASSOC_RIGHT, 8,  0,         false},
        {"/=", 2, opid2('/', '='), ASSOC_RIGHT, 8,  0,         false},
        {"%=", 2, opid2('%', '='), ASSOC_RIGHT, 8,  0,         false},
        {"&=", 2, opid2('&', '='), ASSOC_RIGHT, 8,  0,         false},
        {"|=", 2, opid2('|', '='), ASSOC_RIGHT, 8,  0,         false},

        {"&&", 2, opid2('&', '&'), ASSOC_LEFT,  5,  0,         true},
        {"||", 2, opid2('|', '|'), ASSOC_LEFT,  5,  0,         true},

        {",",  2, opid1(','),      ASSOC_LEFT,  2,  0,         false},
};
extern const oper_info *operators;
extern size_t operator_count;

#endif
