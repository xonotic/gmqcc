/*
 * Copyright (C) 2012, 2013
 *     Dale Weiler
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#include "gmqcc.h"
#include "lexer.h"

typedef struct {
    const char *line;    /* contents of the current line */
    size_t     *tokens;  /* stream of tokens             */
    char      **values;  /* stream of values for tokens  */
} diagnostic_data_t;


/* map<const char *, vector<diagnostic_data_t>> */
static ht diagnostic_table = NULL;

static void diagnostic_line(const char *file, size_t line, diagnostic_data_t ***read, size_t beg, size_t end) {
    diagnostic_data_t  **datas = NULL;
    size_t               feed  = 0;

    if (!diagnostic_table)
         diagnostic_table = util_htnew(1024);

    /*
     * Build the data one line at a time if it doesn't already exists.
     * We also lex one line at a time for consistency here.
     */
    if (!(datas = (diagnostic_data_t**)util_htget(diagnostic_table, file))) {
        lex_file          *lexer  = NULL;
        char              *next   = NULL;
        FILE              *handle = fs_file_open(file, "r");
        size_t             size   = 0;
        size_t             tok    = 0;
        

        /*
         * Now process all data line per line from the file, while inserting
         * the contents of each line into data.line, the token stream for
         * each line into data.tokens, and the values that are associated
         * with that given token into data.values. Then after one line is
         * complete, push the data associated with it into the datas vector
         * which will be stored alongside the hashtable.
         */
        while (fs_file_getline(&next, &size, handle) != EOF) {
            diagnostic_data_t *data    = mem_a(sizeof(diagnostic_data_t));
            
            data->tokens               = NULL;
            data->values               = NULL;
            data->line                 = util_strdup(next);
            lexer                      = lex_open_string(next, strlen(next), file);
            lexer->flags.preprocessing = true; /* enable whitespace */
            lexer->flags.mergelines    = true;
            
            /* build token stream */
            while ((tok = lex_do(lexer)) != TOKEN_EOF) {
                char *string = NULL;
                char *claim  = lexer->tok.value;
                
                for (; claim && *claim; claim ++)
                    vec_push(string, (*claim == '\t') ? ' ' : *claim);
                    
                vec_push(string, '\0');
                vec_push(data->tokens, tok);
                vec_push(data->values, string);
                
                /* prevent duplicated entries */
                memset(&lexer->tok, 0, sizeof(lexer->tok));
            }
            
            lex_close(lexer);
            vec_push(datas, data);
        }
        
        /*mem_d(data);*/
        util_htset(diagnostic_table, file, datas);
        fs_file_close(handle);
    }

    /* store the lines request back to the vector */
    if (line - beg + end > vec_size(datas)) {
        beg = 1;
        end = 1;
    }

    for(feed = line - beg; feed < line - beg + end; ++feed)
        vec_push((*read), datas[feed]);
}

static void diagnostic_feed(const char *file, size_t line, size_t beg, size_t end, bool marker, size_t diagnostic) {
    diagnostic_data_t  **read  = NULL;
    size_t               feed  = 0;
    size_t               space = 6;
    size_t               len   = 0;
    size_t               itr   = 0;
    size_t               tok   = 0;

    /* get line */
    diagnostic_line(file, line, &read, beg, end);

    /* use old token stream to pretty the output */
    for (; feed < vec_size(read); feed++) {
        con_out("%4d: ", line);
        while ((tok = read[feed]->tokens[itr]) != TOKEN_EOL) {
            switch (tok) {
                case TOKEN_TYPENAME:
                case TOKEN_KEYWORD:
                    con_out("\033[1;33m%s\033[0m", read[feed]->values[itr]);
                    break;

                case TOKEN_INTCONST:
                case TOKEN_FLOATCONST:
                    con_out("\033[1;32m%s\033[0m", read[feed]->values[itr]);
                    break;

                case TOKEN_CHARCONST:
                case TOKEN_STRINGCONST:
                    con_out("\033[1;31m%s\033[0m", read[feed]->values[itr]);
                    break;

                case TOKEN_EOF:
                case TOKEN_ERROR:
                case TOKEN_EOL:
                    /* ignore */
                    break;
                    
                default:
                    con_out("%s", read[feed]->values[itr]);
                    break;
            };
            itr++;
        }
        itr = 0;
        con_out("\n");
    }
    
    switch (diagnostic) {
        case DIAGNOSTIC_EXPRESSION_CASE:
        case DIAGNOSTIC_SEMICOLON:
            for (; len < vec_size(vec_last(read)->values); len++)
                space += strlen(vec_last(read)->values[len]);
                
            len    = 1;
            space -= beg - end;
            break;
            
        default:
            break;
    }

    while (space --) con_out(" ");
    while (len   --) con_out("~");

    con_out((marker) ? "^\n" : "\n"); 

    vec_free(read);
}


static void diagnostic_destory_data(void *data) {
    diagnostic_data_t **datas = (diagnostic_data_t **)data;
    size_t              i,j;
    
    for (i = 0; i < vec_size(datas); i++) {
        vec_free(datas[i]->line);
        
        /*
         * There is always the same number of tokens as
         * values, one loop suffices.
         */
        for (j = 0; i < vec_size(datas[i]->tokens); i++) {
            mem_d(datas[i]->tokens[j]);
            mem_d(datas[i]->values[j]);
        }
        
        vec_free(datas[i]->tokens);
        vec_free(datas[i]->values);
        
        mem_d(datas[i]);
    }
}

void diagnostic_destroy() {
    if (!diagnostic_table)
        return;

    util_htrem(diagnostic_table, diagnostic_destory_data);
}

void diagnostic_calculate(const char *file, size_t line, size_t diagnostic) {
    size_t linebeg = 1;
    size_t linecnt = 1;
    bool   marker  = false;


    switch (diagnostic) {
        /*
         * Semicolon reports error on nextline, which is why we need
         * to increment the beginning line for diagnostics, and also
         * enable the marker (to show where it's missing).
         */
        case DIAGNOSTIC_SEMICOLON:
            linebeg++;
            marker = true;
            break;
            
        case DIAGNOSTIC_EXPRESSION_CASE:
            linebeg++;
            marker = true;
            break;

        /* Catches the DIAGNOSTIC_NULL and out of range case */
        default:
            return;
    }

    diagnostic_feed(file, line, linebeg, linecnt, marker, diagnostic);
}
