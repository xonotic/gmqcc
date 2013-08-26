/*
 * Copyright (C) 2012, 2013
 *     Wolfgang Bumiller
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
#ifndef GMQCC_IR_HDR
#define GMQCC_IR_HDR
#include "gmqcc.h"

typedef struct
{
    /* both inclusive */
    size_t start;
    size_t end;
} ir_life_entry_t;

struct ir_function_s;
typedef struct ir_value_s {
    char      *name;
    int       vtype;
    int       store;
    lex_ctx_t   context;
    /* even the IR knows the subtype of a field */
    int       fieldtype;
    /* and the output type of a function */
    int       outtype;
    /* 'const' vs 'var' qualifier */
    int       cvq;
    uint32_t  flags;

    struct ir_instr_s **reads;
    struct ir_instr_s **writes;

    /* constantvalues */
    bool hasvalue;
    union {
        float    vfloat;
        int      vint;
        vec3_t   vvec;
        int32_t  ivec[3];
        char    *vstring;
        struct ir_value_s *vpointer;
        struct ir_function_s *vfunc;
    } constval;

    struct {
        int32_t globaladdr;
        int32_t name;
        /* filled by the local-allocator */
        int32_t local;
        /* added for members */
        int32_t addroffset;
        /* to generate field-addresses early */
        int32_t fieldaddr;
    } code;

    /* for acessing vectors */
    struct ir_value_s *members[3];
    struct ir_value_s *memberof;

    /* arrays will never overlap with temps */
    bool unique_life;
    /* temps living during a CALL must be locked */
    bool      locked;
    bool      callparam;

    /* For the temp allocator */
    ir_life_entry_t *life;
} ir_value;

/* ir_value can be a variable, or created by an operation */
/* if a result of an operation: the function should store
 * it to remember to delete it / garbage collect it
 */
void            ir_value_delete(ir_value*);
ir_value*       ir_value_vector_member(ir_value*, unsigned int member);
bool GMQCC_WARN ir_value_set_float(ir_value*, float f);
bool GMQCC_WARN ir_value_set_func(ir_value*, int f);
bool GMQCC_WARN ir_value_set_string(ir_value*, const char *s);
bool GMQCC_WARN ir_value_set_vector(ir_value*, vec3_t v);
bool GMQCC_WARN ir_value_set_field(ir_value*, ir_value *fld);
bool            ir_value_lives(ir_value*, size_t);
void            ir_value_dump_life(const ir_value *self, int (*oprintf)(const char*,...));

/* PHI data */
typedef struct ir_phi_entry_s
{
    ir_value          *value;
    struct ir_block_s *from;
} ir_phi_entry_t;

/* instruction */
typedef struct ir_instr_s
{
    int       opcode;
    lex_ctx_t   context;
    ir_value* (_ops[3]);
    struct ir_block_s* (bops[2]);

    ir_phi_entry_t *phi;
    ir_value      **params;

    /* For the temp-allocation */
    size_t eid;

    /* For IFs */
    bool   likely;

    struct ir_block_s *owner;
} ir_instr;

/* block */
typedef struct ir_block_s
{
    char      *label;
    lex_ctx_t    context;
    bool       final; /* once a jump is added we're done */

    ir_instr          **instr;
    struct ir_block_s **entries;
    struct ir_block_s **exits;
    ir_value          **living;

    /* For the temp-allocation */
    size_t entry_id;
    size_t eid;
    bool   is_return;

    struct ir_function_s *owner;

    bool   generated;
    size_t code_start;
} ir_block;

ir_value*       ir_block_create_binop(ir_block*, lex_ctx_t, const char *label, int op, ir_value *left, ir_value *right);
ir_value*       ir_block_create_unary(ir_block*, lex_ctx_t, const char *label, int op, ir_value *operand);
bool GMQCC_WARN ir_block_create_store_op(ir_block*, lex_ctx_t, int op, ir_value *target, ir_value *what);
bool GMQCC_WARN ir_block_create_storep(ir_block*, lex_ctx_t, ir_value *target, ir_value *what);
ir_value*       ir_block_create_load_from_ent(ir_block*, lex_ctx_t, const char *label, ir_value *ent, ir_value *field, int outype);
ir_value*       ir_block_create_fieldaddress(ir_block*, lex_ctx_t, const char *label, ir_value *entity, ir_value *field);

/* This is to create an instruction of the form
 * <outtype>%label := opcode a, b
 */
ir_instr* ir_block_create_phi(ir_block*, lex_ctx_t, const char *label, int vtype);
ir_value* ir_phi_value(ir_instr*);
void ir_phi_add(ir_instr*, ir_block *b, ir_value *v);
ir_instr* ir_block_create_call(ir_block*, lex_ctx_t, const char *label, ir_value *func, bool noreturn);
ir_value* ir_call_value(ir_instr*);
void ir_call_param(ir_instr*, ir_value*);

bool GMQCC_WARN ir_block_create_return(ir_block*, lex_ctx_t, ir_value *opt_value);

bool GMQCC_WARN ir_block_create_if(ir_block*, lex_ctx_t, ir_value *cond,
                                   ir_block *ontrue, ir_block *onfalse);
/* A 'goto' is an actual 'goto' coded in QC, whereas
 * a 'jump' is a virtual construct which simply names the
 * next block to go to.
 * A goto usually becomes an OP_GOTO in the resulting code,
 * whereas a 'jump' usually doesn't add any actual instruction.
 */
bool GMQCC_WARN ir_block_create_jump(ir_block*, lex_ctx_t, ir_block *to);
bool GMQCC_WARN ir_block_create_goto(ir_block*, lex_ctx_t, ir_block *to);

/* function */
typedef struct ir_function_s
{
    char      *name;
    int        outtype;
    int       *params;
    ir_block **blocks;

    uint32_t   flags;

    int builtin;

    ir_value *value;

    /* values generated from operations
     * which might get optimized away, so anything
     * in there needs to be deleted in the dtor.
     */
    ir_value **values;

    /* locally defined variables */
    ir_value **locals;

    size_t allocated_locals;
    size_t globaltemps;

    ir_block*     first;
    ir_block*     last;

    lex_ctx_t       context;

    /* for prototypes - first we generate all the
     * globals, and we remember teh function-defs
     * so we can later fill in the entry pos
     *
     * remember the ID:
     */
    qcint_t code_function_def;

    /* for temp allocation */
    size_t run_id;

    struct ir_builder_s *owner;

    /* vararg support: */
    size_t max_varargs;
} ir_function;

#define IR_FLAG_HAS_ARRAYS        (1<<1)
#define IR_FLAG_HAS_UNINITIALIZED (1<<2)
#define IR_FLAG_HAS_GOTO          (1<<3)
#define IR_FLAG_INCLUDE_DEF       (1<<4)
#define IR_FLAG_MASK_NO_OVERLAP     (IR_FLAG_HAS_ARRAYS | IR_FLAG_HAS_UNINITIALIZED)
#define IR_FLAG_MASK_NO_LOCAL_TEMPS (IR_FLAG_HAS_ARRAYS | IR_FLAG_HAS_UNINITIALIZED)

ir_value*       ir_function_create_local(ir_function *self, const char *name, int vtype, bool param);
bool GMQCC_WARN ir_function_finalize(ir_function*);
ir_block*       ir_function_create_block(lex_ctx_t ctx, ir_function*, const char *label);

/* builder */
#define IR_HT_SIZE 1024
#define IR_MAX_VINSTR_TEMPS 1
typedef struct ir_builder_s
{
    char *name;
    ir_function **functions;
    ir_value    **globals;
    ir_value    **fields;

    ht            htfunctions;
    ht            htglobals;
    ht            htfields;

    ir_value    **extparams;
    ir_value    **extparam_protos;

    /* the highest func->allocated_locals */
    size_t        max_locals;
    size_t        max_globaltemps;
    uint32_t      first_common_local;
    uint32_t      first_common_globaltemp;

    const char **filenames;
    qcint_t     *filestrings;
    /* we cache the #IMMEDIATE string here */
    qcint_t      str_immediate;
    /* there should just be this one nil */
    ir_value    *nil;
    ir_value    *reserved_va_count;
    /* some virtual instructions require temps, and their code is isolated
     * so that we don't need to keep track of their liveness.
     */
    ir_value    *vinstr_temp[IR_MAX_VINSTR_TEMPS];

    /* code generator */
    code_t      *code;
} ir_builder;

ir_builder*  ir_builder_new(const char *modulename);
void         ir_builder_delete(ir_builder*);
ir_function* ir_builder_create_function(ir_builder*, const char *name, int outtype);
ir_value*    ir_builder_create_global(ir_builder*, const char *name, int vtype);
ir_value*    ir_builder_create_field(ir_builder*, const char *name, int vtype);
ir_value*    ir_builder_get_va_count(ir_builder*);
bool         ir_builder_generate(ir_builder *self, const char *filename);
void         ir_builder_dump(ir_builder*, int (*oprintf)(const char*, ...));

/*
 * This code assumes 32 bit floats while generating binary
 * Blub: don't use extern here, it's annoying and shows up in nm
 * for some reason :P
 */
typedef int static_assert_is_32bit_float  [(sizeof(int32_t) == 4)?1:-1];
typedef int static_assert_is_32bit_integer[(sizeof(qcfloat_t) == 4)?1:-1];

#endif
