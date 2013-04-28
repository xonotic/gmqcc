/*
 * Copyright (C) 2012, 2013
 *     Dale Weiler
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
#ifndef GMQCC_HDR
#define GMQCC_HDR
#ifdef __cplusplus
extern "C" {
#endif /*! __cplusplus */

/*
 * Forward declare everything since these are `private` implementations
 * that the user shouldn't have information of.
 */
struct gmqcc_preprocess_s;
struct gmqcc_compiler_s;

/*
 * Function: gmqcc_global_setmemory
 *  Set implementations for dynamic memory manipulation.
 *
 * Parameters:
 *  malloc_impl  - Pointer to malloc function
 *  realloc_impl - Pointer to realloc function
 *  free_impl    - Pointer to free function
 *
 * Returns:
 *  true on success, false otherwise.
 *  
 * Remarks:
 *  Do not call this function unless you want to provide
 *  your own functions, by default the standard C library
 *  functions are used.
 */           
bool gmqcc_global_setmemory (
    void *(*malloc_impl) (size_t),
    void *(*realloc_impl)(void *, size_t),
    void  (*free_impl)   (void *)
);

/*
 * Funciton: gmqcc_global_geterror
 *  Get a const string-literal of the last error which occured.
 *
 * Returns:
 *  A string-literal describing the error which occured last.
 *
 * Remarks:     
 *  This should be called to get better diagnostic when a function
 *  doesn't return successfully.
 */ 
const char *gmqcc_global_geterror(void);

/*
 * Function: gmqcc_preprocess_create
 *  Creates a preprocessor context
 *
 * Returns:
 *  Preprocessor context on success, NULL otherwise
 */
struct gmqcc_preprocess_s *gmqcc_preprocess_create(void);

/*
 * Function: gmqcc_preprocess_file
 *  Preprocesses a file for a given preprocessor context
 *
 * Parameters:
 *  pp       - Pointer to a preprocessor context
 *  filename - Filename as string to be opened and preprocessed
 *
 * Returns:
 *  true on success, false otherwise.
 */     
bool gmqcc_preprocess_file(
    struct gmqcc_preprocess_s *pp,
    const char                *filename
);

/*
 * Function: gmqcc_preprocess_string
 *  Preprocesses a string for a given preprocessor context
 *
 * Parameters:
 *  pp   - Pointer to a preprocessor context
 *  name - Name for the given string (used in error reporting)
 *  str  - String to be preprocessed
 *
 * Returns:
 *  true on success, false otherwise.
 */      
bool gmqcc_preprocess_string   (
    struct gmqcc_preprocess_s *pp,
    const char                *name,
    const char                *str
);

/*
 * Function: gmqcc_preprocess_adddefine
 *  Defines a macro for a given preprocessor context
 *
 * Parameters:
 *  pp    - Pointer to a preprocessor context
 *  ident - Identifier for the given definition (used in error reporting)
 *  name  - Name of macro to define
 */ 
void gmqcc_preprocess_adddefine(
    struct gmqcc_preprocess_s *pp,
    const char                *ident,
    const char                *name
);

/*
 * Function: gmqcc_preprocess_addmacro
 *  Defines a macro with a value for a given preprocessor context
 *
 * Parameters:
 *  pp    - Pointer to a preprocessor context
 *  name  - Name of the macro to define
 *  value - Value of the macro
 */ 
void gmqcc_preprocess_addmacro(
    struct gmqcc_preprocess_s *pp,
    const char                *name,
    const char                *value
);

/*
 * Function: gmqcc_preprocess_get
 *  Get preprocessed data as string
 *
 * Parameters:
 *  pp - Pointer to preprocessor context
 *
 * Returns:
 *  const string of the preprocessed contents.
 */        
const char *gmqcc_preprocess_get(
    struct gmqcc_preprocess_s *pp
);

/*
 * Function: gmqcc_preprocess_flush
 *  Flush contents to be preprocessed
 *
 * Parameters:
 *  pp - Pointer to preprocessor context
 */     
void gmqcc_preprocess_flush(
    struct gmqcc_preprocess_s *pp
);

/*
 * Function: gmqcc_preprocess_destroy
 *  Destroy preprocessor context
 *
 * Parameters:
 *  pp - Pointer to preprocessor context
 */     
void gmqcc_preprocess_destroy(
    struct gmqcc_preprocess_s *pp
);


/*
 * Enumeration: gmqcc_compiler_context_t
 *  List of bindable contexts for a compiler context
 *
 *  GMQCC_CONTEXT_PREPROCESSOR - A preprocessor context
 *  GMQCC_CONTEXT_PARSER       - A parser context
 *  GMQCC_CONTEXT_CODE         - A code generator context 
 */  
typedef enum {
    GMQCC_CONTEXT_PREPROCESSOR,
    GMQCC_CONTEXT_PARSER,
    GMQCC_CONTEXT_CODE
} gmqcc_compiler_context_t;

/*
 * Macro: GMQCC_CONTEXT_CAST
 *  A macro for casting a bindable context, used primarly for context
 *  manipulation where the functions are abstract and require a cast.
 *
 * See Also:
 *  <gmqcc_compiler_attachcontext>   
 */   
#define GMQCC_CONTEXT_CAST(X) ((void*)(X))

/*
 * Function: gmqcc_compiler_create
 *  Creates a compiler which will require attachments to
 *  function.
 *
 * Returns:
 *  A new compiler context
 *
 * Remarks:
 *  Inorder to use the compiler context you'll require
 *  at minimal a code and parser context.
 *
 * See Also:
 *  <gmqcc_compiler_attachcontext>
 */
struct gmqcc_compiler_s *gmqcc_compiler_create(void);

/*
 * Function: gmqcc_compiler_attachcontext
 *  Used to attach a context to a compiler context.
 *
 * Parameters:
 *  compiler - Compiler context
 *  base     - Base pointer to attachment context
 *  context  - The type of context (see <gmqcc_compiler_context_t> et. all)
 *
 * Returns:
 *  true on success, false otherwise.
 *
 * Remarks:
 *  A parser context is mandatory, other contexts are optional.
 *  Contexts can be swaped around, i.e you can detach a parser context,
 *  back it up somewhere apply a new parser context and run with that.
 */   
bool gmqcc_compiler_attachcontext(
    struct gmqcc_compiler_s  *compiler,
    void                     *base,
    gmqcc_compiler_context_t  context
);

/*
 * Function: gmqcc_compiler_destroy
 *  Destroys a compiler context.
 *
 * Parameters:
 *  compiler - Compiler context
 *
 * Remarks:
 *  This function doesn't destory any associated contexts with
 *  the compiler context itself.
 */
void gmqcc_compiler_destroy(
    struct gmqcc_compiler_s *compiler
);

#ifdef __cplusplus
}
#endif /*! __cplusplus */
#endif /*! GMQCC_HDR */
