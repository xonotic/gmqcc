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


#ifdef __cplusplus
}
#endif /*! __cplusplus */
#endif /*! GMQCC_HDR */
