/*
 * Copyright (C) 2012, 2013, 2014
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
#ifndef GMQCC_LIVENESS_HDR
#define GMQCC_LIVENESS_HDR

#define GMQCC_BL_BITS 32
#define GMQCC_BL_FULL 0xFFFFFFFF
#define GMQCC_BL_MAKETYPE1(TY) uint##TY##_t
#define GMQCC_BL_MAKETYPE(TY) GMQCC_BL_MAKETYPE1(TY)
#define GMQCC_BL_TYPE GMQCC_BL_MAKETYPE(GMQCC_BL_BITS)
typedef struct {
    GMQCC_BL_TYPE *bits; /* vector */
} ir_bitlist_t;

ir_bitlist_t *ir_bitlist_new     (void);
void          ir_bitlist_delete  (ir_bitlist_t*);
/* precondition: from <= to */
void          ir_bitlist_setrange(ir_bitlist_t*, size_t from, size_t to);

typedef struct {
    /* note that the following two lists always have the same size */

    ir_bitlist_t *alive; /* read or generally alive */
    ir_bitlist_t *dies;  /* instructions which only WRITE to this value */

    /* When a value is written to without being read from it dies. This is
     * used to mask out that specific instruction before checking whether
     * a value's lifetime overlaps with another's.
     */

    /* also note whether a value was alive at all anywhere: */
    bool          used;
} ir_lifemask_t;

ir_lifemask_t *ir_lifemask_new   (size_t size);
void           ir_lifemask_delete(ir_lifemask_t*);
void           ir_lifemask_merge (ir_lifemask_t*, const ir_lifemask_t*);

/*void ir_lifemask_setmask (ir_lifemask_t*);*/
bool ir_lifemask_overlaps(const ir_lifemask_t*, const ir_lifemask_t*);

/* debug */
void ir_lifemask_dump(const ir_lifemask_t *self, const char *ind,
                      int (*oprintf)(const char*, ...));

/* inlined functions */
GMQCC_INLINE static void ir_bitlist_setbit(ir_bitlist_t *self, size_t bit) {
    size_t index = bit / GMQCC_BL_BITS;
    self->bits[index] |= (1 << (bit % GMQCC_BL_BITS));
}

GMQCC_INLINE static void ir_bitlist_unsetbit(ir_bitlist_t *self, size_t bit) {
    size_t index = bit / GMQCC_BL_BITS;
    self->bits[index] &= ~(1 << (bit % GMQCC_BL_BITS));
}

GMQCC_INLINE static bool ir_bitlist_getbit(const ir_bitlist_t *self, size_t bit) {
    size_t        index = bit / GMQCC_BL_BITS;
    GMQCC_BL_TYPE mask  = 1 << (bit % GMQCC_BL_BITS);
    return !!(self->bits[index] & mask);
}

#endif
