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

#include <stdlib.h>

#include "gmqcc.h"
#include "liveness.h"

ir_bitlist_t *ir_bitlist_new() {
    ir_bitlist_t *list = (ir_bitlist_t*)mem_a(sizeof(ir_bitlist_t));
    list->bits = NULL;
    return list;
}

void ir_bitlist_delete(ir_bitlist_t *self) {
    if (self->bits)
        vec_free(self->bits);
    mem_d(self);
}

static GMQCC_INLINE void ir_bitlist_allocindex(ir_bitlist_t *self, size_t index) {
    size_t size  = vec_size(self->bits);
    while (size++ <= index)
        vec_push(self->bits, 0);
}

void ir_bitlist_setrange(ir_bitlist_t *self, size_t from, size_t to) {
    size_t index_from, bit_from, index_to, bit_to;
    GMQCC_BL_TYPE mask;

    if (from > to) {
        con_err("ir_bitlist_setrange: bad bits\n");
        abort();
    }

    ++to;
    index_from = from / GMQCC_BL_BITS;
    bit_from   = from % GMQCC_BL_BITS;
    index_to   = to   / GMQCC_BL_BITS;
    bit_to     = to   % GMQCC_BL_BITS;

    ir_bitlist_allocindex(self, index_to);

    mask = GMQCC_BL_FULL;

    if (index_from == index_to) {
        mask <<= bit_from;
        mask <<= GMQCC_BL_BITS - bit_to;
        mask >>= GMQCC_BL_BITS - bit_to;
        self->bits[index_from] |= mask;
        return;
    }

    /* first chunk: */
    self->bits[index_from] |= mask << bit_from;
    /* filled with all ones */
    for (++index_from; index_from != index_to; ++index_from)
        self->bits[index_from] |= mask;
    /* last chunk */
    mask <<= GMQCC_BL_BITS - bit_to;
    mask >>= GMQCC_BL_BITS - bit_to;
    self->bits[index_to] |= mask;
}

static void ir_bitlist_dump(const ir_bitlist_t *self,
                            int (*oprintf)(const char*, ...))
{
    size_t i;
    size_t size = vec_size(self->bits);
    if (!size)
        oprintf("<empty>");
    for (i = 0; i != size; ++i) {
        size_t b;
        for (b = 0; b != GMQCC_BL_BITS; ++b)
            oprintf( (self->bits[i] & (1UL<<b)) ? "1" : "0" );
    }
    oprintf("\n");
}

ir_lifemask_t *ir_lifemask_new(size_t size) {
    ir_lifemask_t *self;
    self        = (ir_lifemask_t*)mem_a(sizeof(*self));
    self->alive = ir_bitlist_new();
    self->dies  = ir_bitlist_new();
    self->used  = false;
    ir_bitlist_allocindex(self->alive, size / GMQCC_BL_BITS);
    ir_bitlist_allocindex(self->dies,  size / GMQCC_BL_BITS);
    return self;
}

void ir_lifemask_delete(ir_lifemask_t *self) {
    ir_bitlist_delete(self->alive);
    ir_bitlist_delete(self->dies);
    mem_d(self);
}

void ir_lifemask_merge(ir_lifemask_t *self, const ir_lifemask_t *other) {
    size_t i;
    size_t other_alive_size = vec_size(other->alive->bits);
    if (!other_alive_size)
        return;

    for (i = 0; i != other_alive_size; ++i) {
        self->alive->bits[i] |= other->alive->bits[i];
        self->dies->bits[i]  &= ~self->alive->bits[i];
        self->dies->bits[i]  |= ~other->alive->bits[i] & other->dies->bits[i];
        self->used = self->alive->bits[i] || self->used;
    }
}

bool ir_lifemask_overlaps(const ir_lifemask_t *a, const ir_lifemask_t *b) {
    size_t i;
    size_t size   = vec_size(a->alive->bits),
           size_b = vec_size(b->alive->bits);
    if (size > size_b)
        size = size_b;
    for (i = 0; i != size; ++i) {
        GMQCC_BL_TYPE mask_a = a->alive->bits[i] & ~a->dies->bits[i];
        GMQCC_BL_TYPE mask_b = b->alive->bits[i] & ~b->dies->bits[i];
        if (mask_a & mask_b)
            return true;
    }
    return false;
}

void ir_lifemask_dump(const ir_lifemask_t *self, const char *ind,
                      int (*oprintf)(const char*, ...))
{
    oprintf("{\n%s  ", ind);
    ir_bitlist_dump(self->alive, oprintf);
    oprintf("%s  ", ind);
    ir_bitlist_dump(self->dies, oprintf);
    oprintf("%s}\n", ind);
}
