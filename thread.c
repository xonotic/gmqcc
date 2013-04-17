#include "gmqcc.h"

GMQCC_INLINE uint32_t util_atomic_xadd32(volatile uint32_t *x, uint32_t v)
{
    uint32_t r;
    asm volatile (
        "lock xadd %1, %0"
        : "+m"(*x), "=r"(r)
        : "1"(v)
        : "memory", "cc"
    );
    return r;
}
