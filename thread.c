#include "gmqcc.h"

GMQCC_INLINE uint32_t util_atomic_xadd32(volatile uint32_t *x, uint32_t v)
{
    asm volatile (
        "lock xaddl %1, %0"
        : "+m"(*x), "=r"(v)
        : "1"(v)
        : "cc"
    );
    return v;
}
