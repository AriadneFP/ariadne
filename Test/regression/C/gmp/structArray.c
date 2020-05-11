#include <gmp.h>
mpq_t deleteme;
static struct {int n; double f; long i; } fact_table[3] = {
    { 0,  1,     1L     },
    { 1,  1,     1L     },
    { 2,  2,     2L     }
};