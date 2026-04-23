/* Nox standard library: core (errors, strings, slices, runtime start-up)
 * Part of lib/; compiled together with every generated program. */
#include <nox/nox.h>

#if !NOX_USE_GC
void *nox_calloc(size_t sz) {
    void *p = calloc(1, sz ? sz : 1);
    if (!p) { fputs("nox: out of memory\n", stderr); exit(1); }
