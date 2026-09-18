/* ============================================================
 * Nox runtime prelude
 * This file is prepended to every program the Nox compiler
 * generates. It is compiled together with the generated code
 * as a single translation unit by tcc.
 * ============================================================ */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <math.h>
#include <time.h>
#include <errno.h>

#ifdef NOX_NO_GC
/* This build targets a platform where the bundled Boehm GC isn't available.
 * Memory is left to accumulate for the life of the process instead of being
 * collected. */
#include <stdlib.h>
#define GC_MALLOC(sz) calloc(1, (sz))
#define GC_INIT() ((void)0)
#else
#include <gc.h>
#endif

