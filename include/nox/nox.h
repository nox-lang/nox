/* ============================================================
 * nox/nox.h — the Nox runtime & standard library interface.
 *
 * Every program the Nox compiler generates starts with
 *     #include <nox/nox.h>
 * and is compiled (by the bundled nox-tcc) together with the
 * implementation files in <nox root>/lib/*.c.
 * ============================================================ */
#ifndef NOX_NOX_H
#define NOX_NOX_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <math.h>
#include <time.h>
#include <errno.h>

/* ---------------- memory management ----------------
 * Native (non-Windows) builds use the Boehm collector. On Windows — and
 * whenever NOX_NO_GC is defined — allocation is a plain zero-initialising
 * calloc that is never freed: correct and thread-safe, just not collected. */
#if defined(NOX_NO_GC) || (defined(_WIN32) && !defined(NOX_WIN32_GC))
  #define NOX_USE_GC 0
#else
  #define NOX_USE_GC 1
#endif

#if NOX_USE_GC
  #ifndef GC_THREADS
  #define GC_THREADS 1
  #endif
