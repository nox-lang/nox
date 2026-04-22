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

