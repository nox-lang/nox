/* Nox standard library: math package
 * Part of lib/; compiled together with every generated program. */
#include <nox/nox.h>

/* ---------------- math ---------------- */
int64_t nox_math_abs_i(int64_t v) { return v < 0 ? -v : v; }
double nox_math_abs_f(double v) { return fabs(v); }
int64_t nox_math_min_i(int64_t a, int64_t b) { return a < b ? a : b; }
int64_t nox_math_max_i(int64_t a, int64_t b) { return a > b ? a : b; }
double nox_math_min_f(double a, double b) { return a < b ? a : b; }
double nox_math_max_f(double a, double b) { return a > b ? a : b; }

