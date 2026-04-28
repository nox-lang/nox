/* Nox standard library: io package
 * Part of lib/; compiled together with every generated program. */
#include <nox/nox.h>

/* ---------------- printing ---------------- */
void nox_print_int(int64_t v) { printf("%lld", (long long)v); }
void nox_print_float(double v) { printf("%g", v); }
void nox_print_bool(bool v) { printf("%s", v ? "true" : "false"); }
