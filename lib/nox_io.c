/* Nox standard library: io package
 * Part of lib/; compiled together with every generated program. */
#include <nox/nox.h>

/* ---------------- printing ---------------- */
void nox_print_int(int64_t v) { printf("%lld", (long long)v); }
void nox_print_float(double v) { printf("%g", v); }
void nox_print_bool(bool v) { printf("%s", v ? "true" : "false"); }
void nox_print_string(nox_string v) { fwrite(v.data, 1, (size_t)v.len, stdout); }
void nox_print_raw_cstr(const char *s) { fputs(s, stdout); }


/* ---------------- reading ---------------- */
nox_string nox_io_scanln(void) {
    /* Implemented with fgetc() rather than POSIX getline(), which mingw's
     * Windows C runtime does not provide. */
    size_t cap = 128;
    size_t len = 0;
    char *buf = (char *)NOX_ALLOC(cap);
    int c;
