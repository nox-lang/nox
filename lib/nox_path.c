/* Nox standard library: path package
 * Part of lib/; compiled together with every generated program. */
#include <nox/nox.h>

/* ---------------- path ---------------- */
nox_string nox_path_join2(nox_string a, nox_string b) {
    if (a.len == 0) return b;
    if (b.len == 0) return a;
#if defined(_WIN32)
    char sep = '\\';
#else
    char sep = '/';
