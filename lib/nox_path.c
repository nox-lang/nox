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
#endif
    bool need_sep = a.data[a.len - 1] != '/' && a.data[a.len - 1] != '\\';
    int64_t n = a.len + (need_sep ? 1 : 0) + b.len;
    char *buf = (char *)NOX_ALLOC(n + 1);
    memcpy(buf, a.data, a.len);
    int64_t off = a.len;
    if (need_sep) buf[off++] = sep;
    memcpy(buf + off, b.data, b.len);
    buf[n] = 0;
    nox_string s;
    s.data = buf;
    s.len = n;
    return s;
}

nox_string nox_path_basename(nox_string p) {
    int64_t i = p.len;
    while (i > 0 && p.data[i - 1] != '/' && p.data[i - 1] != '\\') i--;
    return nox_string_from_bytes(p.data + i, p.len - i);
