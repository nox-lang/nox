/* Nox standard library: fs package
 * Part of lib/; compiled together with every generated program. */
#include <nox/nox.h>

/* ---------------- filesystem ---------------- */
nox_string nox_fs_read(nox_string path) {
    FILE *f = fopen(path.data, "rb");
    if (!f) { nox_set_error(strerror(errno)); return nox_string_from_cstr(""); }
