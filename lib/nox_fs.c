/* Nox standard library: fs package
 * Part of lib/; compiled together with every generated program. */
#include <nox/nox.h>

/* ---------------- filesystem ---------------- */
nox_string nox_fs_read(nox_string path) {
    FILE *f = fopen(path.data, "rb");
    if (!f) { nox_set_error(strerror(errno)); return nox_string_from_cstr(""); }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = (char *)NOX_ALLOC(sz + 1);
    size_t rd = fread(buf, 1, (size_t)sz, f);
    buf[rd] = 0;
    fclose(f);
    nox_string s;
    s.data = buf;
    s.len = (int64_t)rd;
    return s;
}

void nox_fs_write(nox_string path, nox_string data) {
    FILE *f = fopen(path.data, "wb");
    if (!f) { nox_set_error(strerror(errno)); return; }
    fwrite(data.data, 1, (size_t)data.len, f);
    fclose(f);
}

void nox_fs_append(nox_string path, nox_string data) {
    FILE *f = fopen(path.data, "ab");
    if (!f) { nox_set_error(strerror(errno)); return; }
    fwrite(data.data, 1, (size_t)data.len, f);
    fclose(f);
}

bool nox_fs_exists(nox_string path) {
#if defined(_WIN32)
    DWORD attr = GetFileAttributesA(path.data);
    return attr != INVALID_FILE_ATTRIBUTES;
#else
    struct stat st;
    return stat(path.data, &st) == 0;
#endif
}

void nox_fs_remove(nox_string path) {
    if (remove(path.data) != 0) nox_set_error(strerror(errno));
}

void nox_fs_rename(nox_string oldp, nox_string newp) {
    if (rename(oldp.data, newp.data) != 0) nox_set_error(strerror(errno));
}

void nox_fs_copy(nox_string src, nox_string dst) {
    FILE *in = fopen(src.data, "rb");
    if (!in) { nox_set_error(strerror(errno)); return; }
    FILE *out = fopen(dst.data, "wb");
