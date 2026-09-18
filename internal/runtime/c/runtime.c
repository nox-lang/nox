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

/* ---------------- portable threading / thread-local storage ----------------
 * Nox's async/await/Parallel and per-thread error state need threads and
 * thread-local storage. On Windows this uses the native Win32 API
 * (CreateThread/TlsAlloc) directly rather than pthreads, since a pthreads
 * implementation isn't something this toolchain bundles for that target.
 * Everywhere else, plain POSIX pthreads is used (tcc does not support the
 * `__thread` storage-class keyword, which is why TLS goes through an
 * explicit key/slot API either way, not a compiler-level thread-local
 * variable). */
#if defined(_WIN32)
  #include <direct.h>
  #include <windows.h>
  #define NOX_MKDIR(p) _mkdir(p)

  typedef HANDLE nox_thread_t;
  #define NOX_THREAD_FUNC DWORD WINAPI
  typedef LPVOID nox_thread_arg_t;
  #define NOX_THREAD_RETURN return 0
  #define NOX_THREAD_CREATE(thptr, fn, arg) (*(thptr) = CreateThread(NULL, 0, (fn), (arg), 0, NULL))
  #define NOX_THREAD_JOIN(th) (WaitForSingleObject((th), INFINITE), CloseHandle(th))

  typedef DWORD nox_tls_key_t;
  #define NOX_TLS_CREATE(keyptr) (*(keyptr) = TlsAlloc())
  #define NOX_TLS_GET(key) TlsGetValue(key)
  #define NOX_TLS_SET(key, val) TlsSetValue((key), (val))
#else
  #include <pthread.h>
  #include <sys/stat.h>
  #include <sys/types.h>
  #include <dirent.h>
  #include <unistd.h>
  #define NOX_MKDIR(p) mkdir(p, 0755)

  typedef pthread_t nox_thread_t;
  #define NOX_THREAD_FUNC void*
  typedef void* nox_thread_arg_t;
  #define NOX_THREAD_RETURN return NULL
  #define NOX_THREAD_CREATE(thptr, fn, arg) pthread_create((thptr), NULL, (fn), (arg))
  #define NOX_THREAD_JOIN(th) pthread_join((th), NULL)

  typedef pthread_key_t nox_tls_key_t;
  #define NOX_TLS_CREATE(keyptr) pthread_key_create((keyptr), NULL)
  #define NOX_TLS_GET(key) pthread_getspecific(key)
  #define NOX_TLS_SET(key, val) pthread_setspecific((key), (val))
#endif

/* ---------------- error propagation state (per-thread) ---------------- */
typedef struct {
    bool has_err;
    char msg[1024];
} nox_err_state;

static nox_tls_key_t __nox_err_key;

/* Created once, eagerly, from nox_runtime_init() before any thread other
 * than the main one exists — this sidesteps needing a once-only-init
 * primitive (pthread_once has no simple portable equivalent on Windows). */
static void __nox_err_key_make(void) { NOX_TLS_CREATE(&__nox_err_key); }

static nox_err_state *nox_err_state_get(void) {
    nox_err_state *st = (nox_err_state *)NOX_TLS_GET(__nox_err_key);
    if (!st) {
        st = (nox_err_state *)GC_MALLOC(sizeof(nox_err_state));
        st->has_err = false;
        st->msg[0] = 0;
        NOX_TLS_SET(__nox_err_key, st);
    }
    return st;
}

#define NOX_HAS_ERR (nox_err_state_get()->has_err)

static void nox_clear_error(void) {
    nox_err_state_get()->has_err = false;
}

static void nox_set_error(const char *msg) {
    nox_err_state *st = nox_err_state_get();
    st->has_err = true;
    if (msg) {
        strncpy(st->msg, msg, sizeof(st->msg) - 1);
        st->msg[sizeof(st->msg) - 1] = 0;
    } else {
        st->msg[0] = 0;
    }
}

static void nox_panic(const char *msg) {
    fprintf(stderr, "nox: runtime error: %s\n", msg);
    exit(1);
}

/* ---------------- string ---------------- */
typedef struct {
    char *data;   /* not necessarily NUL free of interior bytes, but we NUL-terminate for C interop convenience */
    int64_t len;
} nox_string;

static nox_string nox_string_from_bytes(const char *bytes, int64_t len) {
    char *buf = (char *)GC_MALLOC(len + 1);
    if (len > 0) memcpy(buf, bytes, len);
    buf[len] = 0;
    nox_string s;
    s.data = buf;
    s.len = len;
    return s;
}

static nox_string nox_string_from_cstr(const char *cstr) {
    if (!cstr) return nox_string_from_bytes("", 0);
    return nox_string_from_bytes(cstr, (int64_t)strlen(cstr));
}

static nox_string nox_string_concat(nox_string a, nox_string b) {
    int64_t n = a.len + b.len;
    char *buf = (char *)GC_MALLOC(n + 1);
    if (a.len > 0) memcpy(buf, a.data, a.len);
    if (b.len > 0) memcpy(buf + a.len, b.data, b.len);
    buf[n] = 0;
    nox_string s;
    s.data = buf;
    s.len = n;
    return s;
}

static bool nox_string_eq(nox_string a, nox_string b) {
    if (a.len != b.len) return false;
    if (a.len == 0) return true;
    return memcmp(a.data, b.data, (size_t)a.len) == 0;
}

static int nox_string_cmp(nox_string a, nox_string b) {
    int64_t n = a.len < b.len ? a.len : b.len;
    int c = n > 0 ? memcmp(a.data, b.data, (size_t)n) : 0;
    if (c != 0) return c;
    if (a.len < b.len) return -1;
    if (a.len > b.len) return 1;
    return 0;
}

static bool nox_string_contains(nox_string s, nox_string sub) {
    if (sub.len == 0) return true;
    if (sub.len > s.len) return false;
    return strstr(s.data, sub.data) != NULL;
}

static bool nox_string_starts_with(nox_string s, nox_string pre) {
    if (pre.len > s.len) return false;
    return memcmp(s.data, pre.data, (size_t)pre.len) == 0;
}

static bool nox_string_ends_with(nox_string s, nox_string suf) {
    if (suf.len > s.len) return false;
    return memcmp(s.data + (s.len - suf.len), suf.data, (size_t)suf.len) == 0;
}

static nox_string nox_string_substring(nox_string s, int64_t start, int64_t end) {
    if (start < 0) start = 0;
    if (end > s.len) end = s.len;
    if (end < start) end = start;
    return nox_string_from_bytes(s.data + start, end - start);
}

/* conversions */
static nox_string nox_int_to_string(int64_t v) {
    char buf[32];
    int n = snprintf(buf, sizeof(buf), "%lld", (long long)v);
    return nox_string_from_bytes(buf, n);
}
static nox_string nox_float_to_string(double v) {
    char buf[64];
    int n = snprintf(buf, sizeof(buf), "%g", v);
    return nox_string_from_bytes(buf, n);
}
static nox_string nox_bool_to_string(bool v) {
    return nox_string_from_cstr(v ? "true" : "false");
}
static int64_t nox_string_to_int(nox_string s) {
    return (int64_t)strtoll(s.data, NULL, 10);
}
static double nox_string_to_float(nox_string s) {
    return strtod(s.data, NULL);
}
static bool nox_string_to_bool(nox_string s) {
    return nox_string_eq(s, nox_string_from_cstr("true"));
}
static int64_t nox_float_to_int(double v) { return (int64_t)v; }
static double nox_int_to_float(int64_t v) { return (double)v; }
static bool nox_int_to_bool(int64_t v) { return v != 0; }
static bool nox_float_to_bool(double v) { return v != 0.0; }
static int64_t nox_bool_to_int(bool v) { return v ? 1 : 0; }
static double nox_bool_to_float(bool v) { return v ? 1.0 : 0.0; }

static nox_string nox_get_error_message(void) {
    return nox_string_from_cstr(nox_err_state_get()->msg);
}

/* ---------------- dynamic array (type-erased; codegen supplies casts) ---------------- */
typedef struct {
    void *data;
    int64_t len;
    int64_t cap;
} nox_array;

static nox_array nox_array_new(void) {
    nox_array a;
    a.data = NULL;
    a.len = 0;
    a.cap = 0;
    return a;
}

static void nox_array_reserve(nox_array *a, int64_t mincap, int64_t elemsize) {
    if (a->cap >= mincap) return;
    int64_t newcap = a->cap < 4 ? 4 : a->cap * 2;
    if (newcap < mincap) newcap = mincap;
    void *nd = GC_MALLOC(newcap * elemsize);
    if (a->len > 0) memcpy(nd, a->data, (size_t)(a->len * elemsize));
    a->data = nd;
    a->cap = newcap;
}

static void nox_array_push_raw(nox_array *a, const void *elem, int64_t elemsize) {
    nox_array_reserve(a, a->len + 1, elemsize);
    memcpy((char *)a->data + a->len * elemsize, elem, (size_t)elemsize);
    a->len++;
}

static nox_array nox_string_split_lines(nox_string s) {
    nox_array result = nox_array_new();
    int64_t start = 0;
    for (int64_t i = 0; i < s.len; i++) {
        if (s.data[i] == '\n') {
            int64_t end = i;
            if (end > start && s.data[end - 1] == '\r') end--;
            nox_string line = nox_string_from_bytes(s.data + start, end - start);
            nox_array_push_raw(&result, &line, sizeof(nox_string));
            start = i + 1;
        }
    }
    if (start < s.len) {
        nox_string line = nox_string_from_bytes(s.data + start, s.len - start);
        nox_array_push_raw(&result, &line, sizeof(nox_string));
    }
    return result;
}

static void nox_array_pop_raw(nox_array *a, void *out, int64_t elemsize) {
    if (a->len == 0) nox_panic("pop from empty array");
    a->len--;
    memcpy(out, (char *)a->data + a->len * elemsize, (size_t)elemsize);
}

static void nox_array_check_index(nox_array *a, int64_t i) {
    if (i < 0 || i >= a->len) {
        char buf[128];
        snprintf(buf, sizeof(buf), "array index out of range: index %lld, length %lld", (long long)i, (long long)a->len);
        nox_panic(buf);
    }
}

static void nox_array_insert_raw(nox_array *a, int64_t idx, const void *elem, int64_t elemsize) {
    if (idx < 0 || idx > a->len) nox_panic("array insert index out of range");
    nox_array_reserve(a, a->len + 1, elemsize);
    char *base = (char *)a->data;
    memmove(base + (idx + 1) * elemsize, base + idx * elemsize, (size_t)((a->len - idx) * elemsize));
    memcpy(base + idx * elemsize, elem, (size_t)elemsize);
    a->len++;
}

static void nox_array_remove_raw(nox_array *a, int64_t idx, int64_t elemsize) {
    nox_array_check_index(a, idx);
    char *base = (char *)a->data;
    memmove(base + idx * elemsize, base + (idx + 1) * elemsize, (size_t)((a->len - idx - 1) * elemsize));
    a->len--;
}

static void nox_array_clear(nox_array *a) {
    a->len = 0;
}

static nox_array nox_array_reverse_raw(nox_array a, int64_t elemsize) {
    nox_array r = nox_array_new();
    nox_array_reserve(&r, a.len, elemsize);
    for (int64_t i = 0; i < a.len; i++) {
        memcpy((char *)r.data + i * elemsize, (char *)a.data + (a.len - 1 - i) * elemsize, (size_t)elemsize);
    }
    r.len = a.len;
    return r;
}

static void nox_array_shuffle_raw(nox_array *a, int64_t elemsize) {
    char *tmp = (char *)GC_MALLOC(elemsize);
    char *base = (char *)a->data;
    for (int64_t i = a->len - 1; i > 0; i--) {
        int64_t j = rand() % (i + 1);
        memcpy(tmp, base + i * elemsize, (size_t)elemsize);
        memcpy(base + i * elemsize, base + j * elemsize, (size_t)elemsize);
        memcpy(base + j * elemsize, tmp, (size_t)elemsize);
    }
}

static void nox_array_choice_raw(nox_array *a, void *out, int64_t elemsize) {
    if (a->len == 0) nox_panic("choice on empty array");
    int64_t i = rand() % a->len;
    memcpy(out, (char *)a->data + i * elemsize, (size_t)elemsize);
}

/* default ascending comparators for sort() with no custom comparator */
static int nox_cmp_int_asc(const void *pa, const void *pb) {
    int64_t a = *(const int64_t *)pa, b = *(const int64_t *)pb;
    return (a > b) - (a < b);
}
static int nox_cmp_float_asc(const void *pa, const void *pb) {
    double a = *(const double *)pa, b = *(const double *)pb;
    return (a > b) - (a < b);
}
static int nox_cmp_string_asc(const void *pa, const void *pb) {
    return nox_string_cmp(*(const nox_string *)pa, *(const nox_string *)pb);
}
static int nox_cmp_bool_asc(const void *pa, const void *pb) {
    bool a = *(const bool *)pa, b = *(const bool *)pb;
    return (a ? 1 : 0) - (b ? 1 : 0);
}

/* ---------------- printing ---------------- */
static void nox_print_int(int64_t v) { printf("%lld", (long long)v); }
static void nox_print_float(double v) { printf("%g", v); }
static void nox_print_bool(bool v) { printf("%s", v ? "true" : "false"); }
static void nox_print_string(nox_string v) { fwrite(v.data, 1, (size_t)v.len, stdout); }
static void nox_print_raw_cstr(const char *s) { fputs(s, stdout); }

/* ---------------- reading ---------------- */
static nox_string nox_io_scanln(void) {
    /* Implemented with fgetc() rather than POSIX getline(), which mingw's
     * Windows C runtime does not provide. */
    size_t cap = 128;
    size_t len = 0;
    char *buf = (char *)GC_MALLOC(cap);
    int c;
    bool any = false;
    while ((c = fgetc(stdin)) != EOF) {
        any = true;
        if (c == '\n') break;
        if (len + 1 >= cap) {
            size_t newcap = cap * 2;
            char *nb = (char *)GC_MALLOC(newcap);
            memcpy(nb, buf, len);
            buf = nb;
            cap = newcap;
        }
        buf[len++] = (char)c;
    }
    if (!any) return nox_string_from_cstr("");
    if (len > 0 && buf[len - 1] == '\r') len--;
    return nox_string_from_bytes(buf, (int64_t)len);
}

static nox_string nox_io_scan(void) {
    char buf[4096];
    if (scanf("%4095s", buf) != 1) return nox_string_from_cstr("");
    return nox_string_from_cstr(buf);
}

/* ---------------- math ---------------- */
static int64_t nox_math_abs_i(int64_t v) { return v < 0 ? -v : v; }
static double nox_math_abs_f(double v) { return fabs(v); }
static int64_t nox_math_min_i(int64_t a, int64_t b) { return a < b ? a : b; }
static int64_t nox_math_max_i(int64_t a, int64_t b) { return a > b ? a : b; }
static double nox_math_min_f(double a, double b) { return a < b ? a : b; }
static double nox_math_max_f(double a, double b) { return a > b ? a : b; }

/* ---------------- filesystem ---------------- */
static nox_string nox_fs_read(nox_string path) {
    FILE *f = fopen(path.data, "rb");
    if (!f) { nox_set_error(strerror(errno)); return nox_string_from_cstr(""); }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = (char *)GC_MALLOC(sz + 1);
    size_t rd = fread(buf, 1, (size_t)sz, f);
    buf[rd] = 0;
    fclose(f);
    nox_string s;
    s.data = buf;
    s.len = (int64_t)rd;
    return s;
}

static void nox_fs_write(nox_string path, nox_string data) {
    FILE *f = fopen(path.data, "wb");
    if (!f) { nox_set_error(strerror(errno)); return; }
    fwrite(data.data, 1, (size_t)data.len, f);
    fclose(f);
}

static void nox_fs_append(nox_string path, nox_string data) {
    FILE *f = fopen(path.data, "ab");
    if (!f) { nox_set_error(strerror(errno)); return; }
    fwrite(data.data, 1, (size_t)data.len, f);
    fclose(f);
}

static bool nox_fs_exists(nox_string path) {
#if defined(_WIN32)
    DWORD attr = GetFileAttributesA(path.data);
    return attr != INVALID_FILE_ATTRIBUTES;
#else
    struct stat st;
    return stat(path.data, &st) == 0;
#endif
}

static void nox_fs_remove(nox_string path) {
    if (remove(path.data) != 0) nox_set_error(strerror(errno));
}

static void nox_fs_rename(nox_string oldp, nox_string newp) {
    if (rename(oldp.data, newp.data) != 0) nox_set_error(strerror(errno));
}

static void nox_fs_copy(nox_string src, nox_string dst) {
    FILE *in = fopen(src.data, "rb");
    if (!in) { nox_set_error(strerror(errno)); return; }
    FILE *out = fopen(dst.data, "wb");
    if (!out) { nox_set_error(strerror(errno)); fclose(in); return; }
    char buf[8192];
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), in)) > 0) fwrite(buf, 1, n, out);
    fclose(in);
    fclose(out);
}

static void nox_fs_mkdir(nox_string path) {
    if (NOX_MKDIR(path.data) != 0 && errno != EEXIST) nox_set_error(strerror(errno));
}

static void nox_fs_rmdir(nox_string path) {
#if defined(_WIN32)
    if (_rmdir(path.data) != 0) nox_set_error(strerror(errno));
#else
    if (rmdir(path.data) != 0) nox_set_error(strerror(errno));
#endif
}

static nox_array nox_fs_list(nox_string path) {
    nox_array result = nox_array_new();
#if defined(_WIN32)
    WIN32_FIND_DATAA fd;
    char pattern[1024];
    snprintf(pattern, sizeof(pattern), "%s\\*", path.data);
    HANDLE h = FindFirstFileA(pattern, &fd);
    if (h == INVALID_HANDLE_VALUE) { nox_set_error("cannot list directory"); return result; }
    do {
        if (strcmp(fd.cFileName, ".") == 0 || strcmp(fd.cFileName, "..") == 0) continue;
        nox_string s = nox_string_from_cstr(fd.cFileName);
        nox_array_push_raw(&result, &s, sizeof(nox_string));
    } while (FindNextFileA(h, &fd));
    FindClose(h);
#else
    DIR *d = opendir(path.data);
    if (!d) { nox_set_error(strerror(errno)); return result; }
    struct dirent *ent;
    while ((ent = readdir(d)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0) continue;
        nox_string s = nox_string_from_cstr(ent->d_name);
        nox_array_push_raw(&result, &s, sizeof(nox_string));
    }
    closedir(d);
#endif
    return result;
}

/* ---------------- path ---------------- */
static nox_string nox_path_join2(nox_string a, nox_string b) {
    if (a.len == 0) return b;
    if (b.len == 0) return a;
#if defined(_WIN32)
    char sep = '\\';
#else
    char sep = '/';
#endif
    bool need_sep = a.data[a.len - 1] != '/' && a.data[a.len - 1] != '\\';
    int64_t n = a.len + (need_sep ? 1 : 0) + b.len;
    char *buf = (char *)GC_MALLOC(n + 1);
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

static nox_string nox_path_basename(nox_string p) {
    int64_t i = p.len;
    while (i > 0 && p.data[i - 1] != '/' && p.data[i - 1] != '\\') i--;
    return nox_string_from_bytes(p.data + i, p.len - i);
}

static nox_string nox_path_dirname(nox_string p) {
    int64_t i = p.len;
    while (i > 0 && p.data[i - 1] != '/' && p.data[i - 1] != '\\') i--;
    if (i == 0) return nox_string_from_cstr(".");
    if (i == 1) return nox_string_from_bytes(p.data, 1);
    return nox_string_from_bytes(p.data, i - 1);
}

static nox_string nox_path_ext(nox_string p) {
    int64_t i = p.len;
    while (i > 0 && p.data[i - 1] != '.' && p.data[i - 1] != '/' && p.data[i - 1] != '\\') i--;
    if (i == 0 || p.data[i - 1] != '.') return nox_string_from_cstr("");
    return nox_string_from_bytes(p.data + i - 1, p.len - i + 1);
}

static nox_string nox_path_stem(nox_string p) {
    nox_string base = nox_path_basename(p);
    int64_t i = base.len;
    while (i > 0 && base.data[i - 1] != '.') i--;
    if (i <= 1) return base;
    return nox_string_from_bytes(base.data, i - 1);
}

static nox_string nox_path_absolute(nox_string p) {
    char buf[4096];
#if defined(_WIN32)
    if (_fullpath(buf, p.data, sizeof(buf)) == NULL) return p;
#else
    if (realpath(p.data, buf) == NULL) return p;
#endif
    return nox_string_from_cstr(buf);
}

