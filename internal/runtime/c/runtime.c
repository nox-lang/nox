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

