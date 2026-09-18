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

