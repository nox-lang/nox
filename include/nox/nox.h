/* ============================================================
 * nox/nox.h — the Nox runtime & standard library interface.
 *
 * Every program the Nox compiler generates starts with
 *     #include <nox/nox.h>
 * and is compiled (by the bundled nox-tcc) together with the
 * implementation files in <nox root>/lib/*.c.
 * ============================================================ */
#ifndef NOX_NOX_H
#define NOX_NOX_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <math.h>
#include <time.h>
#include <errno.h>

/* ---------------- memory management ----------------
 * Native (non-Windows) builds use the Boehm collector. On Windows — and
 * whenever NOX_NO_GC is defined — allocation is a plain zero-initialising
 * calloc that is never freed: correct and thread-safe, just not collected. */
#if defined(NOX_NO_GC) || (defined(_WIN32) && !defined(NOX_WIN32_GC))
  #define NOX_USE_GC 0
#else
  #define NOX_USE_GC 1
#endif

#if NOX_USE_GC
  #ifndef GC_THREADS
  #define GC_THREADS 1
  #endif
  #include <gc.h>
  #define NOX_ALLOC(sz) GC_MALLOC(sz)
  #define NOX_FREE(p) GC_FREE(p)
  #define NOX_GC_INIT() GC_INIT()
#else
  void *nox_calloc(size_t sz);
  #define NOX_ALLOC(sz) nox_calloc(sz)
  #define NOX_FREE(p) free(p)
  #define NOX_GC_INIT() ((void)0)
#endif

/* ---------------- portable threading / thread-local storage / mutex ----------------
 * Windows uses the native Win32 API (CreateThread / TlsAlloc /
 * CRITICAL_SECTION); everything else uses POSIX pthreads. tcc has no
 * `__thread`, so thread-local state goes through an explicit key API. */
#if defined(_WIN32)
  #include <direct.h>
  #include <windows.h>
  #define NOX_MKDIR(p) _mkdir(p)

  typedef HANDLE nox_thread_t;
  #define NOX_THREAD_FUNC DWORD WINAPI
  typedef LPVOID nox_thread_arg_t;
  #define NOX_THREAD_RETURN return 0
  #define NOX_THREAD_CREATE(thptr, fn, arg) (*(thptr) = CreateThread(NULL, 0, (fn), (arg), 0, NULL), (*(thptr) == NULL))
  #define NOX_THREAD_JOIN(th) (WaitForSingleObject((th), INFINITE), CloseHandle(th))

  typedef DWORD nox_tls_key_t;
  #define NOX_TLS_CREATE(keyptr) (*(keyptr) = TlsAlloc())
  #define NOX_TLS_GET(key) TlsGetValue(key)
  #define NOX_TLS_SET(key, val) TlsSetValue((key), (val))

  typedef CRITICAL_SECTION nox_mutex_t;
  #define NOX_MUTEX_INIT(m) InitializeCriticalSection(m)
  #define NOX_MUTEX_LOCK(m) EnterCriticalSection(m)
  #define NOX_MUTEX_UNLOCK(m) LeaveCriticalSection(m)
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
  #define NOX_THREAD_CREATE(thptr, fn, arg) (pthread_create((thptr), NULL, (fn), (arg)) != 0)
  #define NOX_THREAD_JOIN(th) pthread_join((th), NULL)

  typedef pthread_key_t nox_tls_key_t;
  #define NOX_TLS_CREATE(keyptr) pthread_key_create((keyptr), NULL)
  #define NOX_TLS_GET(key) pthread_getspecific(key)
  #define NOX_TLS_SET(key, val) pthread_setspecific((key), (val))

  typedef pthread_mutex_t nox_mutex_t;
  #define NOX_MUTEX_INIT(m) pthread_mutex_init((m), NULL)
  #define NOX_MUTEX_LOCK(m) pthread_mutex_lock(m)
  #define NOX_MUTEX_UNLOCK(m) pthread_mutex_unlock(m)
#endif

/* ---------------- core types ---------------- */
typedef struct {
    bool has_err;
    char msg[1024];
} nox_err_state;

typedef struct {
    char *data;   /* always NUL-terminated for C interop convenience */
    int64_t len;
} nox_string;

/* []T: a growable view over a run of elements (data/len/cap). */
typedef struct {
    void *data;
    int64_t len;
