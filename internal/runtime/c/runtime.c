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

