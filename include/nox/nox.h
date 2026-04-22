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

