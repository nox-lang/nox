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
    int64_t cap;
} nox_slice;

/* map<K, V>: insertion-ordered hash map (see nox_map.c). */
typedef struct nox_map nox_map;
enum { NOX_KEY_BYTES = 0, NOX_KEY_STRING = 1 };

/* Task<T> and Thread share one worker object (see nox_thread.c). */
typedef struct nox_worker nox_task;
typedef struct nox_worker nox_thread_obj;
typedef void (*nox_body_fn)(void *arg, void *result);

#define NOX_HAS_ERR (nox_err_state_get()->has_err)

/* ---------------- runtime API ---------------- */
extern nox_slice nox_args_array;
void __nox_err_key_make(void);
nox_err_state *nox_err_state_get(void);
void nox_clear_error(void);
void nox_set_error(const char *msg);
void nox_panic(const char *msg);
nox_string nox_string_from_bytes(const char *bytes, int64_t len);
nox_string nox_string_from_cstr(const char *cstr);
nox_string nox_string_concat(nox_string a, nox_string b);
bool nox_string_eq(nox_string a, nox_string b);
int nox_string_cmp(nox_string a, nox_string b);
bool nox_string_contains(nox_string s, nox_string sub);
bool nox_string_starts_with(nox_string s, nox_string pre);
bool nox_string_ends_with(nox_string s, nox_string suf);
nox_string nox_string_substring(nox_string s, int64_t start, int64_t end);
nox_string nox_int_to_string(int64_t v);
nox_string nox_float_to_string(double v);
nox_string nox_bool_to_string(bool v);
int64_t nox_string_to_int(nox_string s);
double nox_string_to_float(nox_string s);
bool nox_string_to_bool(nox_string s);
int64_t nox_float_to_int(double v);
double nox_int_to_float(int64_t v);
bool nox_int_to_bool(int64_t v);
bool nox_float_to_bool(double v);
int64_t nox_bool_to_int(bool v);
double nox_bool_to_float(bool v);
nox_string nox_get_error_message(void);
nox_slice nox_slice_new(void);
void nox_slice_reserve(nox_slice *a, int64_t mincap, int64_t elemsize);
void nox_slice_push_raw(nox_slice *a, const void *elem, int64_t elemsize);
nox_slice nox_string_split_lines(nox_string s);
void nox_slice_pop_raw(nox_slice *a, void *out, int64_t elemsize);
void nox_slice_check_index(nox_slice *a, int64_t i);
void nox_slice_insert_raw(nox_slice *a, int64_t idx, const void *elem, int64_t elemsize);
void nox_slice_remove_raw(nox_slice *a, int64_t idx, int64_t elemsize);
void nox_slice_clear(nox_slice *a);
nox_slice nox_slice_reverse_raw(nox_slice a, int64_t elemsize);

void nox_slice_choice_raw(nox_slice *a, void *out, int64_t elemsize);
int nox_cmp_int_asc(const void *pa, const void *pb);
int nox_cmp_float_asc(const void *pa, const void *pb);
int nox_cmp_string_asc(const void *pa, const void *pb);
int nox_cmp_bool_asc(const void *pa, const void *pb);
void nox_runtime_init(int argc, char **argv);
void nox_print_int(int64_t v);
void nox_print_float(double v);
void nox_print_bool(bool v);
void nox_print_string(nox_string v);
void nox_print_raw_cstr(const char *s);
nox_string nox_io_scanln(void);
nox_string nox_io_scan(void);
int64_t nox_math_abs_i(int64_t v);
double nox_math_abs_f(double v);
int64_t nox_math_min_i(int64_t a, int64_t b);
int64_t nox_math_max_i(int64_t a, int64_t b);
double nox_math_min_f(double a, double b);
double nox_math_max_f(double a, double b);
nox_string nox_fs_read(nox_string path);
void nox_fs_write(nox_string path, nox_string data);
void nox_fs_append(nox_string path, nox_string data);
bool nox_fs_exists(nox_string path);
void nox_fs_remove(nox_string path);
void nox_fs_rename(nox_string oldp, nox_string newp);
void nox_fs_copy(nox_string src, nox_string dst);
void nox_fs_mkdir(nox_string path);
void nox_fs_rmdir(nox_string path);
nox_slice nox_fs_list(nox_string path);
nox_string nox_path_join2(nox_string a, nox_string b);
nox_string nox_path_basename(nox_string p);
nox_string nox_path_dirname(nox_string p);
nox_string nox_path_ext(nox_string p);
nox_string nox_path_stem(nox_string p);
nox_string nox_path_absolute(nox_string p);
int64_t nox_time_now(void);
int64_t nox_time_unix(void);
void nox_time_sleep(double seconds);
double nox_time_clock(void);
int64_t nox_time_year(int64_t t);
int64_t nox_time_month(int64_t t);
int64_t nox_time_day(int64_t t);
int64_t nox_time_hour(int64_t t);
int64_t nox_time_minute(int64_t t);
int64_t nox_time_second(int64_t t);

/* slices */
nox_slice nox_slice_make(int64_t len, int64_t cap, int64_t elemsize);
nox_slice nox_slice_slice(nox_slice s, int64_t lo, int64_t hi, int64_t elemsize);
nox_slice nox_slice_from_raw(const void *data, int64_t len, int64_t elemsize);
