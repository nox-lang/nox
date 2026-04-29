/* Nox standard library: time package
 * Part of lib/; compiled together with every generated program. */
#include <nox/nox.h>

/* ---------------- time ---------------- */
int64_t nox_time_now(void) { return (int64_t)time(NULL); }
int64_t nox_time_unix(void) { return (int64_t)time(NULL); }
void nox_time_sleep(double seconds) {
#if defined(_WIN32)
    Sleep((DWORD)(seconds * 1000));
#else
    struct timespec ts;
    ts.tv_sec = (time_t)seconds;
    ts.tv_nsec = (long)((seconds - (double)ts.tv_sec) * 1e9);
    nanosleep(&ts, NULL);
#endif
