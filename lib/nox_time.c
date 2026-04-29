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
}
double nox_time_clock(void) { return (double)clock() / (double)CLOCKS_PER_SEC; }
int64_t nox_time_year(int64_t t) { time_t tt = (time_t)t; struct tm *lt = localtime(&tt); return lt->tm_year + 1900; }
int64_t nox_time_month(int64_t t) { time_t tt = (time_t)t; struct tm *lt = localtime(&tt); return lt->tm_mon + 1; }
int64_t nox_time_day(int64_t t) { time_t tt = (time_t)t; struct tm *lt = localtime(&tt); return lt->tm_mday; }
int64_t nox_time_hour(int64_t t) { time_t tt = (time_t)t; struct tm *lt = localtime(&tt); return lt->tm_hour; }
int64_t nox_time_minute(int64_t t) { time_t tt = (time_t)t; struct tm *lt = localtime(&tt); return lt->tm_min; }
int64_t nox_time_second(int64_t t) { time_t tt = (time_t)t; struct tm *lt = localtime(&tt); return lt->tm_sec; }


