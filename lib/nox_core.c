/* Nox standard library: core (errors, strings, slices, runtime start-up)
 * Part of lib/; compiled together with every generated program. */
#include <nox/nox.h>

#if !NOX_USE_GC
void *nox_calloc(size_t sz) {
    void *p = calloc(1, sz ? sz : 1);
    if (!p) { fputs("nox: out of memory\n", stderr); exit(1); }
    return p;
}
#endif

/* ---------------- error propagation state (per-thread) ---------------- */
static nox_tls_key_t __nox_err_key;

/* Created once, eagerly, from nox_runtime_init() before any thread other
 * than the main one exists — this sidesteps needing a once-only-init
 * primitive (pthread_once has no simple portable equivalent on Windows). */
void __nox_err_key_make(void) { NOX_TLS_CREATE(&__nox_err_key); }
