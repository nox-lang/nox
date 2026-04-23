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

/* The per-thread error state is reachable only through thread-local storage,
 * which a conservative collector does not scan, so it must NOT live on the
 * collected heap. It is allocated with plain calloc and released by
 * nox_err_state_free() when a Nox worker thread ends. */
nox_err_state *nox_err_state_get(void) {
    nox_err_state *st = (nox_err_state *)NOX_TLS_GET(__nox_err_key);
    if (!st) {
        st = (nox_err_state *)calloc(1, sizeof(nox_err_state));
        if (!st) { fputs("nox: out of memory\n", stderr); exit(1); }
        NOX_TLS_SET(__nox_err_key, st);
    }
    return st;
}

void nox_err_state_free(void) {
    nox_err_state *st = (nox_err_state *)NOX_TLS_GET(__nox_err_key);
    if (st) { NOX_TLS_SET(__nox_err_key, NULL); free(st); }
}

void nox_clear_error(void) {
    nox_err_state_get()->has_err = false;
}

void nox_set_error(const char *msg) {
    nox_err_state *st = nox_err_state_get();
    st->has_err = true;
    if (msg) {
        strncpy(st->msg, msg, sizeof(st->msg) - 1);
        st->msg[sizeof(st->msg) - 1] = 0;
    } else {
        st->msg[0] = 0;
    }
}

void nox_panic(const char *msg) {
    fprintf(stderr, "nox: runtime error: %s\n", msg);
    exit(1);
}

