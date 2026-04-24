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


/* ---------------- string ---------------- */
nox_string nox_string_from_bytes(const char *bytes, int64_t len) {
    char *buf = (char *)NOX_ALLOC(len + 1);
    if (len > 0) memcpy(buf, bytes, len);
    buf[len] = 0;
    nox_string s;
    s.data = buf;
    s.len = len;
    return s;
}

nox_string nox_string_from_cstr(const char *cstr) {
    if (!cstr) return nox_string_from_bytes("", 0);
    return nox_string_from_bytes(cstr, (int64_t)strlen(cstr));
}

nox_string nox_string_concat(nox_string a, nox_string b) {
    int64_t n = a.len + b.len;
    char *buf = (char *)NOX_ALLOC(n + 1);
    if (a.len > 0) memcpy(buf, a.data, a.len);
    if (b.len > 0) memcpy(buf + a.len, b.data, b.len);
    buf[n] = 0;
    nox_string s;
    s.data = buf;
    s.len = n;
    return s;
}

bool nox_string_eq(nox_string a, nox_string b) {
    if (a.len != b.len) return false;
    if (a.len == 0) return true;
    return memcmp(a.data, b.data, (size_t)a.len) == 0;
}

int nox_string_cmp(nox_string a, nox_string b) {
    int64_t n = a.len < b.len ? a.len : b.len;
    int c = n > 0 ? memcmp(a.data, b.data, (size_t)n) : 0;
    if (c != 0) return c;
    if (a.len < b.len) return -1;
    if (a.len > b.len) return 1;
    return 0;
}

bool nox_string_contains(nox_string s, nox_string sub) {
    if (sub.len == 0) return true;
    if (sub.len > s.len) return false;
    return strstr(s.data, sub.data) != NULL;
}

bool nox_string_starts_with(nox_string s, nox_string pre) {
    if (pre.len > s.len) return false;
    return memcmp(s.data, pre.data, (size_t)pre.len) == 0;
}

bool nox_string_ends_with(nox_string s, nox_string suf) {
    if (suf.len > s.len) return false;
    return memcmp(s.data + (s.len - suf.len), suf.data, (size_t)suf.len) == 0;
}

nox_string nox_string_substring(nox_string s, int64_t start, int64_t end) {
    if (start < 0) start = 0;
    if (end > s.len) end = s.len;
    if (end < start) end = start;
    return nox_string_from_bytes(s.data + start, end - start);
}

/* conversions */
nox_string nox_int_to_string(int64_t v) {
    char buf[32];
    int n = snprintf(buf, sizeof(buf), "%lld", (long long)v);
    return nox_string_from_bytes(buf, n);
}
nox_string nox_float_to_string(double v) {
    char buf[64];
    int n = snprintf(buf, sizeof(buf), "%g", v);
    return nox_string_from_bytes(buf, n);
}
nox_string nox_bool_to_string(bool v) {
    return nox_string_from_cstr(v ? "true" : "false");
}
int64_t nox_string_to_int(nox_string s) {
    return (int64_t)strtoll(s.data, NULL, 10);
}
double nox_string_to_float(nox_string s) {
    return strtod(s.data, NULL);
}
bool nox_string_to_bool(nox_string s) {
    return nox_string_eq(s, nox_string_from_cstr("true"));
}
int64_t nox_float_to_int(double v) { return (int64_t)v; }
double nox_int_to_float(int64_t v) { return (double)v; }
bool nox_int_to_bool(int64_t v) { return v != 0; }
bool nox_float_to_bool(double v) { return v != 0.0; }
int64_t nox_bool_to_int(bool v) { return v ? 1 : 0; }
double nox_bool_to_float(bool v) { return v ? 1.0 : 0.0; }

nox_string nox_get_error_message(void) {
    return nox_string_from_cstr(nox_err_state_get()->msg);
}


/* ---------------- dynamic array (type-erased; codegen supplies casts) ---------------- */
nox_slice nox_slice_new(void) {
    nox_slice a;
    a.data = NULL;
    a.len = 0;
    a.cap = 0;
    return a;
}

void nox_slice_reserve(nox_slice *a, int64_t mincap, int64_t elemsize) {
    if (a->cap >= mincap) return;
    int64_t newcap = a->cap < 4 ? 4 : a->cap * 2;
    if (newcap < mincap) newcap = mincap;
    void *nd = NOX_ALLOC(newcap * elemsize);
    if (a->len > 0) memcpy(nd, a->data, (size_t)(a->len * elemsize));
    a->data = nd;
    a->cap = newcap;
}

void nox_slice_push_raw(nox_slice *a, const void *elem, int64_t elemsize) {
    nox_slice_reserve(a, a->len + 1, elemsize);
    memcpy((char *)a->data + a->len * elemsize, elem, (size_t)elemsize);
    a->len++;
}

nox_slice nox_string_split_lines(nox_string s) {
    nox_slice result = nox_slice_new();
    int64_t start = 0;
    for (int64_t i = 0; i < s.len; i++) {
        if (s.data[i] == '\n') {
            int64_t end = i;
            if (end > start && s.data[end - 1] == '\r') end--;
            nox_string line = nox_string_from_bytes(s.data + start, end - start);
            nox_slice_push_raw(&result, &line, sizeof(nox_string));
            start = i + 1;
        }
    }
    if (start < s.len) {
        nox_string line = nox_string_from_bytes(s.data + start, s.len - start);
        nox_slice_push_raw(&result, &line, sizeof(nox_string));
    }
    return result;
}

void nox_slice_pop_raw(nox_slice *a, void *out, int64_t elemsize) {
    if (a->len == 0) nox_panic("pop from empty array");
    a->len--;
    memcpy(out, (char *)a->data + a->len * elemsize, (size_t)elemsize);
