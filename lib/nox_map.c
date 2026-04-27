/* Nox standard library: map<K, V>
 *
 * An insertion-ordered hash map (like Python's dict): entries live in dense
 * arrays in the order they were inserted, and an open-addressing index table
 * maps hashes to entry numbers. Iteration order is therefore deterministic
 * (insertion order). Removing an entry leaves a hole that is reclaimed the
 * next time the map is rebuilt.
 *
 * Like a Go map, a nox_map is a reference and is not safe for concurrent
 * mutation from several threads without external synchronisation.
 */
#include <nox/nox.h>

struct nox_map {
    int key_kind;      /* NOX_KEY_BYTES or NOX_KEY_STRING */
    int64_t ksize;     /* sizeof(K) */
    int64_t vsize;     /* sizeof(V) */
    char *keys;        /* entry keys, ksize bytes each */
    char *vals;        /* entry values, vsize bytes each */
    unsigned char *live; /* 1 = entry in use, 0 = deleted */
    int64_t *hashes;   /* cached hash of each entry */
    int64_t used;      /* entry slots consumed so far (live + deleted) */
    int64_t len;       /* live entries */
    int64_t cap;       /* entry slots allocated (power of two) */
    int64_t *index;    /* open-addressing table: entry number or -1 */
    int64_t mask;      /* table size - 1 */
};

static uint64_t nox_hash_bytes(const void *p, size_t n) {
    const unsigned char *b = (const unsigned char *)p;
    uint64_t h = 1469598103934665603ULL; /* FNV-1a */
    for (size_t i = 0; i < n; i++) {
        h ^= b[i];
        h *= 1099511628211ULL;
    }
    h ^= h >> 32;
    return h;
}

static uint64_t nox_map_hash(nox_map *m, const void *key) {
    if (m->key_kind == NOX_KEY_STRING) {
        const nox_string *s = (const nox_string *)key;
        return nox_hash_bytes(s->data, (size_t)s->len);
    }
    return nox_hash_bytes(key, (size_t)m->ksize);
}

static bool nox_map_keyeq(nox_map *m, const void *a, const void *b) {
    if (m->key_kind == NOX_KEY_STRING) {
        return nox_string_eq(*(const nox_string *)a, *(const nox_string *)b);
    }
    return memcmp(a, b, (size_t)m->ksize) == 0;
}

static void nox_map_alloc(nox_map *m, int64_t cap) {
    m->cap = cap;
    m->keys = (char *)NOX_ALLOC((size_t)(cap * (m->ksize ? m->ksize : 1)));
    m->vals = (char *)NOX_ALLOC((size_t)(cap * (m->vsize ? m->vsize : 1)));
    m->live = (unsigned char *)NOX_ALLOC((size_t)cap);
    m->hashes = (int64_t *)NOX_ALLOC((size_t)(cap * (int64_t)sizeof(int64_t)));
    int64_t tsize = cap * 2;
    m->index = (int64_t *)NOX_ALLOC((size_t)(tsize * (int64_t)sizeof(int64_t)));
    for (int64_t i = 0; i < tsize; i++) m->index[i] = -1;
    m->mask = tsize - 1;
    m->used = 0;
    m->len = 0;
}

nox_map *nox_map_new(int key_kind, int64_t ksize, int64_t vsize) {
    nox_map *m = (nox_map *)NOX_ALLOC(sizeof(nox_map));
    m->key_kind = key_kind;
    m->ksize = ksize;
    m->vsize = vsize;
    nox_map_alloc(m, 8);
    return m;
}

/* Find the entry number of `key`, or -1. */
static int64_t nox_map_lookup(nox_map *m, const void *key, uint64_t h) {
    int64_t i = (int64_t)(h & (uint64_t)m->mask);
    for (;;) {
        int64_t e = m->index[i];
        if (e < 0) return -1;
        if (m->live[e] && (uint64_t)m->hashes[e] == h && nox_map_keyeq(m, m->keys + e * m->ksize, key)) return e;
        i = (i + 1) & m->mask;
    }
}

static void nox_map_index_insert(nox_map *m, int64_t e) {
    int64_t i = (int64_t)((uint64_t)m->hashes[e] & (uint64_t)m->mask);
    while (m->index[i] >= 0) i = (i + 1) & m->mask;
    m->index[i] = e;
}

/* Rebuild into fresh storage: compacts deleted entries and grows if needed. */
static void nox_map_rebuild(nox_map *m, int64_t newcap) {
    nox_map old = *m;
    nox_map_alloc(m, newcap);
    for (int64_t e = 0; e < old.used; e++) {
        if (!old.live[e]) continue;
        int64_t n = m->used++;
        memcpy(m->keys + n * m->ksize, old.keys + e * m->ksize, (size_t)m->ksize);
        memcpy(m->vals + n * m->vsize, old.vals + e * m->vsize, (size_t)m->vsize);
        m->live[n] = 1;
        m->hashes[n] = old.hashes[e];
        m->len++;
        nox_map_index_insert(m, n);
    }
}

void *nox_map_find(nox_map *m, const void *key) {
    if (m->len == 0) return NULL;
    int64_t e = nox_map_lookup(m, key, nox_map_hash(m, key));
    return e < 0 ? NULL : m->vals + e * m->vsize;
}

