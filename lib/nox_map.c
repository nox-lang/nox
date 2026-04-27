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
