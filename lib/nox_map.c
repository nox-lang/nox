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
