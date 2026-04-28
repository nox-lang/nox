/* Nox standard library: threads and tasks
 *
 * `Thread` and `Task<T>` are both backed by one "worker" object: a native
 * thread that runs `body(arg, result)`. A Thread is created stopped and
 * started explicitly; a Task starts immediately. Waiting is safe to do any
 * number of times and from any number of threads.
 *
 * An error that is still pending when a worker's body returns (an unhandled
 * Nox error) is captured and re-raised in whichever thread waits on it, so
 * errors cross the thread boundary just as they cross a function call.
 */
#include <nox/nox.h>

struct nox_worker {
    nox_thread_t th;
    int state;          /* 0 = created, 1 = running, 2 = finished and joined */
    nox_mutex_t mu;     /* serialises start / join */
    nox_body_fn body;
    void *arg;
    void *result;       /* result slot of `result_size` bytes, or NULL */
    bool has_err;
    char err[256];
};

static NOX_THREAD_FUNC nox_worker_main(nox_thread_arg_t raw) {
    struct nox_worker *w = (struct nox_worker *)raw;
    w->body(w->arg, w->result);
    nox_err_state *st = nox_err_state_get();
    if (st->has_err) {
        w->has_err = true;
        strncpy(w->err, st->msg, sizeof(w->err) - 1);
        w->err[sizeof(w->err) - 1] = 0;
    }
    nox_err_state_free();
    NOX_THREAD_RETURN;
}

static struct nox_worker *nox_worker_new(nox_body_fn body, void *arg, int64_t result_size) {
    struct nox_worker *w = (struct nox_worker *)NOX_ALLOC(sizeof(struct nox_worker));
    NOX_MUTEX_INIT(&w->mu);
    w->body = body;
    w->arg = arg;
    w->result = result_size > 0 ? NOX_ALLOC((size_t)result_size) : NULL;
    return w;
}

/* Returns true on success. */
static bool nox_worker_start(struct nox_worker *w) {
    bool ok = true;
    NOX_MUTEX_LOCK(&w->mu);
    if (w->state != 0) {
        nox_set_error("thread already started");
        ok = false;
    } else if (NOX_THREAD_CREATE(&w->th, nox_worker_main, w)) {
        nox_set_error("could not create a thread");
        ok = false;
    } else {
        w->state = 1;
    }
    NOX_MUTEX_UNLOCK(&w->mu);
    return ok;
}

static void nox_worker_join(struct nox_worker *w) {
    NOX_MUTEX_LOCK(&w->mu);
    if (w->state == 1) {
        NOX_THREAD_JOIN(w->th);
        w->state = 2;
    }
    int state = w->state;
    NOX_MUTEX_UNLOCK(&w->mu);
    if (state == 0) {
        nox_set_error("thread was never started");
        return;
    }
    if (w->has_err) nox_set_error(w->err);
}

/* ---- Task<T> ---- */
nox_task *nox_task_start(nox_body_fn body, void *arg, int64_t result_size) {
    struct nox_worker *w = nox_worker_new(body, arg, result_size);
    nox_worker_start(w);
    return w;
}

void nox_task_wait(nox_task *t) {
    if (!t) { nox_set_error("wait on a task that does not exist"); return; }
    nox_worker_join(t);
}
