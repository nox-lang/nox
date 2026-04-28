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

