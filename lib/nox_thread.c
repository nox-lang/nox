/* Nox standard library: threads and tasks
 *
 * `Thread` and `Task<T>` are both backed by one "worker" object: a native
 * thread that runs `body(arg, result)`. A Thread is created stopped and
