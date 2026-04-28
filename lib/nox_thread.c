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
