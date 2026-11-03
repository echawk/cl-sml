# Design: Concurrent ML for cl-sml

Status: milestones 1-7 are implemented (system `cl-sml/cml`, `src/cml`;
section 6 says how and where the implementation departs from the plan
below). This file answers which CML to support, how to implement it on
Common Lisp, what it means for the run time and the Coalton backend, how
to test it, and in what order to build it. Sizes use the scale of `docs/ROADMAP.md` (S hours, M
days, L a week or more, XL weeks).

Sources (cited by upstream repository and commit):

- SML/NJ CML: `libraries/cml` in https://github.com/smlnj/smlnj at
  `c0f1f5578f37be8a339f0eb3f5b0ca9226461dd5`.
- MLton's port: `lib/cml` in https://github.com/MLton/mlton at
  `5fe943391e539a502699327a6266b6e8e97939e1`.
- SML/NJ benchmarks (cml-sieve, pingpong): https://github.com/smlnj/benchmarks
  at `75ee8bee6fbd38af68a549bc6ca091f15fb8d3f0`.
- J. Reppy, *Concurrent Programming in ML*, Cambridge University Press,
  1999 (the CML book; the uniprocessor event protocol).
- J. Reppy, C. Russo, Y. Xiao, "Parallel Concurrent ML", ICFP 2009 (the
  multicore protocol).

## 1. Which API

Target the subset that SML/NJ and MLton share. Programs written for
either then run unchanged.

| Structure | Contents | Notes |
|---|---|---|
| `CML` | `VERSION`, `THREAD`, `CHANNEL`, `EVENT`, `TIME_OUT` | the core |
| `SyncVar` | `ivar` (write-once, I-structure) and `mvar` (M-structure); `exception Put` | |
| `Mailbox` | `mbox`: asynchronous, unbounded buffered channel | |
| `Multicast` | `mchan`, `port`, `copy`, `multicast` | written in SML over the core upstream |
| `RunCML` | `doit : (unit -> unit) * Time.time option -> OS.Process.status`, `shutdown : OS.Process.status -> 'a`, `isRunning` | same signature in both |

The core values:

- Threads: `spawn`, `spawnc`, `exit`, `yield`, `getTid`, `sameTid`,
  `compareTid`, `hashTid`, `tidToString`, `joinEvt`, `newThreadProp`,
  `newThreadFlag`.
- Channels: `channel`, `sameChannel`, `send`, `recv`, `sendEvt`,
  `recvEvt`, `sendPoll`, `recvPoll`.
- Events: `never`, `alwaysEvt`, `wrap`, `wrapHandler`, `guard`,
  `withNack`, `choose`, `sync`, `select`.
- Time: `timeOutEvt`, `atTimeEvt`.

Differences between the two that matter here:

- SML/NJ ships a CML-flavoured I/O layer (`TextIO`, `BinIO` and
  `StreamIO` with event-valued input such as `input1Evt` and
  `inputLineEvt`, `OS.IO.ioEvt`, `OS.Process` and `Unix` events,
  sockets) and `RunCML.exportFn`. MLton ships only the core and
  `cml-lib`; its CML has no event-valued I/O. We follow MLton here and
  defer the I/O layer: with OS threads (section 2) an ordinary blocking
  `TextIO` call blocks only its own thread, so most programs do not need
  I/O events.
- MLton's `RunCML.doit` uses the time slice (`Time.time option`) to
  preempt threads with a timer signal. SML/NJ uses it the same way.
  With OS threads we accept and ignore it; the OS preempts.
- MLton's `RunCML.doit` returns `OS.Process.failure` when no thread can
  run and no timeout is pending (deadlock, or all threads finished
  without `shutdown`); `shutdown st` from any thread makes `doit` return
  `st`.
- An uncaught exception ends only its thread; the default handler
  reports it and swallows it (`Thread.doHandler` in MLton's
  `core-cml/thread.sml`).
- Newer SML/NJ adds `Barrier`; `TraceCML` and `SimpleRPC` are in both
  `cml-lib`s. All three are out of scope for a first version.

## 2. Implementation strategies

CML needs two things from the host: a way to run many threads, and a
way to block a thread inside `sync` and resume it when a partner
commits. Upstream gets both from first-class continuations (SML/NJ
`callcc`, MLton `MLton.Thread`). Common Lisp has OS threads, mutexes and
condition variables, but no continuations.

### (a) One OS thread per CML thread

Each `spawn` makes an OS thread. A blocked `sync` waits on a condition
variable. This is the whole of what we need, and it is portable.

Measured on this machine (SBCL 2.6.9, macOS, `sb-thread` directly):

| operation | cost |
|---|---|
| create and join a thread | about 47 µs |
| one ping-pong round trip (mutex and condition variable handoff) | about 3-4 µs |
| live blocked threads before `make-thread` fails | 8190 |

The last row is the macOS per-process limit `kern.num_taskthreads =
8192`. Linux allows more (bounded by `threads-max` and
`vm.max_map_count`), but every thread still costs a control stack
(SBCL's `--control-stack-size`, 2 MB by default, reserved per thread).
SML/NJ and MLton create a CML thread in well under a microsecond and
switch in tens of nanoseconds. So approach (a) is fine for programs
with hundreds or a few thousand threads that compute between
synchronisations. It is slow and may fail for programs that spawn a
thread per datum, such as the prime sieve (12000 live threads for the
12000th prime).

The event protocol. Reppy's uniprocessor protocol (book, chapter 10)
runs `sync` atomically: force the guards, poll every base event, commit
to an enabled one or enqueue a blocked transaction on every channel
involved, then switch away. "Parallel Concurrent ML" replaces atomicity
by per-channel locks, a transaction state word claimed by
compare-and-swap (`WAITING`, `CLAIMED`, `SYNCHED`) and a careful order
of locking, so independent channels synchronise in parallel. We can
take this in two steps:

1. One global CML mutex held during the polling and commit phases of
   `sync` (and around `spawn`, timeouts and `shutdown`). That is the
   uniprocessor protocol with "atomic region" meaning "holding the
   mutex". SML code outside `sync` still runs in parallel on all cores.
   Guards run before taking the mutex; `wrap` functions and handlers run
   after releasing it, in the synchronising thread.
2. If the global mutex shows up as contention, move to the parallel
   protocol: per-channel spin locks and a CAS on the transaction state.
   SBCL has `sb-ext:compare-and-swap` on struct slots; ECL and ABCL
   would use a lock instead.

`choose` with nacks: every `withNack` branch gets a condition variable
("cvar" upstream). When `sync` commits to one branch it sets the cvars
of all other branches' `withNack`s, under the same mutex; the nack
event is an ordinary base event that is enabled once its cvar is set.
Fairness among several enabled branches: pick one at random or round
robin (SML/NJ uses a priority that favours branches not recently
chosen); fairness among threads is the OS scheduler's.

Deadlock and termination: keep, under the mutex, the number of threads
not blocked in `sync`. When it drops to zero and no timeout is pending,
nothing can make progress, so `doit` returns `OS.Process.failure` as
MLton's does. A thread blocked in Lisp I/O counts as running, which is
correct. `shutdown` sets a flag, wakes `doit`, and wakes every blocked
thread so that it unwinds (a non-SML `throw`, so SML handlers do not
catch it). A thread looping without synchronising can only be stopped
with `sb-thread:terminate-thread` or the equivalent, which is
asynchronous; we would do that only at `doit`'s exit and document it.

Timeouts: a blocked `sync` with a time event waits on its condition
variable with a timeout (the earliest deadline among its time events),
and on wake-up re-polls. No timer thread is needed.

Exceptions in threads: SML exceptions are the Lisp condition
`cl-sml::sml-raised-exception`. The thread's top-level function wraps
the SML function in `handler-case` for that condition (and for `error`,
since a Lisp error in a non-main thread otherwise enters the debugger)
and calls the thread's exception handler. An exception raised by a
`wrap` function propagates out of `sync` normally; `wrapHandler`
installs a handler only around the wrap functions, as upstream.

### (b) Green threads on one OS thread

This is how SML/NJ and MLton do it, and it is the only way to get their
thread costs. It needs a continuation for every point where a thread can
block, and CL has none. The compiler would have to produce them:

- Whole-program CPS conversion of the typed IR, in a separate "CML mode"
  of the CL backend. Every SML function, the Basis included, takes a
  continuation; non-tail calls allocate a closure; tail calls must not
  grow the stack, which CL does not guarantee, so a trampoline or
  reliance on SBCL's tail-call merging (not ECL or ABCL). Expect a 2-5x
  slowdown of all code, a second compiled Basis, and Lisp interop
  that is no longer direct (`use{b = ...}` primitives that call back
  into SML must be CPS-aware).
- Selective CPS: convert only functions that may reach `sync`. With
  higher-order functions (`List.app`, `Option.map`) this needs a
  whole-program effect analysis and still converts most of the Basis.
- Coalton: CPS output is ordinary Coalton functions, so it would type
  check, but Coalton does not guarantee tail calls either, and the
  slowdown multiplies with the backend's current 1-6x.

Cost: L for the conversion, plus M for a scheduler in SML or Lisp, plus
the slowdown. Its benefit is the thread count and switch cost.

### (c) Hybrid (M:N)

Green threads from (b) multiplexed over a small pool of OS threads with
the parallel protocol, as Manticore does. It inherits the cost of both;
not worth it unless (b) is already done.

A cheaper variant runs (a) but lets only one SML thread run at a time
(a baton passed at `sync`, `spawn` and `yield`). That makes the run
time trivially safe and the schedule deterministic, but our compiled
code has no safepoints, so a thread that computes without synchronising
starves the others (CML assumes preemption), and it keeps the OS-thread
cost. Not recommended, but useful as a debug mode.

### Recommendation

Build (a): one OS thread per CML thread, the uniprocessor event
protocol under one global CML mutex, written in Lisp in the run time,
with the SML signatures and the derived structures in the Basis. It
reuses CL's threads, preemption and blocking I/O, keeps the compiler
and the Coalton backend unchanged, and runs CML programs with up to a
few thousand threads and real parallelism. Move to the
"Parallel CML" protocol only if the global mutex is measured to be the
bottleneck. Treat green threads (b) as a separate, later decision: they
are a compiler project (CPS) whose cost all code pays, justified only if
thread-per-datum programs matter.

## 3. Fitting into cl-sml

Where it lives:

- Lisp core in a new `src/cml.lisp` (or a section of `src/runtime.lisp`):
  thread records, channels, the event representation (a tree of base
  events after forcing guards), `sync`, cvars, timeouts, `doit` and
  `shutdown`. Primitives are registered in the same alist that maps
  `use{b = NAME}` names to functions (`*sml-env*`'s primitive entries in
  `src/runtime.lisp`, e.g. `("TextIO.openIn" . ,#'sml-text-io-open-in)`).
- SML side in `src/basis/`: `CML-sig.sml`, `CML.sml`, `SYNC_VAR-sig.sml`,
  `SyncVar.sml`, `MAILBOX-sig.sml`, `Mailbox.sml`, `Multicast.sml`,
  `RunCML.sml`, written like the existing structures (abstract types
  over the runtime's objects, values bound with `use{b = "CML.sync"}` and
  so on), and listed in `src/basis/all.sml`. `Multicast` can be ported
  from upstream's SML; `SyncVar` and `Mailbox` are cheaper as Lisp base
  events than as server threads.
- Optional: a separate ASDF system `cl-sml/cml`, so that the core Basis
  stays free of a threading dependency. Open question below.

Thread safety of the run time. The global CML mutex protects only CML
state; SML code in different threads runs in parallel, so:

- `ref`s and arrays are Lisp vectors; unsynchronised access is
  memory-safe in SBCL, ECL and ABCL, and racy programs get the races
  they asked for (as under preemptive CML upstream).
- Global hash tables written at run time must be synchronised:
  `*sml-exception-function-tags*` is written whenever a local
  `exception` declaration is evaluated (`make-sml-exception-function`);
  `*sml-record-shapes*` and `*sml-tuple-shapes*` (the latter is an
  adjustable array) when a shape is first needed. Use SBCL's
  `:synchronized t` hash tables and a lock elsewhere. The tables the
  compiler and REPL use (`*sml-binding-types*`, structure members) are
  written at compile time; compiling from two CML threads at once is not
  supported.
- `TextIO` streams: SBCL's `fd-stream`s are not thread-safe. Each
  output primitive (`TextIO.output`, `print`, `flushOut`) and input
  primitive takes a per-stream lock.
- Special variables: a new thread sees global values, not the creating
  thread's dynamic bindings. `*standard-output*` and friends (bound by
  the REPL and the test harness), `*sml-package*` and the current
  directory must be captured at `spawn` and rebound in the new thread.

Portability:

| Lisp | threads | notes |
|---|---|---|
| SBCL (macOS, Linux) | `sb-thread` | full support; CAS available; macOS limit 8192 threads per process |
| ECL | `mp` | needs a threaded build (the default); condition variables with timeout via `mp:condition-variable-timedwait` |
| ABCL | `threads` (JVM threads) | on JDK 21+ JVM virtual threads would lift the thread-count limit |

Either use bordeaux-threads (a new dependency; cl-sml now depends only
on named-readtables and trivia) or add four or five `#+sbcl #+ecl #+abcl`
wrappers to `src/portability.lisp` (make-thread, mutex, condition
variable with timeout, terminate-thread). The second keeps the
dependency list short and is about 60 lines.

Coalton backend: CML values are run-time objects, like streams, which
the Coalton Basis already represents as `cl_object`. `'a event` and
`'a chan` need parameterised opaque Coalton types (`(repr :native ...)`)
in `src/coalton/prelude.lisp`, with the primitives as `lisp` forms over
the same Lisp core. No change to the backend's code generation.

The two failing real-world programs. `cml-sieve` and `pingpong`
(`t/realworld`, from the SML/NJ benchmarks) do not use the CML
library: `programs/common/cml.sml` defines its own small `CML`
structure (`spawn : (unit -> unit) -> unit`, `yield`, `exit`,
`run : ('a -> 'b) * 'a -> 'b`, `channel`, `send`, `recv`) with
`SMLofNJ.Cont.callcc` and `throw`. CML support alone does not make them
pass. A compat file in `t/realworld/compat` that replaces
`common/cml.sml` by a thin wrapper over our `CML` and `RunCML` would:
pingpong then works (two threads, about 3 µs per message). cml-sieve
needs about 12000 live threads, above macOS's limit, so it fails under
approach (a) on macOS and is slow elsewhere. `SMLofNJ.Cont` itself
(multi-shot `callcc`) cannot be built on OS threads; it needs (b).

## 4. Testing

Upstream tests and examples to port (as SML sources run by a new
`t/cml` suite, not vendored; fetched like the other corpora):

- MLton `lib/cml/tests`: `ping-pong.sml`, `primes.sml`,
  `primes-multicast.sml`, `timeout.sml`, `exit.sml`, `print.sml`
  (each 10-100 lines, driven by `run-main.sml`).
- SML/NJ `libraries/cml/src/tests`: `test.sml` (Barrier tests only if
  we add `Barrier`).
- Examples from the CML book (buffered channels, the sieve, the
  Hamming stream, an RPC server with `withNack`).
- The SML/NJ benchmarks' cml-sieve and pingpong via the compat wrapper
  above (pingpong as a pass; cml-sieve with a smaller `n`).

A small conformance list (each a few lines of SML with expected output):

1. `send`/`recv` rendezvous: neither returns before the other arrives.
2. `sendPoll`/`recvPoll` succeed only with a waiting partner.
3. `choose [recvEvt a, recvEvt b]` commits to exactly one; the other
   message stays available.
4. `wrap` runs after commit, in the synchronising thread; nested wraps
   compose in order.
5. `guard` runs once per `sync`, before polling.
6. `withNack`: the nack event becomes enabled iff another branch is
   chosen; it stays disabled when the branch itself is chosen.
7. `wrapHandler` catches exceptions raised in the wrap functions only.
8. `never` blocks forever; `alwaysEvt v` is always enabled;
   `select []` deadlocks.
9. `timeOutEvt` fires no earlier than its delay; `atTimeEvt` in the past
   fires at once; `choose [timeOutEvt t, recvEvt c]` picks the message
   if one is ready.
10. `joinEvt` becomes enabled when the thread exits, including by an
    uncaught exception, and the exception does not stop other threads.
11. `SyncVar`: second `iPut` raises `Put`; `mTake` empties, `mPut` to a
    full M-var raises `Put`; `mSwap` is atomic.
12. `Mailbox`: `send` never blocks; messages arrive in FIFO order.
13. `Multicast`: every port receives every message after its creation,
    in order; `copy` starts at the original port's position.
14. `RunCML.doit` returns `shutdown`'s status; returns failure on
    deadlock; `isRunning` is false outside `doit`.
15. Stress: 1000 threads in a ring passing a token 100 times; 2 threads
    printing 10000 lines each with no corrupted output.

## 5. Milestones

| # | Milestone | Size |
|---|---|---|
| 1 | Threading layer in `src/portability.lisp` (threads, mutexes, condition variables with timeout, termination) for SBCL, ECL, ABCL; capture of special variables at spawn | S |
| 2 | Run-time thread safety: synchronised global tables, per-stream locks in TextIO/BinIO primitives | S |
| 3 | Core CML in Lisp: threads, channels, events (`sync`, `select`, `choose`, `wrap`, `wrapHandler`, `guard`, `withNack`, `always`, `never`), polls, `RunCML` with deadlock detection and `shutdown`, uncaught exceptions | M |
| 4 | Time events (`timeOutEvt`, `atTimeEvt`) | S |
| 5 | Basis structures `CML`, `RunCML`, `SyncVar`, `Mailbox`, `Multicast` in `src/basis` | S-M |
| 6 | `t/cml` suite: conformance list, ported MLton tests, realworld compat wrapper for pingpong/cml-sieve | M |
| 7 | Coalton backend: opaque event/channel types and primitives in the prelude | S |
| 8 | Optional: "Parallel CML" fine-grained protocol, if the global mutex is a bottleneck | M-L |
| 9 | Optional: CML-flavoured I/O (`TextIO` input events, `OS.IO.ioEvt`) | M |
| 10 | Optional, separate decision: green threads via CPS conversion | XL |

Milestones 1-7 make a usable CML: about two weeks of work.

## 6. The implementation

Decisions (the open questions below): bordeaux-threads, as a dependency
of the separate system `cl-sml/cml` only; the default Basis and the
`cl-sml` system do not change; threads are OS threads for now, behind a
scheduler protocol a green-thread scheduler could implement later;
`TextIO` stays blocking per thread; the choice among enabled events is
random, with a seed (`cl-sml.cml:seed-choice`) for reproducible tests;
the benchmarks' private CML gets a compat layer.

Files:

- `src/cml/scheduler.lisp`: the scheduler protocol and
  `os-thread-scheduler`, its one implementation.
- `src/cml/core.lisp`: threads, events (`force-event` turns an event into
  branches, running guards and `withNack` functions; `sync` polls them in
  the atomic region, commits to a random enabled one or blocks), channels,
  time-outs, `joinEvt`, nack cvars, SyncVar's and Mailbox's base events,
  `RunCML` (deadlock detection, shutdown).
- `src/cml/primitives.lisp`: the `use{b = NAME}` primitives
  (registered in `cl-sml::*sml-extension-primitives*`, which
  `sml-basis-primitive` consults first when it is not empty) and the run
  time's thread safety.
- `src/cml/*.sml`: the signatures and structures. `src/cml/basis.lisp`
  compiles them into the Basis's package (`define-sml-basis-extension`,
  `src/basis.lisp`), and registers them in
  `cl-sml::*sml-basis-extension-files*`, which the native checker and the
  Coalton Basis elaborate after the Basis (before `values.sml`, which
  rebinds `use`).
- `t/cml`: the suite (`cl-sml/cml-tests`, and `cl-sml/cml-coalton-tests`
  for the Coalton backend).

The scheduler protocol (CLOS generic functions on `*scheduler*`):
`call-atomically` (the atomic region: polling and committing),
`make-waiter`, `block-waiter` (leave the atomic region, wait until woken
or a deadline, re-enter), `wake-waiter`, `start-thread` (with the CML
thread that `current-thread` returns in it), `current-thread`,
`yield-thread`, `stop-thread` (for threads that ignore shutdown). The
event protocol uses nothing else of the host. `os-thread-scheduler`
implements it with one global lock, a condition variable per thread, and
bordeaux-threads threads.

Departures from the plan above:

- Thread safety costs a single-threaded program nothing until
  `RunCML.doit` first runs: cl-sml's run time has two hooks,
  `*sml-runtime-lock-hook*` (shape and initial-exception interning) and
  `*sml-stream-lock-hook*` (every TextIO and BinIO primitive and
  `print`), that are NIL until then, when `cl-sml/cml` sets them and
  replaces the exception and constructor tag tables by synchronized
  copies. The tuple-shape vector is replaced, not adjusted, when it grows.
  t/bench is unchanged (within noise) with cl-sml's hooks in place.
- `*inherited-specials*` lists the special variables a new thread takes
  from its parent (the standard streams, `*default-pathname-defaults*`,
  `*sml-package*`, ...). `OS.FileSys.chDir` therefore changes the
  directory of the calling thread only.
- Abstract CML types are `int` in the SML sources (`type obj = int`, as
  the streams are) and `cl_object` in the Coalton Basis (a patch); a
  polymorphic primitive is eta-expanded so that it generalizes. Polls
  take `SOME` and `NONE` from SML, and `RunCML` passes a status as 0 or 1,
  so the primitives never build SML options or statuses, and the Coalton
  backend passes them its own. The backend now converts an SML function
  given to a primitive (`ce-to-cl` on arrow types).
- `RunCML.doit` runs its function as the first CML thread and waits; the
  time slice is ignored. When it finishes (shutdown, deadlock, or every
  thread ended, the last two with `OS.Process.failure`), blocked threads
  unwind at once; threads still running after
  `*shutdown-grace-seconds*` (1 s) are stopped with
  `bt:destroy-thread`. CML operations that need a thread (`sync`, `spawn`,
  `getTid`, `exit`, `shutdown`) raise `Fail` outside `doit`; making
  channels and variables, and `iPut`/`mPut`/`Mailbox.send`, work anywhere.
- An uncaught exception ends its thread and is reported on the error
  output (`*report-uncaught-exceptions*`).
- Multicast is written in SML after Reppy's book (a stream of I-variables
  appended to by a server thread), not ported from upstream.
- Stack sizes: SBCL threads get the main thread's control stack size
  (`--control-stack-size`; `os-thread-scheduler` takes `:stack-size`).
  ECL's threads get a small C stack (about 512 KB on macOS) whatever
  `--c-stack` says, so deep non-tail recursion in a CML thread can crash
  ECL.
- The Coalton backend runs the CML programs of t/cml except `time.sml`,
  which fails on an older backend limitation: an int literal compared with
  a `LargeInt.int` (`Time.toMilliseconds`) is typed as int.

Real-world programs: `t/realworld/compat/cml.sml` replaces the SML/NJ
benchmarks' `common/cml.sml` (`t/realworld/bin/assemble.sh`), so
pingpong and cml-sieve run (their `testit` passes). cml-sieve's
benchmark size (the 12000th prime, a thread per prime) is out of reach on
macOS: the 2000th prime takes about 12 s (2000 threads, each number
handed down the pipeline), and the 12000th exhausts a 1 GB heap before
it meets the 8192-thread limit.

### What a green-thread scheduler needs

A green-thread scheduler implements the same protocol on one OS thread:
`call-atomically` just calls its thunk (nothing else runs),
`make-waiter` makes a slot for a continuation, `block-waiter` saves the
current thread's continuation in the waiter and runs the next ready
thread (returning when someone resumes it), `wake-waiter` puts the
saved continuation on the ready queue, `start-thread` queues a new
thread, `current-thread` is a variable the dispatcher sets, `yield-thread`
requeues the current continuation, and the idle loop sleeps until the
earliest deadline. `core.lisp` needs no change, except that
`block-waiter` returns into a different Lisp stack frame, which is only
possible if the code above it in the stack is in CPS.

From the compiler that means:

1. A CPS mode of the CL backend (or of the IR, before either backend):
   every SML function takes its continuation; `sync`, `spawn`, `yield`,
   `exit` and `RunCML.doit` are primitives that receive it. Code that
   cannot reach them may stay direct (a whole-program effect analysis,
   item 21 of docs/ROADMAP.md, can find it), but higher-order Basis
   functions (`List.app`, `Option.map`) must be compiled in both styles.
2. Constant stack for tail calls in CPS code: a trampoline (portable) or
   SBCL's tail-call merging; the backend's loops for self-tail calls stay.
3. Exceptions as a second continuation (or handlers recorded in the
   thread), since Lisp `handler-case` frames do not survive a switch;
   `wrapHandler` and the handler of a thread's top level follow.
4. A Basis compiled in CPS for the functions that take SML functions,
   and Lisp primitives that call SML (`use{b = ...}` with function
   arguments: the CML primitives themselves) made CPS-aware.
5. Preemption: compiled CPS code polls a flag at function entries or
   loop back edges (a timer sets it), as SML/NJ does, so that a thread
   that computes without synchronising does not starve the others.

The scheduler then plugs in by binding `cl-sml.cml:*scheduler*` to an
instance of its class when a program compiled in CPS mode runs. Both
schedulers could coexist only if `RunCML.doit` chose by how the program
was compiled.

## Open questions

These were answered for the first version: see section 6.

1. Dependency: bordeaux-threads, or our own small wrappers in
   `src/portability.lisp`? (This document leans to our own.)
2. Packaging: CML in the default Basis, or a separate system
   `cl-sml/cml` loaded on demand, so programs that do not use it never
   start a thread or take a lock?
3. Thread count: is "a few thousand threads, microsecond switches"
   acceptable, or are thread-per-datum programs (cml-sieve) a goal? The
   answer decides whether green threads (CPS) are ever worth their cost.
4. Should standard `TextIO` stay blocking per thread (simple, works with
   OS threads), or do we also want SML/NJ's event-valued I/O?
5. Is the realworld compat wrapper for the benchmarks' private `CML`
   acceptable, given that they are meant to test `SMLofNJ.Cont`?
6. Fairness: is OS scheduling plus random choice among enabled branches
   enough, or do we need SML/NJ's priority scheme for reproducible tests?
