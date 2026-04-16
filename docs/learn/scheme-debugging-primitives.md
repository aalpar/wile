# The Load-Bearing Primitives of Scheme Debugging

You want to add debugging to a Scheme implementation. Where do you start?

Not with a debugger. Not with `trace`. Not with a stepping UI. Those are the
*last* things you build. They're leaves on a tree, and right now you need roots.

The question is: what are the roots? What are the 3-5 primitive mechanisms that
*every* debugging tool ends up depending on -- the ones where, if you pulled any
single one out, half the debugging ecosystem collapses?

Racket and Chez Scheme give two very different answers to this question. Studying
both reveals which concerns are fundamental and which are design choices.

## The Problem

Scheme is hostile to debugging in ways that C or Java are not.

First, **proper tail calls erase the stack.** A tail-recursive loop that runs for
a million iterations has a stack depth of one. If something goes wrong on iteration
999,999, the call history is gone. In C, you'd scroll through a backtrace. In
Scheme, the backtrace shows you one frame.

Second, **macros rewrite your code.** The program you wrote and the program that
runs are different. When an error occurs at line 42, does "line 42" refer to your
source, or to the expanded form? If a macro introduced the faulty code, who's to
blame -- the macro author or the macro user?

Third, **continuations are first-class.** Control flow can jump anywhere. The
"call stack" isn't really a stack anymore -- it's a tree (or worse, a DAG). A
debugger that assumes linear control flow will produce nonsense.

Any debugging system for Scheme must solve all three problems. Let's see how
Racket and Chez approach them.

## Racket's Answer: One Primitive to Rule Them All

Racket's debugging infrastructure is a pyramid, and at the apex sits a single
primitive: `with-continuation-mark`.

### What continuation marks are

Imagine you could tape a sticky note to the current stack frame. The note has a
key and a value. As execution continues, you can tape more notes to more frames.
At any point, you can ask: "show me all the sticky notes with key K, from the
top of the stack down to some delimiter."

That's `with-continuation-mark`. The syntax is:

```scheme
(with-continuation-mark key value
  body)
```

This attaches `key -> value` to the current frame, then evaluates `body` in tail
position. One crucial rule: if the frame *already* has a note for that key, the
old note is *replaced*, not stacked. This is what makes marks compatible with
tail calls -- a tail-recursive loop that marks every iteration uses O(1) mark
space, because each iteration replaces the previous mark on the same frame.

To read marks:

```scheme
(current-continuation-marks)               ; snapshot of all marks
(continuation-mark-set->list marks key)    ; all values for key, top-down
(continuation-mark-set-first marks key)    ; innermost value, amortized O(1)
```

### What depends on marks

Here's what breaks if you remove `with-continuation-mark` from Racket:

| System | Why it breaks |
|--------|---------------|
| **Stack traces** | Exception objects carry `(current-continuation-marks)` captured at the raise site. `continuation-mark-set->context` walks these to produce `(name . srcloc)` pairs. No marks, no traces. |
| **`parameterize`** | Parameters are *implemented* via continuation marks + thread cells. Remove marks and `current-output-port`, `current-error-port`, every parameter stops working. |
| **`with-handlers`** | The exception handler chain is stored as continuation marks with an internal key. `raise` walks the marks to find matching handlers. |
| **errortrace** | Wraps every expression with `(with-continuation-mark errortrace-key srcloc expr)`. On exception, reads the marks to produce expression-level stack traces. |
| **DrRacket debugger** | Breakpoints, stepping, local variable display -- all stored as marks on the continuation. |
| **Algebraic stepper** | Records execution state in marks to reconstruct source-level reduction steps. |
| **Statistical profiler** | A sampler thread periodically reads `continuation-mark-set->context` from the target thread. |
| **Contract blame** | `contract-continuation-mark-key` tracks which contract is being checked, so violations report the right blame party. |

This is extraordinary. One primitive -- a way to annotate stack frames with
metadata -- is the foundation for stack traces, dynamic binding, exception
handling, profiling, debugging, stepping, and contract blame. Continuation marks
are to Racket's debugging infrastructure what the program counter is to a CPU.

### Why this works

The power comes from three properties:

1. **Frame-local replacement.** Marks don't accumulate on tail calls. This means
   they're the *only* stack-annotation mechanism compatible with proper tail
   calls -- the exact problem that makes Scheme debugging hard.

2. **Key-value generality.** Different systems use different keys and never
   interfere with each other. The error handler uses one key, errortrace uses
   another, the debugger uses a third.

3. **Cross-thread visibility.** You can read marks from another thread's
   continuation, enabling sampling profilers without instrumentation overhead
   in the profiled thread.

Clements and Felleisen proved (ESOP 2001) that continuation marks provide
stack inspection without violating proper tail calls -- a result that was
believed impossible before their work.

### The second pillar: source locations on syntax objects

Racket's syntax objects carry five source-location fields: source, line, column,
position, and span. These are populated by the reader and flow through macro
expansion because pattern variables carry their source locations with them.

This matters because marks without locations are just anonymous data. Errortrace
annotates expressions with `(with-continuation-mark errortrace-key srcloc expr)`,
where `srcloc` comes from `(syntax-source stx)`, `(syntax-line stx)`, etc. If
syntax objects didn't carry locations, errortrace would have nothing to attach
to its marks.

What breaks without source locations:

- Error messages lose file/line/column (every error says "unknown location")
- Errortrace's `should-annotate?` filter returns false everywhere -- it becomes a no-op
- The profiler's output loses location data (procedure names only)
- The macro stepper can't show correspondence between expansion steps and source

The tools still *run* without source locations. They just produce useless output.
Marks give you the *mechanism*; source locations give you the *meaning*.

### Supporting infrastructure

Two more primitives complete the picture:

**Delimited continuations** (`call-with-continuation-prompt`): Mark queries stop
at prompt boundaries. Exception handling uses `abort-current-continuation` to
escape to handler prompts. Without prompts, marks would be unbounded (walk the
entire continuation) and error recovery would need a different mechanism.

**Inspectors** (`make-inspector`, `current-inspector`): A hierarchy controlling
access to struct internals. A sandbox runs under a sub-inspector, preventing it
from seeing the host's struct fields. Not strictly debugging, but essential for
*safe* debugging -- running untrusted code and inspecting its behavior without
it inspecting yours.

## Chez Scheme's Answer: Compiler Metadata + Raw Inspection

Chez takes a fundamentally different approach. Where Racket invented a single
universal mechanism (marks), Chez exposes the compiler's internal metadata and
provides low-level primitives to inspect runtime objects directly.

### The `code-info` record: compiler metadata that survives

When Chez compiles a procedure, it can optionally embed a `code-info` record
in the resulting code object. This record contains:

| Field | Content |
|-------|---------|
| `src` | Source object (file + byte position) |
| `sexpr` | Original S-expression of the procedure body |
| `free` | Vector of free variable names |
| `rpis` | Return-point information: for each return address, its source location, livemask, and local variable names |
| `live` | Variable-name-to-slot mappings |

This is the join point where debugging, inspection, and profiling all meet.
The compiler produces it; everything else consumes it.

The master switch is `generate-inspector-information` (default: `#t`). When `#f`,
`code-info` records are empty and the inspector degrades to showing raw
addresses. The critical design insight: **this has zero runtime performance cost.**
The same machine code runs either way. The cost is only memory and file size
for the metadata tables.

### Raw continuation inspection

Chez exposes the continuation as a linked list of frames, each accessible through
internal primitives:

```
$continuation-return-code     ; code object at this frame
$continuation-return-offset   ; byte offset within the code object
$continuation-return-livemask ; which stack slots are live
$continuation-stack-length    ; frame size
$continuation-stack-ref       ; read a specific stack slot
$continuation-link            ; next frame
$split-continuation           ; prepare for frame-by-frame traversal
```

These are the building blocks. The inspector and debugger are built on top:

1. `$split-continuation` prepares the captured continuation for traversal
2. At each frame, `$continuation-return-code` gets the code object
3. `$code-info` extracts the metadata
4. The return offset indexes into the `rpis` vector to find the return point
5. The livemask identifies which slots contain values (vs. garbage)
6. `code-info-live` maps slot positions to variable names
7. `$continuation-stack-ref` reads the actual values

This is lower-level than Racket's approach. Racket's marks are a designed
abstraction; Chez's continuation inspection is the compiler's internal
representation, exposed with a thin API.

### Source tracking pipeline

Chez tracks source locations through a three-layer system:

**Source file descriptors** (`make-source-file-descriptor`): Identify a file
with a path and checksum. The checksum detects when source has been modified
since compilation.

**Source objects** (`make-source-object`): A specific span within a file,
identified by beginning/ending byte positions and optional line/column.

**Annotations** (`make-annotation`): Wrap S-expressions with source objects.
The reader produces annotations; the expander and compiler consume them.
Two flag bits -- `annotation-debug` and `annotation-profile` -- control whether
a given expression gets debugging or profiling instrumentation.

### How trace works (no marks needed)

Chez's `trace` predates continuation marks. It uses a different trick: closure
wrapping with continuation comparison for tail-call detection.

```
1. Wrap the original procedure in a new closure
2. On entry, capture the current continuation via call/1cc
3. Compare it to a thread-local "trace continuation" variable
4. If same: this is a tail call (same depth, don't indent)
5. If different: this is a nested call (indent deeper)
6. After the call, display return values
```

The tail-call detection is elegant: if the continuation at the call site is
the same as the continuation of the traced wrapper, then the call is in tail
position -- the caller's frame was already replaced.

### Post-mortem debugging via `&continuation`

When Chez raises an exception, the default handler captures the current
continuation and wraps it in a `&continuation` condition (Chez's extension to
R6RS conditions). This continuation is saved in `debug-condition` (a thread
parameter).

The `debug` procedure reads `debug-condition`, extracts the continuation, and
passes it to the inspector for frame-by-frame traversal. This gives you post-mortem
debugging: after an error, type `(debug)` at the REPL and walk the stack at the
point of failure, examining local variables, source locations, and the full
call chain.

The key requirement: continuations must be *inspectable objects*, not just
invocable procedures. Chez's `$continuation-*` primitives provide this.

## The Comparison: Where They Converge

Despite radically different strategies, both systems solve the same fundamental
problems. The convergence points reveal what's truly necessary:

### 1. Stack inspection compatible with tail calls

This is the hardest problem, and both systems solve it differently:

- **Racket**: Continuation marks with frame-local replacement. O(1) space per
  tail-recursive iteration. A *designed* abstraction.
- **Chez**: Raw continuation frame inspection via `$continuation-*` primitives,
  plus compiler metadata (`code-info`) to give meaning to raw slots. The compiler's
  *internal representation*, exposed.

Both work. Racket's approach is more portable and composable (any code can set
marks; you don't need compiler cooperation). Chez's approach is more efficient
for inspection (no runtime overhead for marks) but less flexible (you can only
inspect what the compiler decided to record).

### 2. Source location propagation through compilation

Both systems must answer: when compiled code raises an error, where in the
source did it come from?

- **Racket**: Source locations on syntax objects (5 fields per syntax datum).
  Survive macro expansion because pattern variables carry locations. Used by
  errortrace marks and error reporting.
- **Chez**: Annotation objects wrapping S-expressions with source objects.
  Tracked through expansion and compilation into `code-info.src` and
  `code-info.rpis[n].src`. Used by the inspector and profiler.

Same problem, same shape of solution, different representation. The essential
insight in both: **source locations must be metadata on the representation**
(syntax objects / annotations), not a side table. Side tables get out of sync
during macro expansion.

### 3. Continuation capture for post-mortem debugging

Both systems capture the continuation at the error site:

- **Racket**: `(current-continuation-marks)` in `exn-continuation-marks`
- **Chez**: `(make-continuation-condition k)` in the `&continuation` condition

The captured object is what `debug` (Chez) or the DrRacket stack display (Racket)
walks to reconstruct the error context.

### 4. Structured error information

Both have hierarchical exception/condition types carrying who, what, where, and
context. This isn't glamorous, but it's load-bearing: without structured errors,
debuggers have nothing to work with except a string message.

## The Load-Bearing Functions

Across both systems, here are the irreducible primitives. Every debugger, tracer,
profiler, and stepper ultimately depends on some subset of these:

### Tier 1: Remove any one and most debugging breaks

| Function | Racket | Chez | What it provides |
|----------|--------|------|------------------|
| **Stack/continuation annotation** | `with-continuation-mark` | `code-info` + `$continuation-*` | Attach metadata to stack frames; read it back |
| **Source location on AST nodes** | `syntax-source/line/column/position/span` | `make-source-object` + `make-annotation` | Track where code came from through all transformations |
| **Continuation capture at error** | `exn-continuation-marks` | `&continuation` condition type | Preserve the stack state at the moment of failure |

### Tier 2: Remove any one and specific tools break

| Function | Racket | Chez | What it provides |
|----------|--------|------|------------------|
| **Delimited continuations** | `call-with-continuation-prompt` | `call/1cc` + `$split-continuation` | Bound mark queries; enable error recovery; frame traversal |
| **Struct/record transparency** | Inspectors (`make-inspector`) | `$record-type-descriptor` + RTD fields | Control who can see inside opaque values |
| **Timer/preemption** | Thread scheduling | `set-timer` + `timer-interrupt-handler` | Statistical profiling; engine-based timeouts |
| **Dynamic binding** | `parameterize` (built on marks) | `fluid-let` / parameters | Configure debugging output, handlers, format |

### Tier 3: Convenience built on the above

| Function | Racket | Chez | Built on |
|----------|--------|------|----------|
| `trace` | Procedure wrapping | Closure wrapping + continuation comparison | Tier 2 (dynamic binding) |
| `errortrace` | Compile handler + marks | N/A (no equivalent) | Tier 1 (marks + source locs) |
| `debug` | DrRacket GUI | Interactive inspector on `debug-condition` | Tier 1 (capture + annotation) |
| `profile` | Sampling thread | Compiler instrumentation + counters | Tier 1 (source locs) + Tier 2 (timer) |
| Inspector | `struct->vector`, `struct-info` | `inspect/object` message dispatch | Tier 2 (transparency) |

## What Would Break

This is the verification question. For each Tier 1 primitive, what specific
failure would a user observe if it were missing?

**Without stack annotation** (no marks, no `code-info`):

```
> (define (f x) (/ 1 x))
> (define (g) (f 0))
> (g)
Error: division by zero
```

That's it. No stack trace. No "in f, called from g." No local variables. Just the
error message. The user has no idea *where* in their program the error occurred.

**Without source locations:**

```
> (g)
Error: division by zero
  in: f
  called from: g
  at: unknown:0:0
  at: unknown:0:0
```

You know the call chain but not where in the source to look. For a 1000-line
program, this is nearly useless.

**Without continuation capture at error:**

```
> (g)
Error: division by zero
  in: f at foo.scm:1:19
  called from: g at foo.scm:2:14
> (debug)
Error: no continuation available
```

You see the error and its location, but you can't inspect local variables or
explore the state at the moment of failure. The error is a photograph; you wanted
a crime scene you could walk through.

## The Subtle Parts

### Racket's bet: one mechanism, many uses

Continuation marks aren't just for debugging. They *are* the implementation of
`parameterize`, `with-handlers`, and contract blame tracking. This means Racket
can't have "debugging off" the way Chez can turn off `generate-inspector-information`.
Marks are always on because the language depends on them for basic semantics.

The upside: you get uniform behavior. Any tool can set marks, and any other tool
can read them. The contract system and the debugger compose naturally because
they use the same mechanism.

The downside: marks have runtime cost. Every `with-continuation-mark` allocates
and installs metadata. Errortrace wraps *every* expression, causing 2-3x slowdown.

### Chez's bet: compiler metadata, zero runtime cost

`generate-inspector-information` costs memory but not time. The generated machine
code is identical whether or not inspector information exists. This is possible
because Chez's approach is *passive* -- the compiler records metadata about the
code it was going to generate anyway.

The upside: debugging support is free at runtime. Production code runs at full
speed with full debugging capability.

The downside: you can only inspect what the compiler decided to record. Custom
metadata (like Racket's contract-blame marks or feature-specific profiling) has
no natural home. And aggressive optimization (`cp0`) can make the recorded
metadata misleading -- it refers to source expressions that were transformed away.

### The tail-call detection trick

Chez's `trace` detects tail calls by comparing continuations:

```scheme
(define trace-k #f)

(define (traced-f . args)
  (let ((k (call/1cc (lambda (k) k))))
    (if (eq? k trace-k)
        (display "tail call")   ; same continuation = tail position
        (begin
          (set! trace-k k)
          (display "nested call")))))
```

If the continuation at the call site equals the saved continuation, the call
is in tail position. This is a cheaper mechanism than marks for this specific
purpose, but it only works for trace -- you can't compose it with other tools.

## What This Means for an Implementor

If you're building a Scheme and want debugging, here's the priority order:

1. **Source location propagation.** Get this right first. Attach locations to your
   AST nodes (syntax objects, annotation wrappers, whatever you have). Make sure
   they survive macro expansion. Make sure your compiler can emit them alongside
   bytecode. Without this, every other tool produces "at unknown:0:0."

2. **Continuation capture at error sites.** When you raise an exception, save the
   continuation (or enough of it to walk later). Attach it to the condition/exception
   object. Without this, post-mortem debugging is impossible.

3. **Stack annotation / inspection.** This is where Racket and Chez diverge, and
   you must choose:
   - **Marks-style** (Racket): More flexible, composable, works for dynamic
     binding. But always-on cost, and you need marks for `parameterize` and
     `with-handlers` too.
   - **Compiler-metadata-style** (Chez): Zero runtime cost, but less flexible.
     Requires your compiler to emit `code-info`-like records with variable names,
     source locations per return point, and liveness information.
   - **Or both.** Chez added continuation marks later (they call them "continuation
     attachments"). They use compiler metadata for the inspector and marks for
     `parameterize`.

4. **Structured exceptions/conditions.** Typed error hierarchy with who/what/where
   fields. The debugger needs to discriminate error kinds, and "everything is a
   string" doesn't cut it.

5. **Everything else** -- `trace`, inspector, profiler, stepper -- is built on top
   of these four. The specific user-facing tools are design choices; the foundations
   are engineering constraints.
