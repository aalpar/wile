# Racket's Low-Level Primitives: What the Scheme Standard Doesn't Give You

R7RS gives you `call/cc` and... that's about it for low-level control. Racket goes
much further. It exposes hooks into the continuation system, the binding environment,
the macro expander, and the module system that let you build new language features
*from within the language itself*. This document maps the territory.

Each section includes a **Wile Status** assessment: what Wile already has, what could
be added without performance cost, and what would regress existing code.

## 1. Continuation Manipulation

### Beyond `call/cc`

R7RS gives you one continuation primitive: `call-with-current-continuation`. Racket
provides a whole *family*, each with different power and different constraints:

| Primitive | What it captures | Can re-enter? | Composable? |
|-----------|-----------------|---------------|-------------|
| `call-with-escape-continuation` | Up-stack only | No | N/A |
| `call-with-current-continuation` | Full continuation | Yes | No — replaces current |
| `call-with-composable-continuation` | Up to nearest prompt | Yes | Yes — appends to current |
| `call-with-continuation-prompt` | Installs a delimiter | N/A (it's the boundary) | N/A |

Why does this matter? Consider what happens when you invoke a full continuation
captured by `call/cc`: it *replaces* the current continuation entirely. You jumped
somewhere, and whatever you were doing before is gone. Composable continuations
(from `call-with-composable-continuation`) don't replace — they *extend*. The
captured continuation behaves like a function: call it, get a result, keep going.

This is the difference between `goto` and a function call, applied to control flow.

### Prompts and Delimiters

The key insight is that `call-with-continuation-prompt` lets you install *boundaries*.
A composable continuation captures everything between the current point and the
nearest prompt with a matching tag. Without prompts, `call/cc` captures *everything*
up to the top level — which is usually too much.

```scheme
(define tag (make-continuation-prompt-tag))

(call-with-continuation-prompt
  (lambda ()
    (+ 1 (call-with-composable-continuation
            (lambda (k)
              ;; k captures (+ 1 [hole]) — just this one frame
              (k (k 10)))  ; => (+ 1 (+ 1 10)) => 12
            tag)))
  tag)
```

The prompt acts like a fence: the continuation captured inside only reaches up to it.
This is what makes `shift`/`reset` possible — `reset` installs the prompt, `shift`
captures up to it.

### Continuation Barriers

Racket adds another concept R7RS doesn't have: *continuation barriers*. A barrier
prevents continuations from crossing it in certain directions. If you capture a
continuation inside a barrier, you can invoke it — but only from inside the same
barrier. This prevents continuations from escaping security boundaries.

```scheme
(call-with-continuation-barrier
  (lambda ()
    ;; continuations captured here can't be invoked from outside
    ...))
```

Barriers are how Racket ensures that a sandboxed module can't use `call/cc` to
escape the sandbox.

### `call-in-continuation`

There's one more: `call-in-continuation`. Instead of passing a *value* to a
continuation `k`, it lets you call a *procedure* with `k` as the continuation
of that call. The difference matters when the procedure itself does control
effects — you want those effects to happen in `k`'s context, not the current one.

### Wile Status: Continuation Manipulation

Wile's continuation infrastructure is already Racket-aligned. The core delimited
continuation primitives follow Flatt et al. (2007).

**Already implemented:**

| Primitive | Location |
|-----------|----------|
| `call/cc` / `call-with-current-continuation` | `registry/core/prim_control.go` |
| `call-with-composable-continuation` | `registry/core/prim_prompt.go` |
| `call-with-continuation-prompt` | `registry/core/prim_prompt.go` |
| `abort-current-continuation` | `registry/core/prim_prompt.go` |
| `make-continuation-prompt-tag` | `registry/core/prim_prompt.go` |
| `default-continuation-prompt-tag` | `registry/core/prim_prompt.go` |
| `continuation-prompt-tag?` | `registry/core/prim_prompt.go` |
| `continuation?` | `registry/core/prim_cont_marks.go` |
| `call-with-exit` (≈ `call/ec`) | `registry/core/prim_exit.go` |
| `with-continuation-barrier` | Derived form |

**Added in PR #547:**

| Primitive | Location |
|-----------|----------|
| `call-with-escape-continuation` / `call/ec` | `(wile control)` — alias for `call-with-exit` |
| `continuation-prompt-available?` | `registry/core/prim_prompt.go` |
| `shift` / `reset` and all `racket/control` operators | `(wile control)` — Scheme macros over existing primitives |

**Could add without regressing existing code:**

| Primitive | Effort | Notes |
|-----------|--------|-------|
| `call-in-continuation` | Moderate | New VM path: install continuation context, then call procedure. Never invoked by existing code, so no regression. |

#### shift/reset as a Scheme Library

All of the named operators from `racket/control` — `shift`/`reset`,
`prompt`/`control`, `shift0`/`reset0`, `prompt0`/`control0`, `spawn`,
`set`/`cupto`, and their tagged variants — are *derived* from the three
core primitives Wile already has. No Go code needed:

```scheme
;; reset installs a prompt with the default tag
(define-syntax reset
  (syntax-rules ()
    [(_ body ...)
     (call-with-continuation-prompt
       (lambda () body ...)
       (default-continuation-prompt-tag))]))

;; shift captures up to the prompt, then aborts with a handler
(define-syntax shift
  (syntax-rules ()
    [(_ k body ...)
     (call-with-composable-continuation
       (lambda (k)
         (abort-current-continuation
           (default-continuation-prompt-tag)
           (lambda () body ...)))
       (default-continuation-prompt-tag))]))
```

The tagged variants (`reset-at`, `shift-at`, etc.) accept an explicit prompt tag
instead of using the default. `shift0`/`reset0` differ from `shift`/`reset` only
in whether a fresh prompt is installed around the abort handler.

## 2. Continuation Marks (Racket's "Stack Manipulation")

Racket doesn't give you raw stack access — that would break abstraction. Instead
it has something more principled: *continuation marks*.

### The Problem They Solve

Imagine you want to implement a stack trace. You need to attach metadata ("I'm
currently inside function `foo`, called from line 42") to each stack frame. But in
a language with tail calls, stack frames get *reused* — a tail call doesn't push a
new frame, it replaces the current one. So you can't just push annotations onto a
stack; you need a mechanism that's *aware* of tail-call optimization.

Continuation marks are that mechanism. They let you attach key-value pairs to the
*current continuation frame*. When a tail call happens, the mark on the current
frame is *replaced* (not pushed), preserving the tail-call space guarantee.

### The API

```scheme
(with-continuation-mark key value body)
```

This evaluates `body` with the mark `key => value` on the current frame. If there's
already a mark with the same key on this frame (from an enclosing tail-position
`with-continuation-mark`), it's replaced.

To read marks:

```scheme
(current-continuation-marks)              ; snapshot of all marks
(continuation-mark-set->list marks key)   ; extract values for one key
(continuation-mark-set-first marks key)   ; just the nearest one
```

### What They Build

Continuation marks are the implementation substrate for several Racket features
that look like they'd need special runtime support:

- **Parameters** (`make-parameter`, `parameterize`): the parameter value is a mark
  keyed by the parameter object itself. `parameterize` installs marks;
  reading a parameter searches the marks.
- **Exception handlers** (`with-handlers`): the handler is stored as a mark.
  When an exception is raised, the system walks the marks to find a matching handler.
- **Stack traces**: every function application installs a mark with source location.
  `current-continuation-marks` collects them for error reporting.

The beauty is that all three compose correctly with tail calls and continuations,
because marks are part of the continuation model, not a side channel.

### Marks and Captured Continuations

When `call/cc` or `call-with-composable-continuation` captures a continuation,
the marks come along. When the continuation is invoked, the marks are restored.
This means parameters, exception handlers, and stack traces all behave correctly
across continuation jumps — something that's very hard to get right with ad-hoc
implementations.

### Wile Status: Continuation Marks

Wile's continuation marks are fully integrated with the continuation model. Marks
live on `MachineContinuation.marks` (per-frame) and `MachineContext.marks` (current
frame), collected by walking the continuation chain up to a prompt boundary.

Wile already uses marks for `parameterize` (PR #542): the mark key is the parameter
object pointer, the mark value is the pre-converted parameter value. The VM's
`findParameterInMarks` walks marks → continuation chain → parent MC chain.

**Already implemented:**

| Primitive | Location |
|-----------|----------|
| `with-continuation-mark` | Special form; compiler in `machine/compilation/compile_validated.go` |
| `current-continuation-marks` | `registry/core/prim_cont_marks.go` |
| `continuation-marks` (from captured cont) | `registry/core/prim_cont_marks.go` |
| `continuation-mark-set->list` | `registry/core/prim_cont_marks.go` |
| `continuation-mark-set-first` | `registry/core/prim_cont_marks.go` |
| `call-with-immediate-continuation-mark` | `registry/core/prim_cont_marks.go` |
| `continuation-mark-set?` | `registry/core/prim_cont_marks.go` |

**Added in PR #547:**

| Primitive | Location |
|-----------|----------|
| `continuation-mark-set->list*` | `registry/core/prim_cont_marks.go` + `machine/continuation_mark_set.go` |
| `continuation-mark-set->iterator` | `(wile control)` — Scheme closure over `->list*` |
| `continuation-mark-set->context` | `(wile control)` — reads `'wile/source-location` key |

**Performance-negative (avoid):**

| Primitive | Why it costs |
|-----------|-------------|
| Automatic source-location marks | Making `continuation-mark-set->context` produce *useful* stack traces requires installing a mark on **every function call** with source location. Racket amortizes this with a JIT. In Wile's bytecode VM, this would hit every call including hot loops. |

The key distinction: `continuation-mark-set->context` itself is cheap. What's
expensive is making every function call install the marks it would read.

## 3. Binding Manipulation

Racket exposes the binding environment at multiple levels.

### Runtime: Namespaces

A *namespace* is a first-class mapping from identifiers to bindings, scoped by
phase level. You can create them, populate them, and evaluate code inside them:

```scheme
(define ns (make-base-namespace))
(namespace-require 'racket/list ns)   ; add bindings from racket/list
(eval '(first '(a b c)) ns)          ; => 'a
```

This is how Racket implements the REPL, sandboxed evaluation, and dynamic module
loading. Each namespace carries its own set of bindings at each phase level.

### Compile-time: `syntax-local-bind-syntaxes`

Inside a macro transformer, you can *create new bindings* during expansion:

```scheme
(define-syntax (my-form stx)
  (let ([ctx (syntax-local-make-definition-context)])
    (syntax-local-bind-syntaxes
      (list #'x)             ; identifiers to bind
      #'(lambda (stx) ...)   ; transformer expression (or #f for value bindings)
      ctx)                   ; definition context
    ...))
```

This is the mechanism underlying internal definitions. When the expander encounters
a `define-syntax` inside a `let`-body, it calls `syntax-local-bind-syntaxes` to
install the transformer binding so that subsequent forms in the same body can use it.

### Introspection: `identifier-binding`

You can ask "where does this identifier come from?":

```scheme
(identifier-binding #'cons)
;; => (list <module-path> 'cons <nominal-module-path> 'cons 0 0 0)
```

The result tells you: which module defined it, what its original name was (it might
have been renamed on import/export), and at what phase. If the binding is local
(lexical), you get `'lexical`. If unbound, `#f`.

This is how tools like DrRacket's "Check Syntax" draw arrows from uses to definitions
— they call `identifier-binding` on every identifier in the expanded program.

### `syntax-local-value`

Inside a macro, you can look up the compile-time value bound to an identifier:

```scheme
(define-syntax my-info 42)

(define-syntax (use-it stx)
  (let ([val (syntax-local-value #'my-info)])
    ;; val is 42
    ...))
```

This is how Racket implements struct type introspection at compile time, among
other things. `struct` definitions install compile-time records describing the
struct's fields, accessors, and predicate. Other macros can query these records
via `syntax-local-value`.

### Wile Status: Binding Manipulation

Wile has partial coverage. The runtime environment introspection is solid; the
compile-time binding manipulation is where gaps remain.

**Already implemented:**

| Primitive | Category | Location |
|-----------|----------|----------|
| `syntax-local-value` | Compile-time lookup | `registry/core/syntax.go` |
| `make-compile-time-value` | Compile-time values | `registry/core/syntax.go` |
| `syntax-local-introduce` | Scope manipulation | `registry/core/syntax.go` |
| `syntax-local-identifier-as-binding` | Binding preparation | `registry/core/syntax.go` |
| `bound-identifier=?` | Scope comparison | `registry/core/syntax.go` |
| `free-identifier=?` | Binding comparison | `registry/core/syntax.go` |
| `identifier?` | Predicate | `registry/core/syntax.go` |
| `syntax->datum` | Unwrap | `registry/core/syntax.go` |
| `datum->syntax` | Wrap | `registry/core/syntax.go` |
| `generate-temporaries` | Fresh identifiers | `registry/core/syntax.go` |
| `eval` | Runtime eval | `extensions/eval/prim_eval.go` |
| `environment` | Create env from library specs | `extensions/eval/prim_eval.go` |
| `interaction-environment` | REPL env | Introspection extension |
| `environment?` / `environment-bound-names` / `environment-ref` / `environment-bound?` | Env introspection | Introspection extension |

**Added in PR #547 (compile-time):**

| Primitive | Location |
|-----------|----------|
| `syntax-local-value/immediate` | `extensions/eval/prim_eval.go` — identical to `syntax-local-value` (no rename-transformers yet) |

**Could add at zero cost (compile-time only):**

| Primitive | Effort | Notes |
|-----------|--------|-------|
| `identifier-binding` | Moderate | Introspect where an identifier is bound. Wile's scope-set system already tracks provenance. Read-only, zero runtime overhead. |
| `syntax-local-context` | Low | Expose expansion context (`'expression`, `'module`, `'top-level`). Expander already knows this. |
| `syntax-local-phase-level` | Trivial | Wile has 2 phases (0, 1). Return the current one. |
| `syntax-transforming?` | Trivial | Boolean: are we inside a transformer? |

**Added in PR #547 (syntax accessors):**

Source location data already exists on every syntax object via `SourceContext`
(`internal/syntax/source_context.go`). These are now exposed as primitives
in `registry/core/prim_syntax_loc.go`:

| Primitive | Maps to |
|-----------|---------|
| `syntax-source` | `SourceContext.File` |
| `syntax-line` | `SourceContext.Start.Line()` |
| `syntax-column` | `SourceContext.Start.Column()` |
| `syntax-position` | `SourceContext.Start.Index()` |
| `syntax-span` | `SourceContext.End.Index() - SourceContext.Start.Index()` |
| `syntax->list` | Unwrap syntax pair chain to list of syntax objects |

**Needs expander engineering (compile-time only, no runtime cost):**

| Primitive | Effort | Notes |
|-----------|--------|-------|
| `syntax-local-bind-syntaxes` | High | Create bindings during expansion. Requires re-entrant expander. Enables internal `define-syntax` in bodies. |
| `syntax-local-make-definition-context` | High | Paired with `syntax-local-bind-syntaxes`. |
| `local-expand` | High | Let macros trigger sub-expansion. Foundational for contract systems, type checkers. Requires re-entrant expander. |

**Performance-negative:**

| Primitive | Why it costs |
|-----------|-------------|
| `syntax-property` (full system) | Requires adding a property map to every syntax object. Nil-by-default mitigates, but adds a pointer field to all syntax objects during expansion. |

## 4. Compiler Callbacks: Interposition Points

This is where Racket does something no other Scheme does. The macro expander
*automatically wraps* certain syntactic positions in hook forms that a language
can override.

### The `#%` Forms

| Form | What it wraps | Default behavior |
|------|--------------|-----------------|
| `#%app` | Every function application | Ordinary function call |
| `#%datum` | Every literal (numbers, strings, booleans) | Expands to `quote` |
| `#%top` | Every unbound identifier | Top-level variable reference |
| `#%module-begin` | The entire body of a `module` | Allows definitions + expressions |

When you write `(f x)`, the expander silently rewrites it to `(#%app f x)`.
When you write `42`, it becomes `(#%datum . 42)`. These `#%` identifiers are
resolved through the normal binding mechanism — so a module language can provide
its own definitions.

### What This Enables

A lambda-calculus teaching language can redefine `#%app` to restrict function calls
to exactly one argument. A contract system can redefine `#%app` to insert runtime
checks. A logging language can redefine `#%module-begin` to wrap every top-level
expression in a `println`.

```scheme
;; A language where every function call is logged
(module my-lang racket
  (provide (except-out (all-from-out racket) #%app)
           (rename-out [logged-app #%app]))
  (define-syntax (logged-app stx)
    (syntax-case stx ()
      [(_ f arg ...)
       #'(let ([result (#%plain-app f arg ...)])
           (printf "called ~a => ~a\n" 'f result)
           result)])))
```

Any module that says `#lang my-lang` will get logged function calls — without
changing the module's source code at all.

### `local-expand`

Macros can trigger expansion of sub-forms *on demand* via `local-expand`:

```scheme
(local-expand stx 'expression stop-ids)
```

The `stop-ids` list controls expansion depth: the expander stops when it hits a
form whose head is in the stop list. An empty stop list means "expand everything."
A stop list of `#f` means "expand only the outermost macro."

This is how type checkers (like Typed Racket) and contract systems work: they
`local-expand` subforms, inspect the expanded result, insert checks, and then
let expansion continue.

There's also `syntax-local-expand-expression`, which is like `local-expand` but
returns an opaque object that can be substituted without re-expansion — avoiding
the quadratic blowup that naive nested `local-expand` calls would cause.

### Wile Status: Compiler Callbacks

**Already implemented:**

| Primitive | Location |
|-----------|----------|
| `expand` | `extensions/eval/prim_eval.go` |
| `expand-once` | `extensions/eval/prim_eval.go` |

**Performance-negative (avoid):**

| System | Why it costs |
|--------|-------------|
| `#%app` / `#%datum` / `#%top` interposition | Requires the expander to wrap **every** function application, literal, and unbound reference. Even with the default behavior compiling to a no-op, the expansion pass does more work for all code. If user-overridable, every call site becomes potentially indirect. This is the feature that makes Racket a "language laboratory" — but it's architecturally invasive and incompatible with Wile's "Scheme that feels native to Go" goal. |

**Needs expander engineering (compile-time only, no runtime cost):**

| Primitive | Effort | Notes |
|-----------|--------|-------|
| `local-expand` | High | Re-entrant expander needed. Compile-time cost only — generated bytecode runs at the same speed. Highest-leverage single addition for macro power users. |
| `syntax-local-expand-expression` | High | Opaque expansion handle. Avoids quadratic blowup from nested `local-expand`. Would be added alongside `local-expand`. |

The `#%` interposition system is the one Racket feature that fundamentally conflicts
with Wile's performance priorities. It turns every syntactic position into a
potential hook point, which means the expander can never assume "this is just a
function call." Racket amortizes this with a JIT compiler; Wile's bytecode VM cannot.

`local-expand` is the more targeted alternative: macros that need to inspect
expanded code can do so explicitly, without forcing *all* code through interposition.

## 5. Phase Introspection

Racket's phase system is more explicit and more inspectable than any other Scheme's.

### Phase Levels Are Integers

Every binding exists at a specific phase level:

- Phase 0: runtime
- Phase 1: compile time (macro transformers run here)
- Phase 2: compile-compile time (macros used by macros)
- Phase -1: template phase (for `syntax` templates)

When you `(require (for-syntax some-module))`, the bindings from `some-module` are
shifted to phase 1. You can nest: `(for-syntax (for-syntax ...))` shifts to phase 2.

### Introspection Functions

From inside a macro transformer, you can query the current phase:

```scheme
(syntax-local-phase-level)     ; which phase am I expanding in?
(syntax-local-context)         ; 'expression, 'module, 'top-level, etc.
(syntax-local-introduce stx)   ; flip the macro-introduction scope
(syntax-local-module-exports mod-path)  ; what does this module export?
```

`syntax-local-phase-level` returns the integer phase level of the current expansion.
This matters when you're writing macros that generate `begin-for-syntax` blocks or
when you need to pass the right phase to `syntax-local-bind-syntaxes`.

### Module Phase Separation

Racket enforces *phase separation*: code at phase 0 cannot call functions defined at
phase 1, and vice versa. Phases communicate exclusively through the macro expansion
protocol — a macro at phase 1 produces syntax that becomes phase 0 code.

This is a deliberate design constraint. It means you can compile phase 0 code without
having phase 1 code resident in memory (it was only needed during expansion). It also
prevents accidental cross-phase side effects.

### The Macro Debugger

Racket ships a macro stepper (`racket/macro-debugger`) that lets you watch expansion
happen step-by-step, showing which scopes are added, which bindings are resolved, and
which `#%` forms are inserted. It's the ultimate phase introspection tool — you can
see the expander's internal decisions in real time.

### Wile Status: Phase Introspection

Wile has a two-phase system (0 = runtime, 1 = compile-time via `define-for-syntax` /
`begin-for-syntax`). It does not have Racket's arbitrary integer phase levels, nor
does it enforce strict phase separation — phase-1 code can reference phase-0 bindings.

**Already implemented:**

| Primitive | Location |
|-----------|----------|
| `syntax-local-introduce` | `registry/core/syntax.go` |

**Could add at zero cost (compile-time only):**

| Primitive | Effort | Notes |
|-----------|--------|-------|
| `syntax-local-phase-level` | Trivial | Return 0 or 1 based on expansion context. |
| `syntax-local-context` | Low | Expose current context: `'expression`, `'module`, `'top-level`. Expander already tracks this. |
| `syntax-transforming?` | Trivial | Boolean: are we inside a syntax transformer? |
| `syntax-local-name` | Low | Inferred name for the current binding position (e.g., "this lambda is being bound to `foo`"). |

Phase-shifting require forms (`for-syntax`, `for-template`, `for-meta`) are part of
Racket's multi-phase module system. Wile's library system uses `define-for-syntax`
for phase-1 bindings. Full phase-shifting imports would require a multi-phase module
loader — significant architecture work but no runtime cost for phase-0 code.

## 6. Runtime Infrastructure Primitives

Beyond the above, Racket exposes several runtime-level abstractions that most Schemes
leave to the implementation:

- **Custodians**: resource containers. Every thread, port, and network connection
  belongs to a custodian. `(custodian-shutdown-all c)` kills everything inside it.
  This is how Racket implements sandboxed evaluation with guaranteed cleanup.

- **Inspectors**: control structure transparency. By default, structs are opaque —
  you can't see their fields unless you have the accessor functions. An inspector
  grants the ability to break this abstraction and inspect any struct's contents.
  The module system uses inspectors to enforce encapsulation.

- **Security guards**: file and network access control. A security guard intercepts
  every primitive filesystem or network operation and can deny it. Guards are
  hierarchical — a child guard can only *restrict*, never *expand*, the parent's
  permissions.

- **Will executors**: GC-triggered finalizers under program control. Register a
  "will" (a callback) on a value; the will becomes ready to execute when the GC
  proves the value is unreachable. Unlike most finalizer APIs, Racket requires you
  to *explicitly poll* for ready wills — no surprise callbacks during arbitrary code.

### Wile Status: Runtime Infrastructure

Wile's two-layer security model (`security.Authorizer` + extension-level opt-in)
already covers the security guard use case with a more Go-native API.

**Could add without regressing existing code:**

| Primitive | Effort | Notes |
|-----------|--------|-------|
| Will executors | Moderate | Go's `runtime.SetFinalizer` + a ready-queue. Racket's explicit-poll model avoids surprise callbacks. Only code that opts in pays. Niche but clean. |

**Performance-negative:**

| System | Why it costs |
|--------|-------------|
| Custodians | Every resource allocation (port open, thread start) needs custodian registration. Small per-op cost, but on every I/O operation. |
| Inspectors | Every record field access needs inspector-hierarchy checks if opacity is enforced. Per-access cost on a hot path. |

**Already covered by existing architecture:**

| Racket concept | Wile equivalent | Notes |
|----------------|-----------------|-------|
| Security guards | `security.Authorizer` interface | K8s-style Resource + Action vocabulary. Hierarchical: `WithAuthorizer()` engine option. Gate sites at files, system, eval, `include`, library import. |
| Sandboxed eval | `WithProfile(Console)` (or `Tiny`) + `WithAuthorizer()` | Extension-level: unprovided extensions don't exist at compile time. Fine-grained: authorizer gates privileged ops at runtime. |

## The Design Principle

There's a pattern across all of these: Racket doesn't give you raw access to
implementation internals (stack frames, the symbol table, the compiler's AST). Instead,
it provides *principled abstractions* that expose the same power with compositional
guarantees:

- Continuation marks instead of stack inspection
- Namespaces and `identifier-binding` instead of raw symbol tables
- `#%` interposition instead of compiler source hooks
- Custodians instead of `kill -9`

Each abstraction composes with the others. Continuation marks work correctly across
continuations. Custodians respect continuation barriers. `#%` forms participate in
hygiene. This compositionality is what separates Racket from "we expose some C
internals via FFI" approaches — the abstractions are designed to be *safe to combine*.

---

## Wile Feasibility Summary

### Implementation Cost Tiers

**Tier 1 — Free lunch** (alias/wrapper over existing infrastructure, zero cost):

All Tier 1 items are now implemented. Go primitives are in `registry/core/`
and `extensions/eval/`. Derived Scheme forms are in `(wile control)`.

| What | Status | Location |
|------|--------|----------|
| `call/ec` / `call-with-escape-continuation` | ✅ Done | `(wile control)` — alias for `call-with-exit` |
| `shift` / `reset` and all `racket/control` operators | ✅ Done | `(wile control)` — 24 operators + tagged variants |
| `continuation-prompt-available?` | ✅ Done | `registry/core/prim_prompt.go` |
| `continuation-mark-set->list*` | ✅ Done | `registry/core/prim_cont_marks.go` |
| `continuation-mark-set->iterator` | ✅ Done | `(wile control)` — Scheme closure over `->list*` |
| `continuation-mark-set->context` | ✅ Done | `(wile control)` — reads `'wile/source-location` key |
| `syntax-source` / `syntax-line` / `syntax-column` / `syntax-position` / `syntax-span` | ✅ Done | `registry/core/prim_syntax_loc.go` |
| `syntax->list` | ✅ Done | `registry/core/prim_syntax_loc.go` |
| `syntax-local-value/immediate` | ✅ Done | `extensions/eval/prim_eval.go` |

**Tier 2 — Compile-time only** (expander work, zero runtime cost):

| What | Effort | Notes |
|------|--------|-------|
| `syntax-local-context` | Low | Expander already tracks context |
| `syntax-local-phase-level` | Trivial | Return 0 or 1 |
| `syntax-transforming?` | Trivial | Boolean check |
| `syntax-local-name` | Low | Inferred binding name |
| `identifier-binding` | Moderate | Scope-set system already tracks provenance |
| `syntax-local-bind-syntaxes` | High | Requires re-entrant expander |
| `local-expand` | High | Highest-leverage single addition for macro power |

**Tier 3 — Cheap runtime additions** (new primitives, no existing regression):

| What | Effort | Notes |
|------|--------|-------|
| `call-in-continuation` | Moderate | New VM path, never invoked by existing code |
| Will executors | Moderate | `runtime.SetFinalizer` + ready-queue. Opt-in only. |

**Tier 4 — Performance-negative** (would regress existing code):

| What | Why it costs |
|------|-------------|
| `#%app` / `#%datum` / `#%top` interposition | Wraps every application and literal during expansion. Potentially indirect dispatch at every call site. |
| Automatic source-location marks | Mark installation on every function call, including hot loops. |
| Custodians | Per-operation registration cost on all I/O. |
| Inspectors | Per-access checks on record field operations. |
| `syntax-property` (full system) | Pointer field added to all syntax objects. |

### What Unlocks What

| Capability | Required primitives | Cost tier |
|------------|-------------------|-----------|
| Coroutines / generators | `shift`/`reset` | **Done** — `(wile control)` |
| Custom control operators | `call-in-continuation` | **Tier 3** |
| Source location accessors | `syntax-source` etc. | **Done** — `registry/core/prim_syntax_loc.go` |
| Macros that analyze expanded code | `local-expand` | **Tier 2** |
| Contract systems | `local-expand` + `syntax-property` | **Tier 2 + Tier 4** |
| Language-as-library (`#lang`) | `#%` interposition | **Tier 4** |
| Sandboxed eval with cleanup | Custodians | **Tier 4** (security model covers most of this) |
| Stack traces from marks | `continuation-mark-set->context` + auto marks | **Reader done** (`(wile control)`), auto-install is Tier 4 |

---

## Appendix: Complete Primitive Reference

### 1. Continuation Capture & Control

#### Core primitives (§10.4)

| Primitive | Purpose | Wile |
|-----------|---------|------|
| `call-with-current-continuation` (`call/cc`) | Capture full (undelimited) continuation | Yes |
| `call-with-composable-continuation` (`call/comp`) | Capture delimited composable continuation (up to nearest prompt) | Yes |
| `call-with-escape-continuation` (`call/ec`) | Capture escape-only (one-shot, up-stack) continuation | `call-with-exit` |
| `call-with-continuation-prompt` | Install a prompt delimiter | Yes |
| `abort-current-continuation` | Abort to nearest prompt with matching tag | Yes |
| `call-in-continuation` | Call proc with a given continuation as its continuation | No (Tier 3) |
| `call-with-continuation-barrier` | Install a barrier continuations can't cross | Yes |
| `make-continuation-prompt-tag` | Create a fresh prompt tag | Yes |
| `default-continuation-prompt-tag` | The default tag (wraps each REPL interaction, each thread) | Yes |
| `continuation-prompt-tag?` | Predicate | Yes |
| `continuation?` | Predicate — is `v` a captured continuation? | Yes |
| `continuation-prompt-available?` | Is a prompt with this tag on the current continuation? | Yes |

#### `racket/control` — named operators (§10.4)

These are all *derived* from the core primitives above:

| Operator pair | Semantics | Citation | Wile |
|---------------|-----------|----------|------|
| `prompt` / `control` | Felleisen's `F` | Felleisen 1988 | `(wile control)` |
| `reset` / `shift` | Danvy & Filinski | Danvy & Filinski 1990 | `(wile control)` |
| `prompt0` / `control0` | Like `prompt`/`control`, no re-prompt on abort | — | `(wile control)` |
| `reset0` / `shift0` | Like `reset`/`shift`, no re-prompt on abort | — | `(wile control)` |
| `spawn` | Hieb & Dybvig | Hieb & Dybvig 1990 | `(wile control)` |
| `set` / `cupto` | Queinnec & Serpette | Queinnec & Serpette 1991 | `(wile control)` |

Plus tagged variants: `prompt-at`, `reset-at`, `control-at`, `shift-at`, `prompt0-at`,
`reset0-at`, `control0-at`, `shift0-at`, `spawn-at`, `set-at`, `cupto-at`.

And the alias `new-prompt` for `make-continuation-prompt-tag`.

### 2. Continuation Marks (§10.5, §3.19)

| Primitive | Purpose | Wile |
|-----------|---------|------|
| `with-continuation-mark` | Attach key-value mark to current frame (syntax) | Yes |
| `current-continuation-marks` | Snapshot all marks on current continuation | Yes |
| `continuation-marks` | Extract marks from a captured continuation or exn | Yes |
| `continuation-mark-set->list` | All values for a key, ordered innermost-first | Yes |
| `continuation-mark-set->list*` | Multi-key variant (returns vectors) | Yes |
| `continuation-mark-set-first` | Nearest value for a key (amortized O(1)) | Yes |
| `continuation-mark-set->iterator` | Lazy iterator over marks | `(wile control)` |
| `continuation-mark-set?` | Predicate | Yes |
| `continuation-mark-set->context` | Extract stack-trace-style context from marks | `(wile control)` |
| `continuation-mark-key?` | Predicate for impersonated mark keys | No (needs impersonators) |
| `make-continuation-mark-key` | Create an impersonator-friendly mark key | No (needs impersonators) |
| `call-with-immediate-continuation-mark` | Call proc with mark from current frame | Yes |

### 3. Binding Manipulation

#### Namespaces — runtime bindings (§14.1)

| Primitive | Purpose | Wile |
|-----------|---------|------|
| `make-empty-namespace` | Empty namespace (only `#%kernel`) | No |
| `make-base-namespace` | Namespace with `racket/base` bindings | No |
| `namespace?` | Predicate | `environment?` |
| `current-namespace` | Parameter: current namespace | No |
| `namespace-require` | Import module bindings into namespace | No |
| `namespace-variable-value` | Read a variable's value by symbol | `environment-ref` |
| `namespace-set-variable-value!` | Set a variable's value by symbol | No |
| `namespace-mapped-symbols` | List all bound symbols | `environment-bound-names` |
| `eval` | Evaluate in namespace | Yes |
| `environment` | Create environment from library specs | Yes |

#### Compile-time bindings (§12.4)

| Primitive | Purpose | Wile |
|-----------|---------|------|
| `syntax-local-value` | Look up compile-time value of an identifier | Yes |
| `syntax-local-value/immediate` | Like above, no rename-transformer chasing | Yes |
| `syntax-local-bind-syntaxes` | Create bindings in a definition context during expansion | No (Tier 2, high effort) |
| `syntax-local-make-definition-context` | Create an internal-definition context | No (Tier 2, high effort) |
| `identifier-binding` | Introspect: where is this identifier bound? | No (Tier 2) |

#### Syntax object operations (§12.2, §12.3)

| Primitive | Purpose | Wile |
|-----------|---------|------|
| `bound-identifier=?` | Same binding in same scope set? | Yes |
| `free-identifier=?` | Resolve to same binding? | Yes |
| `identifier?` | Is this a syntax object wrapping a symbol? | Yes |
| `syntax-e` | Unwrap one layer of syntax | No (use `syntax->datum`) |
| `syntax->datum` | Fully unwrap (strip all syntax info) | Yes |
| `datum->syntax` | Wrap datum with scopes from a template identifier | Yes |
| `syntax-property` | Get/set syntax properties | No (Tier 4) |
| `syntax-track-origin` | Transfer properties for macro-expansion tracking | No (Tier 4) |
| `syntax-source` / `syntax-line` / `syntax-column` / `syntax-position` / `syntax-span` | Source location accessors | Yes |
| `syntax->list` | Unwrap to list of syntax objects | Yes |
| `generate-temporaries` | Generate unique temporary identifiers | Yes |

### 4. Compiler Callbacks / Interposition

#### `#%` interposition forms (§18.8)

| Form | Inserted where | Wile |
|------|---------------|------|
| `#%app` | Every function application `(f x ...)` | No (Tier 4) |
| `#%datum` | Every literal | No (Tier 4) |
| `#%top` | Every unbound identifier | No (Tier 4) |
| `#%module-begin` | Body of every `module` form | No (Tier 4) |
| All others | Various | No (Tier 4) |

#### Expansion control (§12.4, §12.9)

| Primitive | Purpose | Wile |
|-----------|---------|------|
| `local-expand` | Expand sub-form on demand, with stop list | No (Tier 2, high effort) |
| `syntax-local-expand-expression` | Expand + return opaque handle | No (Tier 2, high effort) |
| `expand` | Fully expand a top-level form | Yes |
| `expand-once` | One expansion step | Yes |

### 5. Phase Introspection (§12.4, §16.2.6)

| Primitive | Purpose | Wile |
|-----------|---------|------|
| `syntax-local-phase-level` | Current phase level (integer) | No (Tier 2, trivial) |
| `syntax-local-context` | Expansion context | No (Tier 2, low effort) |
| `syntax-local-introduce` | Flip macro-introduction scope on syntax | Yes |
| `syntax-local-identifier-as-binding` | Prepare identifier for use as binding | Yes |
| `syntax-local-name` | Inferred name for current expression position | No (Tier 2, low effort) |
| `syntax-local-transforming?` / `syntax-transforming?` | Are we inside a syntax transformer? | No (Tier 2, trivial) |

### 6. Runtime Infrastructure

#### Custodians (§14.7)

All custodian primitives: **No** (Tier 4 — per-operation registration cost).

Wile's security model covers the sandboxing use case via `security.Authorizer` +
extension-level opt-in without per-operation custodian overhead.

#### Security Guards (§14.6)

| Primitive | Wile equivalent |
|-----------|-----------------|
| `make-security-guard` | `security.Authorizer` interface |
| `current-security-guard` | `WithAuthorizer()` engine option |

Already covered by existing architecture with a more Go-native API.

#### Will Executors (§16.3)

All will executor primitives: **No** (Tier 3 — opt-in, no regression).

Implementable via Go's `runtime.SetFinalizer` + explicit-poll queue.

#### Inspectors (§14.9, §14.10)

All inspector primitives: **No** (Tier 4 — per-access check cost on record fields).

### Sources

- [Racket Reference: Continuations (§10.4)](https://docs.racket-lang.org/reference/cont.html)
- [Racket Reference: Continuation Marks (§10.5)](https://docs.racket-lang.org/reference/contmarks.html)
- [Racket Reference: Syntax Transformers (§12.4)](https://docs.racket-lang.org/reference/stxtrans.html)
- [Racket Reference: Syntax Object Bindings (§12.3)](https://docs.racket-lang.org/reference/stxcmp.html)
- [Racket Reference: Namespaces (§14.1)](https://docs.racket-lang.org/reference/Namespaces.html)
- [Racket Reference: Custodians (§14.7)](https://docs.racket-lang.org/reference/custodians.html)
- [Racket Reference: Inspectors (§14.9)](https://docs.racket-lang.org/reference/inspectors.html)
- [Racket Reference: Security Guards (§14.6)](https://docs.racket-lang.org/reference/securityguards.html)
- [Racket Reference: Wills and Executors (§16.3)](https://docs.racket-lang.org/reference/willexecutor.html)
- [Racket Reference: Kernel Forms (§18.8)](https://docs.racket-lang.org/reference/Kernel_Forms_and_Functions.html)
- [Racket Reference: Expanding Top-Level Forms (§12.9)](https://docs.racket-lang.org/reference/Expanding_Top-Level_Forms.html)
- [Racket Guide: Phase Levels (§16.2.6)](https://docs.racket-lang.org/guide/phases.html)
- [Beautiful Racket: Interposition Points](https://beautifulracket.com/explainer/interposition-points.html)
