# Racket Namespaces

You write `(eval '(+ 1 2))` and get `3`. Simple. But now ask: where does `eval`
find `+`? In "the environment," sure — but *which* environment? Who decides what
bindings are available? And what happens when you want two independent `eval`
sessions that can't see each other's definitions?

This is the problem namespaces solve.

## The Problem

Most Schemes give you a few canned environments for `eval`:

```scheme
(eval '(+ 1 2) (scheme-report-environment 5))   ; R5RS bindings
(eval '(+ 1 2) (interaction-environment))        ; REPL bindings
```

These work, but they're static and coarse-grained. You can't add bindings, remove
bindings, or control which modules are visible. You get all-or-nothing.

Now imagine you're building a plugin system. Users submit Scheme code that your
application evaluates. You want them to have access to your API functions but *not*
to `delete-file` or `exit`. With R7RS environments, you're stuck. You could create
an `environment` from a custom library — but the user's code can't `define` new
functions or `import` additional modules. The environment is a snapshot, not a
workspace.

Or imagine you're building a REPL. The user types `(define x 10)`, then
`(define y (+ x 5))`. Each evaluation needs to see the results of previous ones.
The evaluations share state. But if you start a second REPL (maybe in a different
thread), it should get its own `x` and `y`, not see the first REPL's definitions.

R7RS has no answer for either of these. The "environment" abstraction is too thin.

## The Key Insight

A Racket *namespace* is a mutable, first-class binding container. It's what an
R7RS "environment" would be if you could:

1. **Create** empty ones or ones pre-loaded with specific module bindings
2. **Mutate** them — `eval` can define new bindings, and they stick
3. **Control** which modules are visible inside them
4. **Isolate** them — two namespaces share nothing by default

Think of it as the difference between a frozen dictionary and a live one. An R7RS
environment is `const dict` — you can read it, but that's all. A namespace is a
full `dict` — you can read, write, delete, and merge.

But namespaces go further than a dictionary of bindings. They also carry a
**module registry**: the set of module declarations and instances available for
`require`. This is the crucial detail. When code inside a namespace says
`(require racket/list)`, the namespace's module registry determines whether that
module is available, and if so, which instance of it to use.

## How It Works

### Creating Namespaces

Racket gives you two constructors:

```scheme
(make-empty-namespace)     ; almost nothing — just the kernel
(make-base-namespace)      ; racket/base bindings pre-loaded
```

`make-empty-namespace` gives you a namespace with only the bare minimum to run
code. `make-base-namespace` gives you the standard library.

### Using Namespaces with `eval`

The `current-namespace` parameter (a Racket parameter, like a dynamically-scoped
variable) controls where `eval` operates:

```scheme
(define ns (make-base-namespace))

(parameterize ([current-namespace ns])
  (eval '(define x 42))       ; defines x in ns
  (eval '(+ x 8)))            ; => 50
```

After this, `x` exists in `ns` but nowhere else. If you create a second namespace
and evaluate `x` there, you get an error — it's unbound.

```scheme
(define ns2 (make-base-namespace))

(parameterize ([current-namespace ns2])
  (eval 'x))    ; error: x is not defined
```

This is how the REPL works. Each REPL session gets its own namespace. Definitions
accumulate in that namespace across evaluations, but don't leak into other sessions.

### Populating Namespaces

You can explicitly add module bindings:

```scheme
(namespace-require 'racket/list ns)    ; make racket/list visible in ns
```

Or read and write individual variables:

```scheme
(namespace-set-variable-value! 'greeting "hello" #f ns)
(namespace-variable-value 'greeting #t #f ns)    ; => "hello"
```

Or list what's bound:

```scheme
(namespace-mapped-symbols ns)    ; => list of all symbols with bindings
```

### The Module Registry

Here's where it gets more interesting than a simple dictionary. When you call
`(namespace-require 'racket/list)`, the namespace needs to know what
`racket/list` *is*. It consults a **module registry** — a table mapping module
paths to module declarations.

A newly created namespace shares the module registry of the namespace that created
it. This is why `make-base-namespace` works: it inherits the registry from the
creating namespace, which knows about `racket/base` and all the modules it
transitively requires.

But a *declaration* is not an *instance*. Even if two namespaces share a registry,
they don't share module instances. If `racket/list` has mutable state (unlikely,
but possible), each namespace gets its own copy of that state.

You can explicitly share module instances between namespaces using
`namespace-attach-module`:

```scheme
(namespace-attach-module source-ns 'some-module dest-ns)
```

This copies the module *instance* (not just the declaration) from one namespace to
another. Now both namespaces see the same module state.

## The Subtle Parts

### Phases

Here's what makes namespaces genuinely harder than a `dict`. In Racket, code lives
at different *phase levels*:

- **Phase 0** — runtime: the functions you call
- **Phase 1** — compile time: the macro transformers that rewrite your code

When `eval` processes `(define-syntax my-mac ...)`, it needs to *compile* the macro
transformer (phase 1) and also register the syntax binding (phase 0). A namespace
must carry bindings at both phases.

This is why namespaces have a `namespace-base-phase` — they know which phase
they're rooted at. And `namespace-require` implicitly brings in phase-0 bindings.
If you want phase-1 bindings (for writing macros in evaluated code), you need:

```scheme
(namespace-require '(for-syntax racket/base))
```

Most users never think about this because `make-base-namespace` sets it all up.
But if you're building a sandbox or a custom evaluator, phase-awareness is the
thing that will bite you.

### Namespace Anchors

There's a problem with namespaces and modules. If you're inside a module and you
call `(make-base-namespace)`, the resulting namespace doesn't automatically see the
bindings from your module. It's a fresh namespace — it knows about `racket/base`,
not about your code.

What if you want a namespace that includes your module's bindings? That's what
namespace anchors are for:

```scheme
(define-namespace-anchor a)

(define ns (namespace-anchor->namespace a))
;; ns now has all bindings from the enclosing module
```

`define-namespace-anchor` captures a reference to the enclosing module's namespace
at definition time. `namespace-anchor->namespace` retrieves it. This is the
standard way for library code to create a namespace for evaluating expressions
that can reference the library's own bindings.

### The Sandbox Pattern

The most common use of namespaces is sandboxing. Racket's `racket/sandbox` module
provides high-level sandboxing, but the core mechanism is:

1. Create a namespace with only the bindings you want to expose
2. Set resource limits (via custodians — see
   [racket-low-level-primitives.md](racket-low-level-primitives.md))
3. Evaluate untrusted code in that namespace

```scheme
(define sandbox-ns (make-base-namespace))
;; Don't add filesystem or system modules
;; Only add safe computation modules
(namespace-require 'racket/math sandbox-ns)

(parameterize ([current-namespace sandbox-ns])
  (eval '(sqr 5)))     ; => 25
  ;; (eval '(delete-file "important.txt"))  ; error: delete-file unbound
```

The untrusted code literally cannot call `delete-file` — the binding doesn't exist
in its namespace. This is stronger than a runtime check: the function isn't hidden
behind a permission system, it *doesn't exist*.

## What Would Break

Without namespaces, `eval` becomes dangerous. Every call to `eval` shares the same
global state. One plugin's `(define x 10)` stomps on another plugin's `x`. There's
no way to limit what evaluated code can access. There's no way to have independent
REPL sessions.

You could try to fake it with R7RS `environment`:

```scheme
(eval '(+ 1 2) (environment '(scheme base)))
```

But `environment` returns a frozen snapshot. `define` inside `eval` doesn't persist.
You can't build a REPL on it — each evaluation forgets everything.

The alternative is what Python does: pass a raw dictionary as the "globals" for
`exec`. This works for variables, but it doesn't handle modules, doesn't handle
compile-time bindings, and doesn't compose with the macro system. Namespaces are the
principled version of that idea, extended to handle Racket's phased module system.

## How This Relates to Wile

Wile has first-class environments with introspection (`environment?`,
`environment-ref`, `environment-bound-names`, `environment-bound?`) and `eval`
accepts an environment argument. This covers the "frozen dictionary" use case.

What Wile *doesn't* have is the mutable, module-registry-carrying namespace that
Racket uses for REPL state accumulation and sandboxed evaluation with controlled
imports. Wile's security model takes a different approach: instead of controlling
what *bindings* are available, it controls what *extensions* are loaded
(`WithExtension()`) and what *operations* are authorized (`security.Authorizer`).
The effect is similar — untrusted code can't call `delete-file` — but the mechanism
is at the Go embedding layer rather than the Scheme namespace layer.

See [racket-low-level-primitives.md](racket-low-level-primitives.md) §3 for the
full primitive reference and Wile feasibility assessment.
