# Sandboxing Is a Language Property, Not a Feature

*Why embedded scripting sandboxes keep getting bypassed — and what the lambda calculus[^lambda-calculus] got right in 1941.*

---

In 2021, Oracle deprecated Java's SecurityManager for removal (JEP 411). In 2025, JDK 24 permanently disabled it (JEP 486) — every `check` method now unconditionally throws. The mechanism that was supposed to let you run untrusted Java code safely — the one that applets, application servers, and plugin systems relied on. After twenty-five years, Oracle declared it unsalvageable.

Python's story is shorter. The `rexec` module, designed to run restricted Python code, was removed in Python 3.0. The core team's assessment: restricted execution in CPython is fundamentally infeasible. Not just hard — infeasible.

JavaScript fared no better. Google's Caja project, which tried to sandbox third-party JavaScript by translating it into a safe subset, lost active development by 2018; Google archived it in 2021 with known unpatched vulnerabilities and now advises against its use. The successor effort — SES (Secure ECMAScript) — requires *freezing the entire realm* (every built-in prototype, every global) before it can make safety guarantees. The TC39 proposal has sat at Stage 1 since 2020; the work has since fragmented into smaller proposals like Compartments, none past Stage 1.

These aren't implementation failures. They're language failures. Excellent engineers staffed each project. Well-resourced organizations backed them for years. They failed because the languages they were trying to sandbox actively resist containment.

This post is about why. And about what happens when you pick a language that cooperates.

## The pattern: ambient authority

Every failed sandbox above shares a root cause: the language provides **ambient authority** — the ability to reach privileged operations from any point in the code, regardless of what the caller intended.

In Python, any code can call `__import__('os')` to get filesystem access. Even if you delete `os` from the module namespace, `__builtins__` provides a back door. Even if you replace `__builtins__`, `getattr` on the right object chain reaches it again. The language assumes all code is equally trusted, and every restriction has a workaround — because the *introspection that makes Python productive* is the same introspection that defeats sandboxing.

In JavaScript, `globalThis` is reachable from any scope. Prototype chains mean modifying `Object.prototype` affects every object in the realm. `eval` and `Function` can construct arbitrary code at runtime. The proposed fix (SES) requires freezing hundreds of built-in objects to remove ambient authority — a fragile operation that must track every new JavaScript feature.

In Java, reflection — `Class.forName()` and `setAccessible(true)` — bypasses `private` access entirely. The SecurityManager tried to intercept these at runtime with stack-walking permission checks, but the interaction between permissions, class loaders, and the call stack bred constant security bugs.

The pattern is always the same:

1. The language provides a global namespace, reflection, or metaprogramming facility
2. This facility grants ambient authority — access to capabilities the caller didn't provide
3. The sandbox must enumerate and block every path to ambient authority
4. The language adds new features, creating new paths
5. Go to step 3

This is a losing game. You're patching a sieve.

## The alternative: authority is lexical

What if authority weren't ambient? What if a procedure could *only* invoke operations that its caller explicitly provided?

This is how the lambda calculus works — and Scheme[^scheme] is a thin layer over it. In Scheme, a procedure's authority is exactly the set of bindings[^lexical] in its lexical environment. If `open-input-file` isn't bound, no Scheme expression can conjure it. Scheme has no global namespace to reach into, no reflection to bypass scope, no prototype chain to pollute.

```scheme
;; This procedure can read files — it closes over open-input-file
(define (read-config path)
  (let ((port (open-input-file path)))
    (read port)))

;; This procedure cannot — open-input-file is not in scope
(define (compute x)
  (+ x 1))
```

This isn't a security mechanism. It's just how lexical scoping works. But it has a profound consequence: **if you control the environment, you control the authority.**

Jonathan Rees formalized this in 1996 in "A Security Kernel Based on the Lambda Calculus." His insight: a lexically-scoped language needs no added security layer. The scoping rules *already are* a security model. A closure[^closure] captures exactly the bindings it can see — no more, no less. Capabilities are just values in scope.

## From theory to practice

I built [Wile](https://github.com/aalpar/wile), a Scheme interpreter designed for embedding in Go applications. When I implemented sandboxing, it required almost no work.

The entire mechanism is this: you register primitives at engine construction time. If you don't register the filesystem extension, `open-input-file` has no binding. The compiler encounters it as an unbound variable and produces a compile-time error. No runtime checks. No permission callbacks. No stack walking. The capability simply doesn't exist.

```go
// Safe sandbox: only arithmetic, lists, strings, control flow
engine, err := wile.NewEngine(ctx, wile.WithSafeExtensions())

// This produces a compile-time error — open-input-file is unbound
result, err := engine.Eval(ctx, `(open-input-file "/etc/passwd")`)
// err: expand/compile error: no such local or global binding "open-input-file": no such binding
```

In Java, `FileInputStream` exists in every JVM. The SecurityManager intercepts the `open` call at runtime, walks the call stack to check permissions, and allows or denies the operation. Every permission check costs runtime; every new API needs explicit gating; and the interaction between permissions breeds bugs.

In Scheme, the binding either exists or it doesn't. Nothing remains to intercept.

## Five properties that make this work

Lexical scoping alone is not enough. Scheme has a cluster of properties that cooperate with capability security[^capability]. Remove any one and the story weakens.

### 1. No ambient authority

No `globalThis`, no `__builtins__`, no `Class.forName`. The environment is explicitly constructed; every binding has a known origin.

### 2. No mutable dispatch

Scheme lacks something that imperative languages take for granted: **mutable dispatch**. Scheme has no prototypes, no method tables, no class hierarchies. In JavaScript, modifying `Array.prototype.push` affects every array in the program — a single mutation poisons all code that touches arrays. In Python, monkey-patching a class method changes its behavior for every instance. In Java, reflection can replace `private` field values on shared objects.

Scheme has no equivalent. Operations like `car`, `+`, and `open-input-file` are bindings, not methods on mutable objects. You can `set!` a binding in your own scope, but that's local — it doesn't affect closures that already captured the original value. No shared mutable dispatch table exists for an attacker to poison.

This distinction matters for sandboxing: the question isn't "can untrusted code mutate data?" (it can — pairs, vectors). The question is "can untrusted code change what operations *mean*?" In Scheme, it can't. Lexical scope, not mutable object state, determines the authority graph — which bindings exist and what they point to.

### 3. No reflection

Scheme provides no built-in mechanism to access bindings outside the current lexical scope, enumerate an environment's contents, or bypass access restrictions through metaprogramming.

Python has `getattr`, `__dict__`, `inspect`; Java has `java.lang.reflect`; JavaScript has property enumeration, `Proxy`, and `Reflect` — each a path to ambient authority that sandboxes must block.

In Scheme, if a binding isn't in scope, there's no reflective operation to reach it. You can add introspection as an explicit extension (Wile does), but it remains opt-in and read-only — observation without modification.

### 4. Hygienic macros[^hygiene]

Specific to Scheme and underappreciated in security discussions.

Unhygienic macro systems — C's preprocessor, Common Lisp's `defmacro` — can capture bindings from the expansion site. A macro could inadvertently (or deliberately) expose a privileged operation to unauthorized code.

```lisp
;; Common Lisp: unhygienic macro can leak internal bindings
(defmacro with-dangerous-access (&body body)
  `(let ((secret-delete-fn #'delete-file))
     ,@body))

;; User code now has access to delete-file through secret-delete-fn
(with-dangerous-access
  (funcall secret-delete-fn "/important/data"))
```

R7RS Scheme's hygienic macros prevent this. Macro-introduced identifiers resolve in the macro's *definition* environment, not the use site. The same scope-set mechanism enforces both hygiene and sandboxing — consequences of lexical scoping taken seriously.

### 5. Closures are the composition mechanism

In capability systems, the hard problem is **attenuation**: granting partial authority. "You can read files, but only in `/data/`." "You can write to the log, but not to the database."

In most languages, attenuation requires a separate mechanism — a policy language, a permissions framework, a proxy layer. In Scheme, attenuation is just a closure:

```scheme
;; Full authority: write anywhere
(define write-file open-output-file)

;; Attenuated: write only to /tmp/
(define (safe-write path)
  (if (and (>= (string-length path) 5)
           (string=? (substring path 0 5) "/tmp/"))
      (open-output-file path)
      (error "access denied" path)))

;; Pass safe-write to untrusted code instead of open-output-file
(run-untrusted-plugin safe-write)
```

The attenuated capability is a first-class value[^firstclass] — passed, stored, and composed using the same tools as any other Scheme value. No policy DSL to learn, no `Permission` object hierarchy, no XML configuration. The language's composition mechanism — the closure — *is* the security mechanism.

This is the central argument of Mark Miller's 2006 dissertation, "Robust Composition": in a language where authority flows through closures, capability security and software engineering are the same discipline. Writing modular code with clear interfaces *is* writing secure code.

## What this doesn't cover

Lexical sandboxing has limits.

It can't limit CPU time. An infinite loop in sandboxed code runs forever. (Wile handles this through Go's `context.WithTimeout`.)

It can't limit memory allocation. A sandboxed program can allocate until the process runs out of memory. (OS-level limits — cgroups, ulimits — handle this.)

It can't prevent timing side-channels. A sandboxed computation whose duration depends on secret data leaks that data through its runtime.

And it can't prevent capability transfer: if you pass a file handle to sandboxed code, that code can pass it onward. Preventing this requires a full object-capability model with membrane patterns[^membrane] — heavier than what's described here.

These are real limitations. But they're resource-management problems, not authority problems. Every language faces them, and every language solves them the same way: OS-level limits, timeouts, monitoring. The authority problem — "can untrusted code access operations it shouldn't?" — is where language choice matters, and where Scheme has a structural advantage.

## The uncomfortable question

If lexical scoping makes sandboxing tractable, and Scheme has had lexical scoping since 1975, why did we spend thirty years trying to sandbox Java?

Part of the answer is inertia. Java was where the untrusted code was (applets, servlets, plugins), so that's where people tried to build sandboxes. You sandbox what you have, not what you'd choose.

Part of it is that the Scheme community focused on other things — standards, compilers, academic research — and never articulated the security story. Rees wrote the security kernel paper in 1996; it stayed niche.

And part of it is that "just use a different language" is impractical advice for most projects. You can't rewrite your Java application server in Scheme.

But embedded scripting is the exception. When you're choosing a scripting language to embed in your application — for configuration, plugins, extension points, user-defined rules — you *are* choosing the language. And for that use case, a language whose scoping rules double as security boundaries isn't an academic curiosity. It's a practical advantage.

The alternative is adding runtime permission checks, maintaining an allowlist of safe APIs, patching reflection escape hatches, and hoping you didn't miss one. Java tried that for twenty-five years.

## Further reading

- Jonathan Rees, "A Security Kernel Based on the Lambda Calculus" (MIT AI Memo 1564, 1996) — The paper that formalized closures as capabilities.
- Mark S. Miller, "Robust Composition: Towards a Unified Approach to Access Control and Concurrency Control" (PhD dissertation, Johns Hopkins, 2006) — The definitive treatment of object-capability security in programming languages.
- Dennis & Van Horn, "Programming Semantics for Multiprogrammed Computations" (1966) — The original capability model.
- Matthew Flatt, "Binding as Sets of Scopes" (POPL 2016) — The scope-set model that unifies macro hygiene and lexical scoping.
- Mark S. Miller et al., "Secure ECMAScript (SES)" (TC39 Stage 1, stalled since 2020; active work shifted to Compartments proposal) — The effort to retrofit capability security onto JavaScript, illustrating the cost of doing so after the fact.

---

*[Wile](https://github.com/aalpar/wile) is a Scheme interpreter for Go. It compiles Scheme to bytecode and runs it on a stack-based VM, with R7RS-style hygienic macros, first-class continuations[^continuations], and capability-based sandboxing. Pure Go, no CGo, `go get` install.*

---

## Notes

[^lambda-calculus]: **Lambda calculus** is a formal system for expressing computation using only function definition and application. Invented by Alonzo Church in 1936 and published in book form as *The Calculi of Lambda Conversion* (Princeton, 1941), it is the mathematical foundation of all functional programming languages. The core idea: anonymous functions and variable substitution suffice to express any computation — numbers, booleans, loops, data structures all emerge from those two primitives. See Michaelson, *An Introduction to Functional Programming Through Lambda Calculus* (Dover, 2011) for an accessible introduction.

[^scheme]: **Scheme** is a dialect of Lisp designed in 1975 by Guy Steele and Gerald Sussman at MIT. Unlike Common Lisp (the other major Lisp dialect), Scheme emphasizes minimalism: a small core language with powerful abstractions. It was the first language to require both lexical scoping and proper tail calls. The classic introduction is Abelson & Sussman, *Structure and Interpretation of Computer Programs* (MIT Press, 1996), freely available online from MIT Press.

[^lexical]: **Lexical scoping** (also called *static scoping*) means the source text, not the runtime call stack, determines a variable's scope. If function `f` is defined inside function `g`, then `f` can access `g`'s variables — regardless of where `f` is later called. This is how JavaScript, Python, and most modern languages work (as opposed to *dynamic scoping*, where variable lookup follows the call chain at runtime). A **binding** associates a name with a value within a scope — `let x = 5` binds `x` to `5`. The **lexical environment** is the set of all bindings visible at a given point in the source code.

[^closure]: A **closure** is a function that captures the variables from its defining scope and retains them even after that scope exits. In JavaScript: `function makeCounter() { let n = 0; return () => n++; }` — the returned arrow function *closes over* `n`, retaining access to it. Closures exist in JavaScript, Python, Ruby, Swift, Rust, Go, and most modern languages. The term originates from Landin, "The Mechanical Evaluation of Expressions" (1964).

[^capability]: **Capability security** (or *object-capability security*) is a model where access to a resource requires possessing a *capability* — an unforgeable reference to that resource. Unlike access-control lists (ACLs), where a central authority decides who can access what, capabilities travel with the code that uses them. If you have a file handle, you can use it; if you don't, you can't — and there's no way to forge one. The foundational paper is Dennis & Van Horn, "Programming Semantics for Multiprogrammed Computations" (1966). For a modern treatment, see Miller's dissertation cited in Further Reading.

[^hygiene]: **Hygienic macros** are macros that respect lexical scope — they cannot accidentally capture or shadow variables from the code surrounding the macro use site. The term was introduced by Kohlbecker et al. in "Hygienic Macro Expansion" (ACM LFP, 1986). In practical terms: if a macro uses a variable called `x`, and the surrounding code also has an `x`, hygienic expansion keeps the two separate. Unhygienic systems (C's `#define`, Common Lisp's `defmacro`) don't guarantee this separation, leading to subtle bugs when variable names collide.

[^firstclass]: A **first-class value** is any value that can be assigned to a variable, passed as an argument, returned from a function, and stored in a data structure — no restrictions. Numbers and strings are first-class in virtually all languages. In languages with *first-class functions* (JavaScript, Python, Go, Scheme), functions themselves are values you can pass around and store. The significance here: attenuated capabilities are just closures, and closures are first-class, so capabilities compose using the same tools as any other data.

[^membrane]: A **membrane** is a pattern from object-capability security where a wrapper intercepts all access to a target object and can *revoke* that access at any time. Think of it as a proxy with an off switch — once revoked, all references obtained through the membrane die, even those passed to third parties. See Miller, *Robust Composition* (2006), Chapter 9.

[^continuations]: A **continuation** represents "the rest of the computation" from any point in a program's execution. Scheme's `call/cc` (*call-with-current-continuation*) captures this as a value, letting programs save and resume execution contexts — enabling exceptions, coroutines, generators, and backtracking without special language support. The closest mainstream equivalent is a saved call stack that you can jump back into. See Friedman, Wand & Haynes, *Essentials of Programming Languages* (MIT Press, 3rd ed., 2008), Chapter 6.
