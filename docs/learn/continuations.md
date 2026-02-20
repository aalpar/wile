# Continuations

You're debugging a program. You hit a breakpoint inside some deeply nested function call. Your debugger shows you the call stack — a tower of frames, each one waiting for the frame above it to finish so it can do something with the result. `main` called `processOrder`, which called `validatePayment`, which called `checkBalance`, and here you are.

Now imagine you could reach into the debugger, grab that entire tower of waiting frames, and stuff it into a variable. Not a copy of the data — the actual *waiting*. All those functions, paused mid-thought, ready to resume. And then later, from anywhere in your program, you could hand a value to that variable and watch the whole tower come back to life, each frame picking up exactly where it left off.

That's a continuation.

## The Problem

Let's start with something concrete. Say you're searching a tree for a value, and you want to return the moment you find it — not after the recursion finishes unwinding:

```scheme
(define (find-in-tree tree target)
  (cond
    ((null? tree) #f)
    ((equal? (car tree) target) #t)    ; Found it! But we're deep in recursion...
    (else
      (or (find-in-tree (cadr tree) target)
          (find-in-tree (caddr tree) target)))))
```

This works, but it's doing unnecessary work. When we find the target deep in the left subtree, we still have to unwind through every recursive call, passing `#t` up through a chain of `or` expressions, before the original caller gets the answer. For a balanced tree of depth 20, that's 20 returns doing nothing useful.

In a language like Java or Python, you'd throw an exception to bail out immediately. But that's a hack — you're using the error-handling mechanism for control flow. What if you had a general tool for this?

Now consider a harder problem. You're writing a web server, and a request handler needs to:

1. Ask the user for their name (send a form, wait for response)
2. Ask for their email (send another form, wait again)
3. Use both to create an account

In most languages, step 2 is a problem. After you send the first form, your function *returns* — the HTTP response goes out, the handler's stack frame is gone. When the user submits the form, a *new* request comes in with a *new* call stack. The variable holding the user's name from step 1? Gone with the old stack.

The usual solutions are to stash state in a session object, restructure the code into a state machine, or use callbacks. All of these force you to manually disassemble the natural flow of your logic to accommodate the fact that the call stack is ephemeral.

This is the core tension: **the call stack is the most natural way to express sequential computation, but it's destroyed every time you return.**

## The Key Insight

Here's the idea. At any point during the execution of a program, there is a "rest of the computation" — everything that's waiting to happen with the result of the current expression. Consider:

```scheme
(+ 1 (* 2 3))
```

When the machine is about to compute `(* 2 3)`, the rest of the computation is: "take whatever this produces, add 1 to it, and that's the final answer." That "rest" is a function, conceptually: it takes a value and does something with it. We could write it as:

```scheme
(lambda (v) (+ 1 v))
```

This "rest of the computation" has a name: **the continuation** of the expression `(* 2 3)`.

Every expression has one. In `(display (+ 1 (* 2 3)))`, the continuation of `(* 2 3)` is `(lambda (v) (display (+ 1 v)))`. The continuation of `(+ 1 (* 2 3))` is `(lambda (v) (display v))`. The continuation of the whole `display` call is whatever's next in the program.

So far this is just a way of thinking about evaluation — every language has implicit continuations; they're just the call stack read from bottom to top. The radical move is making them **first-class**: capturing the current continuation as a value you can store, pass around, and invoke later.

The mechanism for this in Scheme is `call-with-current-continuation`, mercifully abbreviated `call/cc`. It takes a function of one argument, and calls that function with the current continuation:

```scheme
(call/cc (lambda (k) ...))
```

Inside the body, `k` is the continuation — the "rest of the computation" that was waiting for `call/cc` to produce a value. If you call `(k 42)`, the program behaves as though `call/cc` returned `42`, and execution continues from there. The crucial part: this works *even if the call to `k` happens much later*, after `call/cc` has already returned normally.

## How It Works

Let's trace through a concrete example. We'll build up to something interesting, starting simple.

**Escape continuation** — bailing out of a computation early:

```scheme
(define (find-first lst pred)
  (call/cc
    (lambda (return)
      (for-each (lambda (x)
                  (when (pred x)
                    (return x)))    ; Jump straight out
                lst)
      #f)))                         ; Only reached if nothing matched

(find-first '(1 2 3 4 5) even?)   ; => 2
```

Here's what happens step by step. When `call/cc` runs, it captures the current continuation — everything waiting for `find-first` to produce a result. It packages that continuation as the function `return` and passes it to the lambda. Inside `for-each`, the moment we find an even number, we call `(return 2)`. This doesn't return from the lambda — it abandons the entire computation in progress (the `for-each`, the `lambda`, all of it) and delivers `2` directly to whatever was waiting for `find-first`.

This is essentially what `throw` does in exception-based languages. But it's not a special mechanism — it's a consequence of first-class continuations. We built exceptions out of a more general tool.

**Saving and re-entering a continuation:**

```scheme
(define saved-k #f)

(define (test)
  (let ((x (call/cc (lambda (k)
                      (set! saved-k k)
                      1))))
    (display x)
    (newline)))

(test)       ; prints 1
(saved-k 42) ; prints 42
(saved-k 99) ; prints 99
```

This is where it gets genuinely strange. The first call to `(test)` runs normally: `call/cc` stashes the continuation in `saved-k` and returns `1`. The `let` binds `x` to `1`, we print it, done.

But `saved-k` is still there. It represents the continuation "bind whatever I receive to `x`, display it, print a newline." When we call `(saved-k 42)`, we jump back into that computation, as if `call/cc` had returned `42` instead of `1`. The `let` binds `x` to `42`, we print it.

We can keep doing this. Every call to `saved-k` re-enters the same point with a new value. The continuation doesn't wear out.

Visualize the state at the moment `call/cc` captures the continuation:

```
  Stack:
  ┌─────────────────────────────┐
  │ top-level: waiting for test │
  ├─────────────────────────────┤
  │ test: waiting for let       │
  ├─────────────────────────────┤
  │ let: waiting for call/cc    │  <── continuation captured here
  └─────────────────────────────┘
```

The continuation is this entire stack, frozen. When you invoke it, you effectively replace the current stack with this one and supply a value.

## The Subtle Parts

There are three things that make continuations genuinely tricky, and it's worth being honest about each.

**1. Continuations are not goto.**

`goto` jumps to a point in the code. A continuation jumps to a point in the *execution* — it restores the entire dynamic context: which functions are waiting, what variables they've bound, what cleanup is pending. It's closer to restoring a saved game than to jumping to a line number.

**2. Multiple returns.**

A function that uses `call/cc` can effectively return more than once. In the example above, `test` "returned" three times — once normally, and twice via `saved-k`. This breaks a fundamental assumption of stack-based languages: that every function call returns exactly once. If your language uses a traditional call stack, you have to do something different to support this — you can't destroy a frame on return if someone might jump back into it.

This is why most implementations of first-class continuations don't use a simple call stack. They use heap-allocated frames, or they copy the stack when capturing a continuation, or they transform the program into a style where continuations are explicit (Continuation-Passing Style, or CPS). Each choice has real performance implications.

**3. Continuations compose with side effects in surprising ways.**

```scheme
(define saved #f)

(let ((x 0))
  (call/cc (lambda (k) (set! saved k)))
  (set! x (+ x 1))
  (display x)
  (newline))

; First run: prints 1
(saved 'ignored) ; prints 2
(saved 'ignored) ; prints 3
```

Each time we re-enter the continuation, the mutation `(set! x (+ x 1))` happens again, but `x` retains its value from previous invocations because the variable lives in a heap-allocated environment, not on the stack. The continuation restores the control flow, but mutable state has moved on. This is the source of most practical confusion with continuations.

## Seeing It In Action

Here's a classic: using continuations to implement cooperative threads. Two computations take turns running, each yielding control to the other.

```scheme
(define (make-coroutines)
  (let ((other-continuation #f))

    (define (yield)
      (call/cc
        (lambda (my-continuation)
          (let ((resume other-continuation))
            (set! other-continuation my-continuation)
            (resume 'go)))))

    (define (start-thread thunk)
      (call/cc
        (lambda (caller)
          (set! other-continuation caller)
          (thunk))))

    (values yield start-thread)))
```

What's happening: `yield` captures its own continuation ("where I am right now"), stores it, and then jumps to wherever the *other* thread left off. Each thread's continuation is a bookmark of where it paused. Calling `yield` swaps which bookmark is active.

No operating system threads. No scheduler. No concurrency primitives. Just two continuations taking turns. The entire threading mechanism is about 10 lines of code built on a single primitive.

## What Would Break

Remove continuations from Scheme and what happens?

You can still write any computable function — continuations don't add computational power. But you lose the ability to express certain control flow patterns *locally*. Without continuations:

- **Exception handling** requires dedicated language support (a `try/catch` mechanism baked into the runtime). With continuations, `raise` and `guard` are library functions.
- **Coroutines** require OS threads or a language-level `yield` keyword. With continuations, they're a pattern.
- **Backtracking search** requires restructuring your code into continuation-passing style manually, or using explicit stacks. With `call/cc`, the language does it for you.
- **Web server session management** requires manual state serialization. With continuations, you can literally suspend a request handler and resume it when the next HTTP request arrives (this was the basis of the Seaside web framework in Smalltalk and Racket's web server).

The pattern: every time you find yourself saving state, dismantling a natural computation into a state machine, and reconstructing the state later to resume — you're manually implementing what continuations give you for free.

> **A note on cost.** First-class continuations aren't free. They constrain how the language runtime manages the call stack, typically making function calls slightly more expensive even when you're not using `call/cc`. This is why most languages don't have them. The languages that do — Scheme, Racket, some Smalltalks — have decided that the expressive power is worth the implementation cost. Many modern languages adopt a compromise: delimited continuations or `async/await`, which provide some of the same patterns with lower overhead. That's a story for another document.
