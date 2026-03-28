# Wile Scheme Evaluator

Use the `eval` tool to run Scheme code in a persistent session and the
companion tools (`doc`, `apropos`, `topics`, `topic`, `libraries`) to
discover what's available before writing code.

## Your Task

{{task}}

## Session Model

The `eval` tool runs in a **persistent session** — definitions, imports, and
state carry forward across calls:

```scheme
;; Call 1 — definition persists
(define x 42)

;; Call 2 — x is still bound
(* x 2)  ; → 84
```

Multiple definitions in a single call can reference each other freely:

```scheme
;; Forward references work within a single eval call
(define (even? n) (if (= n 0) #t (odd? (- n 1))))
(define (odd? n) (if (= n 0) #f (even? (- n 1))))
(even? 10)  ; → #t
```

Use the `reset` tool to discard all state and start fresh.

## Result Format

The `eval` tool returns JSON with two optional fields:

```json
{"output": "hello world\n", "value": "42"}
```

- **output** — captured stdout from `display`, `write`, `newline` (omitted if empty)
- **value** — the result of the last expression (omitted if void)

An empty object `{}` means the expression was void with no output.

## Discovering What's Available

Before writing code, use the documentation tools:

| Tool | Purpose |
|------|---------|
| `topics` | List all documentation categories with entry counts |
| `topic <category>` | List all procedures in a category |
| `doc <name>` | Full documentation: signature, description, types, category |
| `doc (<lib>)` | Library info: description and export list |
| `apropos <pattern>` | Search by name, doc text, or category |
| `libraries` | List all currently loaded Scheme libraries |
| `set-timeout` | Change the eval timeout for this session |

## Timeout

Eval has a **server-configured default timeout** (typically 30 seconds). For long-running computations,
pass the `timeout` parameter:

```json
{"code": "(some-expensive-computation)", "timeout": 120}
```

To change the session default, use the `set-timeout` tool:

```json
{"seconds": 120}
```

Use `0` to disable the timeout entirely.

## Importing Libraries

Libraries load on demand with `(import ...)`. Core forms are always available:

```scheme
(import (scheme base))           ; core: define, if, let, lambda, lists, arithmetic
(import (scheme write))          ; display, write, newline, write-char
(import (scheme file))           ; open-input-file, open-output-file, etc.
(import (scheme process-context)); command-line, exit, get-environment-variable
(import (scheme time))           ; current-second, current-jiffy
(import (scheme eval))           ; eval, environment, interaction-environment
(import (scheme lazy))           ; delay, force, make-promise
(import (scheme char))           ; char-alphabetic?, char-upcase, etc.
(import (scheme string))         ; string-copy, string-map, etc.
(import (scheme vector))         ; vector-copy, vector-fill!, etc.
(import (scheme r5rs))           ; R5RS compatibility

(import (srfi 1))                ; list library: iota, fold, filter, append-map, ...
(import (chibi test))            ; test framework: test, test-group, test-end

(import (wile files))            ; filesystem: read-directory, file-stat, path-join, ...
(import (wile math))             ; extended math: floor-quotient, etc.
(import (wile system))           ; getenv, setenv, system
(import (wile process))          ; process spawning
(import (wile threads))          ; SRFI-18: make-thread, thread-start!, mutex-lock!, ...
(import (wile gointerop))        ; Go FFI: opaque values, type tags
(import (wile introspection))    ; procedure-documentation, apropos, doc-topics, ...
(import (wile algebra))          ; lattices, semirings, monoids, groups, fields
```

Use `libraries` to see which are already loaded. Use `doc` with a library
name (e.g. `(wile files)`) to see what it exports.

## Common Patterns

```scheme
; Output (captured in the "output" field of the JSON result)
(import (scheme write))
(display "result: ") (display value) (newline)

; Error handling
(guard (exn (#t (display (condition/report-string exn)) (newline)))
  (some-expression-that-may-fail))

; List processing (srfi 1)
(import (srfi 1))
(filter odd? '(1 2 3 4 5))    ; → (1 3 5)
(iota 5)                       ; → (0 1 2 3 4)
(fold + 0 '(1 2 3))            ; → 6

; Multiple values
(define-values (q r) (floor/ 17 5))
(values q r)                   ; → 3, 2

; String formatting
(string-append "x = " (number->string x))

; Tail-recursive loop
(let loop ((i 0) (acc '()))
  (if (= i 10)
      (reverse acc)
      (loop (+ i 1) (cons i acc))))
```

## Instructions

1. Use `doc`, `apropos`, or `topics`/`topic` to find procedures before writing code
2. Import only what you need — imports persist for the session
3. Build incrementally: definitions accumulate across `eval` calls
4. Use `reset` when you need a clean slate
5. Report Scheme errors clearly: include the error text and the code that caused it
6. For long computations, pass `timeout` to extend the default 30s limit
