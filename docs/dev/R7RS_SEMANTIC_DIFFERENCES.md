# R7RS Semantic Differences

This document catalogs differences between the current implementation and the R7RS-small specification. These are semantic differences where the implementation produces results but may not match R7RS behavior for certain inputs.

**Reference:** [R7RS-small Specification](https://small.r7rs.org/attachment/r7rs.pdf)

**Last Updated:** 2026-02-12

---

## Summary

One known limitation exists: non-blocking I/O detection (`char-ready?`, `u8-ready?`) always returns `#t`. This is a conservative safe behavior with minimal practical impact. See details below.

---

## Non-Blocking I/O Detection

**Affected Primitives:** `char-ready?`, `u8-ready?`

**R7RS §6.13.2 Requirement:**
> Returns `#t` if a character (or byte) is ready on the input port and returns `#f` otherwise. If `char-ready?` returns `#t` then the next `read-char` (or `read-u8`) operation on the given port is guaranteed not to hang.

**Wile Behavior:** Always returns `#t`.

**Rationale:**

Go's `io.Reader` interface does not expose readiness status or non-blocking I/O semantics. Implementing true non-blocking detection would require:
1. OS-specific syscalls (`select`/`poll` on Unix, overlapped I/O on Windows)
2. Platform-specific build tags and dependencies (`golang.org/x/sys/unix`, `golang.org/x/sys/windows`)
3. Handling buffered readers (`bufio.Reader`) where buffered data makes reads non-blocking even when the underlying descriptor would block
4. Significant complexity in the I/O layer with cross-platform maintenance burden

The conservative behavior (always returning `#t`) is **safe**: it may cause blocking where R7RS code expected non-blocking, but never claims data is available when it isn't (which would violate R7RS guarantees).

**Workaround:**

Use Go channels or goroutines for non-blocking I/O patterns:

```scheme
;; Instead of polling with char-ready?:
(if (char-ready? port)
    (read-char port)
    'not-ready)

;; Use a thread to read asynchronously:
(let ((ch (make-channel)))
  (thread-start!
    (make-thread
      (lambda ()
        (channel-send! ch (read-char port)))))
  (channel-receive ch))
```

**Impact:** **LOW** — `char-ready?` and `u8-ready?` are rarely used in modern Scheme code. These predicates were designed for select-style event loops, a pattern largely superseded by async/await and channel-based concurrency. Most I/O in Wile is either:
- File-based (always ready, blocking is acceptable)
- Network streams where blocking semantics are expected
- Interactive REPL input where immediate blocking is desired

**Estimated implementation effort:** 4-8 hours including cross-platform support and testing.

**ROI analysis:** Documentation (15 minutes) provides clear expectations at far better ROI than implementation (4-8 hours) for an exotic edge case.

---

## Extensions Beyond R7RS

These are Wile-specific features that extend R7RS. They do not conflict with R7RS behavior — standard Scheme programs behave identically. These extensions use reader prefixes in the `#` dispatch space that R7RS leaves implementation-defined.

### Arbitrary-Precision Number Literals

Wile provides reader syntax for explicitly constructing arbitrary-precision numbers. These are not part of any Scheme standard (R5RS, R6RS, R7RS, or SRFIs).

| Prefix | Type | Exactness | Backed by | Examples |
|--------|------|-----------|-----------|----------|
| `#z` | BigInteger | exact | `math/big.Int` | `#z12345678901234567890`, `#z-42`, `#z+7` |
| `#m` | BigFloat | inexact | `math/big.Float` (256-bit) | `#m3.14159265358979323846`, `#m1.5e-10`, `#m.5` |

Both prefixes are case-insensitive (`#Z`, `#M` also work), following R7RS §7.1.1 conventions.

**BigInteger (`#z`)** supports radix prefixes: `#z#b101` (binary), `#z#o77` (octal), `#z#x1F` (hex).

**BigFloat (`#m`)** supports optional sign, decimal point, and exponent markers (`e`, `s`, `f`, `d`, `l`).

**Note:** R7RS requires implementations to support arbitrarily large exact integers (§6.2.3). Wile satisfies this via automatic overflow promotion from `Integer` (int64) to `BigInteger` — the `#z` prefix is a convenience for explicit construction, not a conformance requirement. Standard R7RS programs never need `#z` or `#m`.

### Guard Body Multiple Values

R7RS §7.3's reference implementation of `guard` uses `(let ((result (begin e1 e2 ...))) ...)`, which binds a single value. If the body produces multiple values via `(values v1 v2 ...)`, the `let` binding triggers an arity mismatch.

Wile's `guard` uses `call-with-values` to capture all values from the body, then re-emits them via `(apply values results)`. This means `(guard (e (#f)) (values 1 2))` correctly propagates both values, whereas the R7RS reference implementation would signal an error.

