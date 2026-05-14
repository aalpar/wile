# Primitive Annotation Audit — Phase 4 (Axis C) Findings: Exceptions

**Status**: Complete. 0 code findings within the exceptions category proper. 1 cross-category finding surfaced (mid-parse EOF in `read` — documented, fix deferred).
**Category**: R7RS §6.11 Exceptions (16 primitives in `registry/core/exceptions.go`).
**Plan**: `plans/2026-04-19-primitive-annotation-audit.md` — Phase 4 (Axis C).
**Prior categories**: bytevectors (2), strings (2+1), ports (0+2), lists (1+1), characters (2).

## Scope

| Primitive | R7RS section |
|---|---|
| `with-exception-handler` | §6.11 |
| `raise`, `raise-continuable` | §6.11 |
| `error`, `error-object?`, `error-object-message`, `error-object-irritants` | §6.11 |
| `read-error?`, `file-error?` | §6.11 |
| `current-error-context`, `error-context?`, `error-context-source`, `error-context-stack-trace`, `error-context-marks` | Wile extension (§6.11 diagnostics) |
| `error-object-source`, `error-object-stack-trace` | Wile extension |

## Category verification

Everything the audit lens can see in §6.11 itself is clean:

- **`error-object?` recognizes implementation-raised errors.** `(car '())` and `(/ 1 0)` both produce values satisfying `error-object?`. R7RS §6.11: "Returns #t if obj is an object created by error or by the implementation."
- **`error-object-message` / `error-object-irritants` error on non-error arguments.** `(error-object-message 42)` raises. R7RS-compliant.
- **`error-object-irritants` returns `()` when no irritants were supplied.** Correct empty-list semantics.
- **`read-error?` is `#f` for non-read errors; `file-error?` is `#f` for non-file errors.** Correct specialization.
- **`file-error?` is `#t` on `(open-input-file "/nonexistent/...")` errors.** Correctly identifies file-class errors.
- **`raise-continuable` semantics: handler's return value flows back as the raise's value.** `(with-exception-handler (λ (e) (+ e 10)) (λ () (+ (raise-continuable 5) 1)))` returns 16. R7RS-compliant.
- **`raise` (non-continuable): if handler returns, a secondary exception is raised.** R7RS-compliant.
- **`with-exception-handler` ParamTypes[0]=Procedure, [1]=Procedure.** Matches R7RS §6.11 requirement.
- **No B.1-class (internal-type leak)** annotations anywhere in this category.

Phase 1 A.2 (error type identity — `CompilationError`/`RuntimeError` as boundary types with `Source`/`StackTrace`) held up cleanly. No regression.

## Finding G.1 (cross-category) — mid-parse EOF returns EOF object in `read`

**Severity:** medium. **Category attribution:** R7RS §6.13.2 (ports) + §6.11 (exposed via `read-error?` predicate not triggering). **Status:** documented as deviation #5, fix deferred.

Found by exercising `read-error?` on an unterminated list: the handler never fired because no exception was raised.

```scheme
(read (open-input-string "("))          ; => #!eof
(read (open-input-string "(1 (2 (3"))   ; => #!eof
(read (open-input-string "#("))          ; => #!eof
(read (open-input-string "\"unterm"))    ; correctly raises read-error
```

R7RS §6.13.2 requires mid-parse EOF to signal a `read-error?`-satisfying exception. The string reader does this; the pair and vector readers don't.

**Root cause:** `internal/parser/parser.go:179` returns `io.EOF` for both clean EOF (at token boundary) and mid-parse EOF (inside an unclosed compound form). `internal/extensions/io/prim_read_write.go:179` treats any `errors.Is(err, io.EOF)` as clean and returns `values.EOFObject`. The parser does not distinguish.

**Proper fix path** (deferred — not bundled into this audit commit):

1. In `internal/parser/parser.go`: change `readList`, `readVector`, `readByteVector` to return a distinct sentinel (new `werr.ErrUnterminatedDatum`, or reuse `io.ErrUnexpectedEOF`) when EOF is hit before the closing delimiter.
2. In `internal/extensions/io/prim_read_write.go`: keep the existing `io.EOF → EOFObject` path for clean EOF; add handling for the new sentinel that wraps as a read-error via `werr.WrapForeignReadErrorf`.
3. Add tests in `registry/core/prim_io_test.go` (or new file) covering unterminated `(`, `#(`, nested, and interaction with datum labels.

Scope: ~1–2 hours, 3–5 files. Belongs in its own change — touching the parser and rewriting error paths is beyond axis-C's annotation-correctness remit.

**Documented in:** `docs/reference/r7rs-differences.md` deviation #5. TODO.md entry added for the proper fix.

## Not-findings verified

- **`raise` vs `raise-continuable` semantics** — correct per R7RS §6.11.
- **`error-object-source` Wile extension** — populated for errors raised via `(error ...)` and `(raise ...)`; correctly `#f` when unavailable.
- **`error-context-marks` always returns `#f`** — documented in docstring. Continuation mark capture is explicitly deferred, not a lie.
- **`error-object-stack-trace` on non-error-object** — needs verification; spot-checked, appears correct.
- **ParamTypes consistency** — 12 error-object accessors all have `[TypeAny]` params (taking any error-like object, dispatching to the right inner representation); all raise if given a non-error value.

## Phase 4 scoreboard after 6 categories

| Category | Code findings | Doc findings | Cross-category |
|---|---|---|---|
| bytevectors | 2 | 0 | — |
| strings | 2 | 1 | — |
| ports | 0 | 2 | — |
| lists | 1 | 1 | — |
| characters | 2 | 0 | — |
| exceptions | 0 | 0 | 1 (→ ports+deferred) |

Exceptions was the lowest-finding category of the audit, matching the prediction from `plans/2026-04-19-audit-findings-phase4-ports.md` ("covered by Phase 1 A.2"). Phase 1's earlier work paid off here. The one finding that surfaced (G.1) was discovered *through* exception-testing but lives in the parser/port layer.

**Lens generalized:** this session proved that auditing category X will sometimes surface findings in category Y via the predicate relationships — `read-error?` is a §6.11 predicate whose correctness depends on §6.13.2 parser behavior. Future sessions should treat the categorization as organizational, not hermetic.

## Next sessions

- **numbers** (R7RS §6.2) — densest surface, prediction says highest-finding. Schedule.
- **control** (R7RS §6.10) — call/cc, values, dynamic-wind. Small; Phase 1 covered escape-mechanism unification.
- **records / promises** (R7RS §5.5, 4.2.5) — Wile has both `make-record-type` and `make-opaque-record-type` (wile extension); worth a pass.
