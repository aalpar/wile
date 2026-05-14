# Primitive Annotation Audit — Phase 4 (Axis C) Findings: Ports

**Status**: Complete. 2 findings (both stale CLAUDE.md docs), resolved. No code changes.
**Category**: R7RS §6.13 Ports (~35 primitives across `internal/extensions/io/` and `extensions/files/`).
**Plan**: `plans/2026-04-19-primitive-annotation-audit.md` — Phase 4 (Axis C: implementation vs published standard).
**Prior categories**: bytevectors (2 code findings), strings (2 code + 1 doc finding).

## Scope

R7RS §6.13 primitives:

| File | Primitives registered |
|---|---|
| `internal/extensions/io/register.go:addReadWrite` | 24 textual + binary read/write primitives |
| `internal/extensions/io/register.go:addPorts` | 11 port predicates + close/eof + string/bytevector port constructors |
| `extensions/files/register.go` | 4 file-open primitives |

Total surface: ~39 primitives (port procs from `port_procs.scm` like `call-with-port` are Scheme, out of Go-audit scope).

## Positive verification — what works

Most of the session was spent applying the parallel-case lens from prior categories. Every textual/binary primitive pair I checked is consistently registered:

| Textual | Binary | Shape |
|---|---|---|
| `read-char` | `read-u8` | `ParamCount: 1, IsVariadic: true` (0 required + optional port) ✓ |
| `peek-char` | `peek-u8` | same ✓ |
| `char-ready?` | `u8-ready?` | same ✓ |
| `write-char` | `write-u8` | `ParamCount: 2, IsVariadic: true` (1 required + optional port) ✓ |
| `read-string` | `read-bytevector` | `ParamCount: 2, IsVariadic: true` (1 required + optional port) ✓ |
| `write-string` | `write-bytevector` | same ✓ |
| `open-input-string` | `open-input-bytevector` | `ParamCount: 1` ✓ |
| `open-output-string` | `open-output-bytevector` | no `ParamCount` (0-arg) ✓ |
| `get-output-string` | `get-output-bytevector` | `ParamCount: 1`, specialized ParamType ✓ |
| `close-input-port` | `close-output-port` | `ParamCount: 1`, specialized ParamType ✓ |

The B.1-class bug (`TypeByte` leak) does **not** recur — `write-u8` uses `TypeInteger`, matching my Phase 4 bytevector fix convention. Range validation (0–255) lives in `ValidateByteValue`.

The C.1/C.2-class bug (`ParamCount` lying about arity) does **not** recur — every port primitive I checked has consistent `ParamCount` matching R7RS minimum arity.

Verified runtime behaviors:

```scheme
(close-input-port (open-output-string))       ; raises: "expected an input port"
(close-output-port (open-input-string "hi"))  ; raises: "expected an output port"
(get-output-bytevector (open-output-string))  ; raises: "expected a bytevector output port"
(get-output-string (open-output-bytevector))  ; raises: "expected a string output port"
(write-u8 -1  p)                              ; raises: "byte must be a byte (0-255)"
(write-u8 256 p)                              ; raises: "byte must be a byte (0-255)"
```

Range and type enforcement is correct everywhere.

## Finding D.1 — `CLAUDE.local.md` misdescribes close-port family

**Severity:** low (internal documentation drift only). **Status:** fixed.

`internal/extensions/io/CLAUDE.local.md` claimed:

> - **close-input-port and close-output-port**: Both map to the same `PrimClosePort` implementation

The claim is false. `prim_ports.go` has three distinct implementations:

- `PrimClosePort` (line 70) — accepts any `values.Port`, no direction check
- `PrimCloseInputPort` (line 88) — requires `values.InputPort`, errors on output-only ports
- `PrimCloseOutputPort` (line 107) — requires `values.OutputPort`, **flushes before closing**, errors on input-only ports

The flush step is the semantically important difference — `close-output-port` on an output port with buffered data writes the buffer before closing; `close-port` does not. A reader who believed the CLAUDE.md claim might skip `close-output-port` in favor of `close-port` and silently drop buffered output.

All three delegate to `closePort()` for cache eviction, which is the kernel of truth behind the stale claim. Fixed the CLAUDE.md to describe the divergence accurately.

## Finding D.2 — `CLAUDE.local.md` lists `open-output-bytevector` as arity 0-1

**Severity:** low (internal documentation drift only). **Status:** fixed.

`CLAUDE.local.md` bytevector-ports table:

> | `open-output-bytevector` | 0-1 | Create output bytevector port |

The one-arg form was removed in commit `d098c54b` during the Phase 1 audit (referenced in `plans/2026-04-19-primitive-annotation-audit.md:16`). Current registration is zero-arg:

```go
{Name: "open-output-bytevector", Impl: PrimOpenOutputBytevector, ...}
// no ParamCount, no IsVariadic → 0 args
```

Fixed the CLAUDE.md entry to `0` with a note about when the other form was removed.

## `char-ready?` / `u8-ready?` deviation (not a new finding)

Already documented in `TODO.md` "Documented Exceptions" as L7:

> L7 (`char-ready?`/`u8-ready?` always `#t`) — documented semantic difference, no fix planned

The impls always return `#t` because Wile lacks non-blocking I/O. Spec says they *may* return `#t` when reading wouldn't block, so this is a conservative under-approximation — returns `#t` even when a read *might* block. A strict conformance interpretation could call this a deviation; Wile accepts it. No action.

## Not-findings

### ParamType positional-optional limitation — not a category-level issue

`write-string` takes `(string [port [start [end]]])` — three heterogeneous optionals. Current `ParamTypes: [TypeString, TypeTextualOutputPort]` only types through the first optional. Beyond that, args are implicitly `TypeAny` in the rest list.

This is a **Phase-2 type-checking design question** (how do `TypeConstraint` arrays handle heterogeneous variadic optionals?), not an annotation correctness issue. Noted for the future vocabulary plan; does not belong in axis-C.

### read-bytevector!, write-bytevector same limitation

Same pattern. Not a finding.

### Port impl caching strategy

CLAUDE.md describes tokenizer/parser caches with eviction on EOF + close. Verified by inspection; behavior matches docstring. No drift.

## Phase 4 scoreboard after 3 categories

| Category | Code findings | Doc findings | Time |
|---|---|---|---|
| bytevectors | 2 (B.1, B.5) | 0 | ~45 min |
| strings | 2 (C.1, C.2) | 1 (C.0) | ~60 min |
| ports | 0 | 2 (D.1, D.2) | ~30 min |

The port category's low code-finding count is interesting signal. Two plausible explanations:

1. **The parallel-case lens is now load-bearing in the registration files.** When `read-char` and `read-u8` are registered back-to-back in the same file, inconsistencies are easy to spot at review time. Bytevectors and strings had cross-file splits (`registry/core/byte_vectors.go` vs `internal/extensions/io`) that made parallel-case drift easier to miss.
2. **Port primitives are more recently touched** (v1.9.7 embedded-stdlib work, v1.8.0 Expression-Type API, v1.13.x registry/helpers migrations). Recent code tends to follow current conventions; older code carries older conventions.

Both would predict lower findings per session as the audit progresses through categories that are newer, homogeneous, and well-paralleled. That's the current state.

## Next categories — updated prioritization

- **lists** (R7RS §6.4) — moderate surface (25 primitives), heterogeneous register (`core/lists.go` + bootstrap procs), `assq`/`memq`/`list-tail` edge cases, predicates split across multiple files.
- **characters** (R7RS §6.6) — Unicode case mapping; smaller surface than strings; potential parallel-case drift with strings (`char-upcase` ↔ `string-upcase` etc.).
- **numbers** (R7RS §6.2) — schedule last. Largest surface (~50 primitives), densest existing test coverage (`values/numeric_*_test.go` ~49 type combinations), most recent audit attention in annotation-via-axis-A work.
- **control** (R7RS §6.10) — continuations, call/cc, values. Tricky semantics, low annotation surface, mostly dealt with in prior PRs.
- **exceptions** (R7RS §6.11) — small surface, covered by Phase 1 A.2 error-type-identity work.
