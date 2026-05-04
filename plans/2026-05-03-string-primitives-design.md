# String Primitives — Design + Phasing

**Status**: Approved (2026-05-03). User answered all open questions; ready for implementation per the companion `2026-05-03-string-primitives-impl.md`.

**Goal**: Add string functionality covering SRFI-13 + a small set of non-SRFI extras to wile's default library set, partitioned into two libraries:

1. `(srfi 13)` — primitives whose names appear in [SRFI-13](https://srfi.schemers.org/srfi-13/) ("String Library").
2. `(wile strings)` — string primitives that are *not* in SRFI-13 but are widely useful (e.g. `string-replace-all`, `string-blank?`).

**Implementation policy** (per Q-A resolution): **v1 is 100% Scheme.** No Go (FFI) primitives ship in v1. Promotion to Go is *profile-driven*: only when measured profiling on a real workload demonstrates a primitive is a hot path do we cross the FFI boundary. The `(wile strings-ffi)` library and `extensions/strings/` Go package described in §6 are **deferred** until that need is concrete.

## 1. Prior art and current state

| Layer | Where | What's already there |
|---|---|---|
| R7RS §6.7 baseline | `registry/core/strings.go` (always-on, `(scheme base)`) | `string?`, `string`, `make-string`, `string-length`, `string-ref`, `string-set!`, `substring`, `string-copy`, `string-append`, `string->list`, `list->string`, `symbol->string`, `string->symbol`, `string=?` / `<?` / `>?` / `<=?` / `>=?` |
| R7RS §6.7 extras | `internal/extensions/all/prim_strings.go` (`(wile all)` synthetic library) | `string-copy!`, `string-fill!`, `string-upcase`, `string-downcase`, `string-foldcase`, `string-ci=?` / `<?` / `>?` / `<=?` / `>=?` |
| SRFI library precedent | `stdlib/lib/srfi/1.sld` + `stdlib/lib/srfi/1/*.scm` (10 includes) | Pure Scheme library, manifest in `1.sld`, implementation files in `1/`. SRFI-132 is similar. SRFI-13 stub does **not** exist. |
| Audit | `plans/2026-04-19-audit-findings-phase4-strings.md` | All R7RS string primitives audited; 3 findings resolved. No SRFI-13 work. |

**Net gap**: every SRFI-13 search / trim / pad / take / drop / replace / tokenize / prefix-suffix procedure is missing. None of the non-R7RS extras (`string-blank?`, `string-replace-all`, etc.) exist either.

## 2. Architecture: pure Scheme, mirroring SRFI-1

Mirror the SRFI-1 / SRFI-132 file layout exactly:

```
stdlib/lib/srfi/13.sld             ← manifest: (define-library (srfi 13) ... (include "13/*.scm"))
stdlib/lib/srfi/13/util.scm        ← %string-range, %match-char? (shared helpers)
stdlib/lib/srfi/13/predicates.scm  ← (string-null? string-every string-any …)
stdlib/lib/srfi/13/selection.scm   ← (string-take string-drop string-tabulate …)
stdlib/lib/srfi/13/prefix-suffix.scm
stdlib/lib/srfi/13/search.scm      ← (string-index string-skip string-contains string-count …)
stdlib/lib/srfi/13/trim-pad.scm
stdlib/lib/srfi/13/comparison.scm
stdlib/lib/srfi/13/replace.scm     ← (string-replace string-tokenize string-join …)
stdlib/lib/srfi/13/concat.scm      ← (string-concatenate reverse-list->string)
stdlib/lib/srfi/13/fold.scm        ← (string-fold string-map string-for-each-index …)
stdlib/lib/srfi/13/reverse.scm     ← (string-reverse string-reverse!)
stdlib/lib/srfi/13/case.scm        ← (string-upcase! string-downcase!)
stdlib/lib/srfi/13/CLAUDE.local.md ← per-directory doc, mirrors srfi/1/

stdlib/lib/wile/strings.sld        ← (wile strings) kitchen-sink manifest
stdlib/lib/wile/strings/extras.scm ← string-replace-all, string-blank?, string-split, string-repeat, string-byte-length
```

**No Go-side changes in v1.** The R7RS-baseline + R7RS-extras already in `registry/core/` and `internal/extensions/all/` are sufficient substrate for every Scheme implementation in this plan. `string-byte-length` uses `(bytevector-length (string->utf8 s))`. `string-reverse` uses `(list->string (reverse (string->list s)))`. `string-contains` uses a naive O(n·m) search — acceptable for typical usage, profile-promoted later if a hot path appears.

### Future: profile-driven FFI promotion (deferred)

When (and only when) profiling on a real workload demonstrates a hot path, promote a Scheme procedure to Go by:

1. Creating `extensions/strings/` (Go ext, `LibraryName() => []string{"wile", "strings-ffi"}`).
2. Implementing the primitive in Go using the Go-stdlib routine that justifies the move.
3. Updating the relevant `.sld` manifest to import `(wile strings-ffi)` and re-export the Go primitive under the user-facing name. The Scheme implementation in the corresponding `13/<topic>.scm` file is removed in the same commit.

This path preserves a clean promotion boundary and reverts cleanly if the optimization regresses (just delete the Go ext and restore the Scheme define). No user-facing imports change.

The candidate list of plausible future promotions is preserved in §6 as a *non-binding* reference for future profiling work.

## 3. Library naming

| Library | Source | Contains |
|---|---|---|
| `(scheme base)` | `registry/core/strings.go` (unchanged) | R7RS §6.7 baseline (`string-length`, `string-append`, `string=?`, etc.) |
| `(wile all)` synthetic | `internal/extensions/all/prim_strings.go` (unchanged) | R7RS extras (`string-upcase`, `string-ci=?`, `string-copy!`, `string-fill!`) |
| `(wile strings-ffi)` | `extensions/strings/` (Go ext) | **Deferred to future work** (§2 profile-driven promotion). Does not exist in v1. |
| `(srfi 13)` | `stdlib/lib/srfi/13.sld` (manifest) + `stdlib/lib/srfi/13/*.scm` (Scheme implementations) | SRFI-13 surface; imports `(scheme base)` and `(scheme char)`, implements all primitives in Scheme. |
| `(wile strings)` | `stdlib/lib/wile/strings.sld` + `stdlib/lib/wile/strings/*.scm` | **Kitchen-sink convenience library**: imports `(scheme base)` + `(srfi 13)` and re-exports their entire user-facing surface, plus the non-SRFI-13 extras (§7). One import (`(import (wile strings))`) gets you everything. |

## 4. SRFI-13 surface inventory (~95 procedures)

Procedures grouped by SRFI-13 section. R7RS overlap noted in **bold** (re-exported from `(scheme base)` by the manifest).

### 4.1 Predicates (4)
- `string?` (**R7RS**), `string-null?`, `string-every`, `string-any`

### 4.2 Constructors (3)
- `make-string` (**R7RS**), `string` (**R7RS**), `string-tabulate`

### 4.3 List/string conversion (4)
- `string->list` (**R7RS**), `list->string` (**R7RS**), `reverse-list->string`, `string-join`

### 4.4 Selection (15)
- `string-length` (**R7RS**), `string-ref` (**R7RS**), `string-copy` (**R7RS**, with optional [start [end]]), `substring/shared`, `string-copy!` (**R7RS**)
- `string-take`, `string-drop`, `string-take-right`, `string-drop-right`
- `string-pad`, `string-pad-right`
- `string-trim`, `string-trim-right`, `string-trim-both`

### 4.5 Modification (2)
- `string-fill!` (**R7RS**), `string-set!` (**R7RS**)

### 4.6 Comparison (16)
SRFI-13 takes optional `[start1 end1 start2 end2]` and drops the trailing `?` from boolean comparisons:
- `string-compare`, `string-compare-ci` (3-way w/ procedure callback)
- `string=`, `string<`, `string>`, `string<=`, `string>=`, `string<>`
- `string-ci=`, `string-ci<`, `string-ci>`, `string-ci<=`, `string-ci>=`, `string-ci<>`
- `string-hash`, `string-hash-ci`

### 4.7 Prefix/Suffix (8)
- `string-prefix-length`, `string-suffix-length`
- `string-prefix-length-ci`, `string-suffix-length-ci`
- `string-prefix?`, `string-suffix?`
- `string-prefix-ci?`, `string-suffix-ci?`

### 4.8 Search (7)
- `string-index`, `string-index-right`
- `string-skip`, `string-skip-right`
- `string-count`
- `string-contains`, `string-contains-ci`

### 4.9 Case (6)
- `string-titlecase`, `string-titlecase!`
- `string-upcase` (**R7RS**), `string-upcase!`
- `string-downcase` (**R7RS**), `string-downcase!`

### 4.10 Reverse / Append (5+)
- `string-reverse`, `string-reverse!`
- `string-append` (**R7RS**), `string-concatenate`, `string-concatenate-reverse`
- `string-append/shared`, `string-concatenate/shared`, `string-concatenate-reverse/shared`

### 4.11 Fold / Unfold / Map (8)
- `string-map`, `string-for-each` (R7RS provides; SRFI-13 takes optional [start end])
- `string-fold`, `string-fold-right`
- `string-unfold`, `string-unfold-right`
- `string-for-each-index`

### 4.12 Replicate / Rotate (2)
- `xsubstring`, `string-xcopy!`

### 4.13 Replace / Tokenize (4)
- `string-replace` (splice — replace `s1[i:j]` with `s2`)
- `string-tokenize`
- `string-filter`, `string-delete`

## 5. SRFI-14 char-set criterion — deferred

SRFI-13 search / trim / count / index / skip / filter / delete / tokenize procedures accept a *criterion* of:
- a `char` — compare equal,
- a `char-set` (SRFI-14) — membership test,
- a predicate procedure — `(criterion ch) => boolean`.

**SRFI-14 is not implemented in wile.** Implementing the char-set type and ~70 SRFI-14 primitives is a separate effort.

**Plan**: ship SRFI-13 primitives with criterion = `char | predicate-procedure` only. char-set arguments raise a clear error until SRFI-14 lands. Document the deferral in the SRFI-13 library docstring header.

The criterion-dispatch helper (a 5-line cond on `char?` / `procedure?` / else-error) is shared across `string-index`, `string-skip`, `string-count`, `string-trim*`, `string-tokenize`, `string-filter`, `string-delete`. Putting it in `13/util.scm` keeps it auditable.

## 6. FFI promotion candidates (non-binding reference for future profile-driven work)

**v1 ships zero FFI primitives.** This section documents the procedures that are most plausible candidates for future Go promotion if profiling on a real workload demonstrates a hot path. **Do not act on this list during initial implementation** — it exists so a future session has the analysis pre-staged.

| Primitive | SRFI-13? | Plausible Go backing | Reason it might warrant promotion |
|---|---|---|---|
| `string-contains` | yes | `strings.Index` | Highly tuned (often Rabin-Karp / Boyer-Moore variants in Go runtime). Naive Scheme is O(n·m). |
| `string-contains-ci` | yes | `cases.Fold` + `strings.Index` | Same argument; case-fold cost amortized. |
| `string-replace-all` | no (`(wile strings)`) | `strings.ReplaceAll` | Bulk replace with internal `Builder` reuse. Scheme version has high allocator pressure. |
| `string-byte-length` | no (`(wile strings)`) | `len(s.GoString())` | Constant-time in Go; O(n) UTF-8 walk in Scheme (though the Scheme version using `bytevector-length` of `string->utf8` is itself a primitive call). |
| `string-reverse` | yes | rune-slice reverse in Go | Unicode-correct reversal; Go provides `[]rune` cleanly. Scheme version allocates list + reverses + re-strings. |

**Promotion procedure** (when justified):
1. Open a profile-driven plan citing the specific workload (`plans/<date>-string-<name>-promotion.md`).
2. Benchmark before (Scheme) vs after (Go) on that workload.
3. Promote only if measured improvement ≥ 5× on the workload's hot path *and* no regression on non-hot paths.
4. Architecture per §2's "Future: profile-driven FFI promotion" — `extensions/strings/` Go ext, `(wile strings-ffi)` synthetic library, manifest re-export.

### Rejected for FFI in v1 (Scheme implementation is the right tool)

| Primitive | Why Scheme is fine |
|---|---|
| `string-null?` | `(zero? (string-length s))`. |
| `string-every`, `string-any` | Predicate fold — has to call Scheme predicate per char anyway. FFI buys nothing. |
| `string-tabulate` | Build a list of `n` chars, then `list->string`. |
| `string-take`, `string-drop`, `string-take-right`, `string-drop-right` | Wrappers around `substring`. |
| `string-pad`, `string-pad-right` | `make-string` + `substring` + `string-append`. |
| `string-trim*` | Predicate scan from each end. Per-char predicate dominates regardless. |
| `string-prefix?`, `string-suffix?` | A `substring` + R7RS `string=?` already in `(scheme base)`. |
| `string-prefix-length`, `string-suffix-length` (and `-ci` variants) | Character-pair scan; no faster Go primitive available without rebuilding the comparison. |
| `string-index`, `string-skip` (char or predicate criterion) | Predicate path must call Scheme; char path is one comparison per rune — Scheme equivalent of Go's `strings.IndexRune` is ~2× slower on average, not 10×. Not worth crossing the FFI boundary. |
| `string-count` | Same as `string-index` — predicate per rune. |
| `string-replace` (splice form: `s1[0:i] ++ s2 ++ s1[j:]`) | Two `substring` + one `string-append`. |
| `string-tokenize`, `string-filter`, `string-delete` | Predicate-driven; FFI buys nothing. |
| `string-join`, `string-concatenate`, `reverse-list->string` | One pass over the list, native `string-append` does the work. |
| `string-fold`, `string-fold-right`, `string-map`, `string-for-each`, `string-for-each-index` | Per-char Scheme procedure call dominates. |
| Comparison family (`string=`, `string-compare`, etc., with optional `[start1 end1 start2 end2]`) | Wrapper around `substring` + R7RS `string=?` etc. Substring slice is the only allocation. |
| `string-upcase!`, `string-downcase!`, `string-reverse!` (mutating versions) | Loop with `string-set!`. Wile strings *are* mutable. R7RS provides the substrate. |

If profiling later shows any of these as a hot path in a real consumer, a Go primitive can replace the Scheme definition without changing the SRFI-13 interface — the manifest just stops re-importing the Scheme version.

## 7. `(wile strings)` candidate set

Procedures that are *not* in SRFI-13 but are widely useful:

| Name | Layer | Semantics |
|---|---|---|
| `string-blank?` | Scheme | `#t` if string is empty or contains only whitespace. Built on `string-every` + `char-whitespace?`. |
| `string-replace-all` | Scheme | Replace all occurrences of `from` substring with `to`. Loop on `string-contains` + `substring` + `string-append`. |
| `string-split` | Scheme | Split on a delimiter character. Built on `string-index` + `substring` loop. (Distinct from SRFI-13 `string-tokenize`, which splits on a char-set.) |
| `string-repeat` | Scheme | Repeat string N times. `string-append` over `make-list`. |
| `string-byte-length` | Scheme | UTF-8 byte length, distinct from codepoint length. `(bytevector-length (string->utf8 s))` — one-liner, since `string->utf8` and `bytevector-length` are already in `(scheme base)`. |
| `string-starts-with?` | Scheme | Alias for `string-prefix?` matching Wile's `?`-suffix predicate convention. Optional. |
| `string-ends-with?` | Scheme | Alias for `string-suffix?`. Optional. |

Per Q-C (resolved): `(wile strings)` v1 ships exactly these five extras: `string-blank?`, `string-replace-all`, `string-split`, `string-repeat`, `string-byte-length`. The `starts-with?` / `ends-with?` aliases are deferred — add only on explicit request.

## 8. Implementation conventions

### Scheme-side (the majority)

Mirror `stdlib/lib/srfi/1/*.scm`:

1. **Each `.scm` file groups one SRFI-13 section** (predicates, search, etc.). One concept per file.
2. **No top-of-file `define-library`** — these files are `include`d from `13.sld`. They contain raw `(define …)` forms.
3. **Docstrings**: structured per `2026-04-06-structured-docstring-metadata-design.md` (`Examples:`, `Parameters:`, `Returns:`, `Category: srfi-13`, `Keywords: …`, `See also: …`).
4. **Optional `[start [end]]` arguments**: implement once via a shared helper `%string-range` in `13/util.scm` that returns `(values start end)` after bounds-checking against the string length. R7RS-style `(define-record-type)` not needed; multi-value return is the idiomatic shape.
5. **Criterion dispatch**: shared helper `%match-char?` in `13/util.scm`:
   ```scheme
   (define (%match-char? criterion ch)
     (cond ((char? criterion) (char=? criterion ch))
           ((procedure? criterion) (criterion ch))
           (else (error "string-* criterion must be char or predicate"
                        criterion))))
   ```
   When SRFI-14 lands, add a `char-set?` branch.
6. **Mutating variants** (`string-upcase!`, `string-reverse!`): walk the string and `string-set!` each position. R7RS guarantees mutability for non-literal strings.
7. **Tests**: each section's primitives get a sibling Scheme integration test under `integration/testdata/srfi-13-tests-<section>.scm`, tested via the existing integration harness (`integration/r7rs_test.go` precedent). Per-procedure unit tests can also live in the implementation file if helpful.

### Go-side conventions

**v1 has no Go-side work.** When future profile-driven promotion ships, that PR will document the Go-side conventions it follows (`internal/extensions/all/prim_strings.go` is the established pattern: `helpers.RequireArg[T]`, `helpers.ParseSubrange`, sentinel + wrap errors, `LibraryName() => []string{"wile", "strings-ffi"}`).

## 9. Verification gates

For each phase:

1. **Compile**: `make build` clean.
2. **Integration tests** (Scheme side): `go test ./integration -run TestSRFI13Phase<N>` (or `TestWileStrings` for Phase 8) exercising the canonical SRFI-13 reference test cases (cribbed from the SRFI-13 reference impl's test suite).
3. **Lint**: `make lint` clean (golangci-lint, ruleguard).
4. **Coverage**: `make covercheck` — Scheme code measured via `--cover` (existing `coverage` package), gated at **≥80% line coverage** for new procedures (per Min-2). Coverage of pre-existing Go primitives we *call into* is not in scope for this gate.
5. **Library import smoke**: `(import (srfi 13))` and `(import (wile strings))` succeed end-to-end with no missing-binding warnings.
6. **`make ci`** passes locally before opening PR.

## 10. Phasing (one phase = one PR)

Reordered so wile-goast's top-five (`string-contains`, `string-join`, `string-suffix?`, `string-prefix?`, `string-split`) lands in **Phase 1** alongside the scaffolding, retiring three hand-rolls in `wile-goast/utils.scm` and `wile-goast/fca-recommend.scm` immediately. Subsequent phases fill out the rest of the SRFI-13 surface in topical groups.

**All v1 work is Scheme** (per Q-A). No Go extension creation, no FFI changes, no `(wile strings-ffi)` library.

| Phase | Scope | Approx LOC | Test count |
|---|---|---|---|
| **1** | **Scaffolding + wile-goast top-five.** Create `stdlib/lib/srfi/13.sld` + `13/util.scm` (with `%string-range`, `%match-char?`) + `13/prefix-suffix.scm` (with `string-prefix?`, `string-suffix?` only) + `13/replace.scm` (with `string-join` only) + `13/search.scm` (with naive Scheme `string-contains`, `string-contains-ci` only). Create `stdlib/lib/wile/strings.sld` + `wile/strings/extras.scm` (with `string-split` only). The `(wile strings)` library re-exports the four SRFI-13 procedures + `string-split`. After this phase, wile-goast can drop its three hand-rolls. | ~220 | ~35 |
| **2** | **SRFI-13 predicates + selection + full prefix/suffix family**: `string-null?`, `string-every`, `string-any`, `string-take`, `string-drop`, `string-take-right`, `string-drop-right`, `substring/shared` (alias), `string-tabulate`, `string-prefix-ci?`, `string-suffix-ci?`, `string-prefix-length`, `string-suffix-length`, `string-prefix-length-ci`, `string-suffix-length-ci`. | ~250 | ~50 |
| **3** | **Search family** (extends `13/search.scm` from Phase 1): `string-index`, `string-index-right`, `string-skip`, `string-skip-right`, `string-count`. | ~200 | ~25 |
| **4** | **Trim/pad**: `string-trim`, `string-trim-right`, `string-trim-both` (+ `string-trim-left` alias per Q-H), `string-pad`, `string-pad-right`. | ~125 | ~22 |
| **5** | **Comparison** (Scheme wrappers): `string=`, `string<`, `string>`, `string<=`, `string>=`, `string<>`, `string-ci` variants, `string-compare`, `string-compare-ci`. All built on `substring` + R7RS `string=?`/`<?`/etc. and `string-foldcase` (from `(scheme char)`). | ~180 | ~30 |
| **6** | **Reverse + replace + tokenize + concat + fold/map**: `string-reverse`, `string-reverse!`, `string-replace`, `string-tokenize`, `string-filter`, `string-delete`, `string-concatenate`, `reverse-list->string`, `string-for-each-index`, `string-map` (with optional [start end]), `string-fold`, `string-fold-right`. (`string-join` already shipped in Phase 1.) `string-reverse` is `(list->string (reverse (string->list s)))`; `string-reverse!` is a `string-set!` swap loop. | ~290 | ~40 |
| **7** | **Mutating case**: `string-upcase!`, `string-downcase!`. (`string-reverse!` already from Phase 6.) All built on `string-set!`. | ~70 | ~10 |
| **8** | **`(wile strings)` remaining extras** (all Scheme): `string-replace-all` (loop on `string-contains` + `substring` + `string-append`), `string-byte-length` (`(bytevector-length (string->utf8 s))`), `string-blank?`, `string-repeat`. (`string-split` already shipped in Phase 1.) | ~90 | ~22 |

**Total estimated delta**: ~1425 LOC (all Scheme + ~50 plumbing), ~234 test cases.

**Phase 1 is the demand-justified subset**: every procedure cites a wile-goast call site or hand-roll. Phases 2–8 are ordered by topical cohesion rather than urgency — any of them can be dropped or deferred if no consumer surfaces. Each phase is independently buildable + mergeable.

## 11. Scope cuts (deferred to v2)

| Name | Why deferred |
|---|---|
| `string-titlecase` / `-titlecase!` | Locale-sensitive Unicode title-casing. Real consumers (display formatting) usually want locale-aware. Defer until needed; Go infrastructure (`golang.org/x/text/cases.Title`) is ready when we cross that bridge. |
| `string-hash`, `string-hash-ci` | SRFI-13 spec is loose. Defer until hashtables actually need them — current hashtable extension uses Go map default hashing internally, no Scheme-visible hash function required. |
| `string-unfold`, `string-unfold-right` | SRFI-13 unfold is a 4-7-arg combinator. Pure-Scheme implementation (~30 lines) is fine, but low utility for v1. Add to `13/fold.scm` only if a downstream consumer asks. |
| `string-concatenate-reverse` | Niche. |
| `xsubstring`, `string-xcopy!` | Replicate/rotate semantics rarely useful in modern programs. |
| `string-append/shared`, `string-concatenate/shared`, `string-concatenate-reverse/shared` | Wile strings are mutable; sharing semantics tricky and SRFI-13 explicitly leaves them implementation-defined. v1 omits these entirely — programs needing sharing must use the non-shared forms. (`substring/shared` *is* shipped in v1 as a one-line alias for `substring`, in Phase 2 — it's the only `/shared` form widely depended on.) |
| char-set criterion (`(char-set ...)`) for all search/trim/count/tokenize/filter procedures | Blocked on SRFI-14. v1 raises an error if a `char-set` appears in a criterion position. |

After cuts: **58 SRFI-13 primitives + 5 `(wile strings)` extras + 1 alias (`string-trim-left`) = 64 user-visible names**. **0 FFI primitives in v1** — all Scheme.

## 12. Resolved questions

All questions resolved 2026-05-03; resolutions captured in the plan body.

| Q | Question | Resolution |
|---|---|---|
| Q-A | FFI subset for v1 | **No FFI in v1.** All Scheme. Future promotion is profile-driven (§6 lists candidates, §2 documents the promotion path). |
| Q-B | char-set scope | **Defer SRFI-14 char-set criteria to v2.** v1 accepts char + predicate criteria only; char-set raises an error. |
| Q-C | `(wile strings)` v1 set | **Ship §7's recommended five**: `string-blank?`, `string-replace-all`, `string-split`, `string-repeat`, `string-byte-length`. Skip the `starts-with?` / `ends-with?` aliases. |
| Q-D | Phase ordering | **Phases 1–8 as listed**, top-five-first. |
| Q-E | Defer titlecase/hash/unfold/xsubstring/`/shared` to v2 | **Yes.** Documented in §11. |
| Q-F | FFI helper library name (when v2 ships it) | **`(wile strings-ffi)`**. |
| Q-G | Test placement | **Per-phase integration test files** under `integration/testdata/srfi-13-tests-phase<N>.scm`. Optional consolidation at end of Phase 8. |
| Q-H | `string-trim-left` alias | **Yes, ship the alias.** One-line `(define string-trim-left string-trim)` in `13/trim-pad.scm` (Phase 4). |
| Q-I | wile-goast cutover bundling | **Separate PR** in the wile-goast repo, opened immediately after Phase 1 merges. |

## 13. Out of scope (and why)

- **SRFI-14 char-sets**: standalone library, ~70 procedures. Tracked separately if user wants it later. Plan stub: `plans/<future>-srfi-14-design.md`.
- **Regex** (SRFI-115 or `pcre`/`re2`): distinct domain, dependency conversation.
- **Locale-sensitive collation** (`string-collate`, ICU): not needed for R7RS / SRFI-13 v1. `string-compare-ci` uses Unicode case folding, locale-independent.
- **String ports** (`open-input-string`, etc.): already in `extensions/io`. Untouched here.
- **R7RS audit re-run**: the existing audit (`plans/2026-04-19-audit-findings-phase4-strings.md`) is closed. New primitives go through Phase-4-axis-C scrutiny *as part of their phase*, not as a separate audit.
