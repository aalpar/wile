# String Primitives — Implementation Plan

**Companion to**: `plans/2026-05-03-string-primitives-design.md` (design + phasing).

**Branch**: `feat/string-primitives` (branch from `master`, commit the design + impl plans together as commit 1).

**Total tasks**: 38 across 8 phases (Phase 1: 4, Phase 2: 6, Phase 3: 4, Phase 4: 4, Phase 5: 4, Phase 6: 7, Phase 7: 4, Phase 8: 5). **Total estimated delta**: ~1425 LOC (all Scheme + ~50 plumbing), ~234 test cases.

**Prerequisite reading** (load these into context at the start of any execution session):
- `plans/2026-05-03-string-primitives-design.md` (this plan's design)
- `registry/core/strings.go` and `registry/core/prim_strings.go` (R7RS-baseline patterns the Scheme code calls into)
- `internal/extensions/all/prim_strings.go` (R7RS-extras the Scheme code calls into: `string-foldcase`, `string-ci=?`, etc.)
- `stdlib/lib/srfi/1.sld` and `stdlib/lib/srfi/1/*.scm` (the layout we mirror)

**No Go-side changes in v1** (per Q-A). The Go references above are read-only — we call into existing primitives from Scheme but do not add new ones.

---

## Cross-cutting prerequisites

Read once before starting Phase 1; cite back during each phase.

### Naming conventions

- Scheme files in `stdlib/lib/srfi/13/`: lowercase, hyphenated, one topical group per file (e.g. `prefix-suffix.scm`, `search.scm`, `trim-pad.scm`).
- Internal Scheme helpers: `%`-prefix (e.g. `%string-range`, `%match-char?`).
- Library names: `(srfi 13)` user-facing; `(wile strings)` kitchen-sink user-facing. `(wile strings-ffi)` is reserved for future profile-driven promotion (does not exist in v1).

### Error reporting

All errors raised from Scheme implementations use `(error "site: what failed" details ...)` with structured arguments. No new `werr` sentinels are added in v1 — the v1 implementation is entirely Scheme, and Scheme `error` produces user-visible messages. If a future profile-driven promotion moves a procedure to Go, that PR adds whatever sentinels Go-side error wrapping requires.

Criterion mismatch error message convention (used by `%match-char?`):
> `"string-* criterion must be char or procedure (char-set support deferred to SRFI-14): <criterion-value>"`

### Implementation pattern: `case-lambda` for optional `[start end]` (per Crit-1)

**Every SRFI-13 procedure that accepts optional `[start [end]]` arguments uses `case-lambda` to dispatch by arity.** This is the project-wide convention for this plan; all 8 phases follow it. No rest-args, no sentinel values.

The shared helper does *bounds-checking only*, after the caller has resolved which arity branch is in play:

```scheme
;; %string-range-check — verify start/end are in range for s. Returns (values start end).
;; Raises on out-of-bounds.
(define (%string-range-check s start end)
  (let ((len (string-length s)))
    (cond ((or (< start 0) (> start len))
           (error "string range: start out of bounds" start len))
          ((or (< end start) (> end len))
           (error "string range: end out of bounds" end len))
          (else (values start end)))))
```

**Worked example** (every optional-range procedure follows this shape exactly):

```scheme
(define string-take
  (case-lambda
    ((s n)
     (substring s 0 n))
    ((s n start)
     (let-values (((s0 e0) (%string-range-check s start (string-length s))))
       (substring s s0 (+ s0 n))))
    ((s n start end)
     (let-values (((s0 e0) (%string-range-check s start end)))
       (substring s s0 (+ s0 n))))))
```

For procedures whose signature is `(name s [start [end]])` (no other required args), the `case-lambda` has three arms: `((s))`, `((s start))`, `((s start end))`. For comparison procedures like `(string= s1 s2 [start1 end1 start2 end2])`, the case-lambda has five arms. Always exhaust the arity space — no `error`-on-default fallthrough.

### Shared Scheme helpers (`stdlib/lib/srfi/13/util.scm`)

Created in Phase 1; consumed by every later phase.

```scheme
;; %string-range-check — bounds-check resolved start/end against s. Returns (values start end).
(define (%string-range-check s start end)
  (let ((len (string-length s)))
    (cond ((or (< start 0) (> start len))
           (error "string range: start out of bounds" start len))
          ((or (< end start) (> end len))
           (error "string range: end out of bounds" end len))
          (else (values start end)))))

;; %match-char? — apply a SRFI-13 criterion to a single char.
;; Criterion is either a char (compared with char=?) or a predicate procedure.
;; char-set criterion deferred until SRFI-14 lands.
(define (%match-char? criterion ch)
  (cond ((char? criterion)      (char=? criterion ch))
        ((procedure? criterion) (criterion ch))
        (else (error "string-* criterion must be char or procedure (char-set support deferred to SRFI-14)" criterion))))
```

Each phase that uses optional `[start end]` or criterion arguments imports `%string-range-check` / `%match-char?` from this file via the includes in `13.sld`.

### Scheme test placement

- **Per-phase integration test file**: `integration/testdata/srfi-13-tests-phase<N>.scm` (mirrors `srfi-132-tests.scm` shape — uses `(chibi test)` with `test-group`, `test-equal`).
- Each phase's PR includes one such file. Phase 8 final task consolidates them into one `srfi-13-tests.scm` if useful.
- Test count target per phase from §10 of the design plan.

### Per-phase definition of done

A phase is **not** complete until:
1. `make build` clean.
2. `go test ./integration -run TestSRFI13Phase<N>` clean (`TestWileStrings` for Phase 8).
3. `make lint` clean.
4. `make covercheck` clean — Scheme line coverage of new procedures **≥80%** (per Min-2).
5. `make ci` clean (final gate before opening the PR).
6. The phase's commit message cites the new procedure count and test count.

### Commit boundaries within a phase's PR (per Min-1)

Each phase's PR contains **at minimum two commits**:
- **Commit 1** — plan files only. Records the starting design. Only on the Phase-1 PR (subsequent phases reference these existing files, not duplicate them).
- **Commit 2+** — phase implementation work, one logical chunk per commit (manifest changes, then per-file Scheme implementations, then integration tests). Conventional-commit style: `feat(strings): <phase summary>` for the implementation; later fix-up commits use `fix(strings): <issue>`.

Phases 2–8 start their PR with the implementation commit directly — the design+impl plan files already exist on `master` from Phase 1's merge.

---

## Phase 1 — Scaffolding + wile-goast top-five

**Procedures shipped**: `string-prefix?`, `string-suffix?`, `string-join`, `string-contains`, `string-contains-ci`, `string-split`. (6 procedures, all Scheme.)

**Goal**: retire the three hand-rolls in wile-goast (`utils.scm:95`, `utils.scm:147`, `fca-recommend.scm:24`) immediately on merge.

**Commit message**: `feat(strings): SRFI-13 + (wile strings) scaffolding with wile-goast top-five (6 procedures)`

### Task 1.1 — Create `(srfi 13)` manifest and Phase-1 Scheme files

Create:

**`stdlib/lib/srfi/13.sld`**:
```scheme
(define-library (srfi 13)
  (description "SRFI 13: String Library — string predicates, search, trim, pad, replace, tokenize. v1: char + predicate criteria only (char-set deferred to SRFI-14). v1 implementation is pure Scheme; FFI promotion deferred to profile-driven future work.")
  (import (scheme base)
          (scheme char))
  (export
   ;; Phase 1: wile-goast top-five (SRFI-13 subset)
   string-prefix? string-suffix?
   string-contains string-contains-ci
   string-join)
  (include "13/util.scm"
           "13/prefix-suffix.scm"
           "13/search.scm"
           "13/replace.scm"))
```

`(import (scheme base))` brings in `substring`, `string-length`, `string=?`, `string-set!`, etc. `(import (scheme char))` brings in `string-foldcase`, `char-whitespace?`, `char-upcase`, `char-downcase`, the `char-ci*?` family, and the `string-ci*?` family — all of which Phase 1 (`string-contains-ci`) and later phases (4, 7, 8) depend on. Both are R7RS-standard; both are already implemented in wile (`stdlib/lib/scheme/{base,char}.sld`).

Subsequent phases will extend the export list and add more `include` clauses.

**`stdlib/lib/srfi/13/util.scm`** — content from cross-cutting prerequisites above (`%string-range`, `%match-char?`).

**`stdlib/lib/srfi/13/prefix-suffix.scm`** — `string-prefix?` and `string-suffix?` only. Each is a 5–10 line `define` building on `substring` and R7RS `string=?`. Include structured docstrings.

**`stdlib/lib/srfi/13/search.scm`** — `string-contains` and `string-contains-ci` only (Phase 3 will add the rest of the search family).
- `string-contains s1 s2 [start1 [end1 [start2 [end2]]]]`: naive O(n·m) loop. Walk index `i` from `start1` to `end1 - (length s2)`, compare `(substring s1 i (+ i s2-len))` with the substring of `s2`. Return `i` on match, `#f` otherwise. Reasonable v1 algorithm; profile-promote to `strings.Index` (Go) if a hot path appears.
- `string-contains-ci s1 s2 [...]`: `string-foldcase` both inputs once, then call `string-contains`.

**`stdlib/lib/srfi/13/replace.scm`** — `string-join` only. SRFI-13 signature: `(string-join string-list [delimiter [grammar]])` where `grammar` is one of `'infix` (default), `'strict-infix`, `'suffix`, `'prefix`. Empty list handling per spec: `'infix` returns `""`; `'strict-infix` raises an error.

**`stdlib/lib/srfi/13/CLAUDE.local.md`** — short doc explaining the file layout, similar to `srfi/1/CLAUDE.local.md`.

**Verification**: `(import (srfi 13)) (string-prefix? "hello" "helloworld")` evaluates to `#t` end-to-end. `(string-contains "hello world" "world")` evaluates to `6`.

### Task 1.2 — Create `(wile strings)` manifest and Phase-1 Scheme

Create:

**`stdlib/lib/wile/strings.sld`**:
```scheme
(define-library (wile strings)
  (description "Wile string library: SRFI-13 surface plus Wile-specific extras (string-split, string-replace-all, etc.).")
  (import (scheme base)
          (srfi 13))
  (export
   ;; Re-exported from (srfi 13)
   string-prefix? string-suffix?
   string-contains string-contains-ci
   string-join
   ;; Wile extras
   string-split)
  (include "strings/extras.scm"))
```

**`stdlib/lib/wile/strings/extras.scm`** — `string-split` only. Signature: `(string-split s delim)` where `delim` is a single character. Returns a list of substrings split at every occurrence of `delim`. Implement with a tight character-walk that uses only `string-length`, `string-ref`, `substring` — avoids the bootstrapping problem of needing `string-index` before Phase 3 ships it.

**Verification**: `(import (wile strings)) (string-split "a,b,c" #\,)` evaluates to `("a" "b" "c")`.

### Task 1.3 — Integration test for Phase 1

Create `integration/testdata/srfi-13-tests-phase1.scm`:
```scheme
(import (scheme base) (scheme write) (chibi test) (srfi 13) (wile strings))

(test-begin "srfi-13 phase 1")

(test-equal "string-prefix? positive" #t (string-prefix? "hello" "hello world"))
(test-equal "string-prefix? negative" #f (string-prefix? "world" "hello"))
(test-equal "string-prefix? empty"    #t (string-prefix? "" "anything"))
;; …

(test-equal "string-suffix? positive" #t (string-suffix? "world" "hello world"))
;; …

(test-equal "string-contains found"   6 (string-contains "hello world" "world"))
(test-equal "string-contains missing" #f (string-contains "hello" "xyz"))
(test-equal "string-contains-ci"      6 (string-contains-ci "Hello World" "WORLD"))
;; …

(test-equal "string-join default" "a,b,c" (string-join '("a" "b" "c") ","))
(test-equal "string-join no-delim" "abc"   (string-join '("a" "b" "c")))
;; …

(test-equal "string-split simple" '("a" "b" "c") (string-split "a,b,c" #\,))
(test-equal "string-split single" '("hello") (string-split "hello" #\,))
;; …

(test-end))
```

Aim for ~35 cases total spanning the **six** procedures (`string-prefix?`, `string-suffix?`, `string-contains`, `string-contains-ci`, `string-join`, `string-split`) with edge cases (empty strings, boundary conditions, optional-arg paths for `string-contains`).

Wire into `integration/r7rs_test.go` by appending a function in the existing SRFI-test idiom (matching `TestSRFI132` at line 127):
```go
// TestSRFI13Phase1 runs the SRFI-13 Phase-1 integration tests.
func TestSRFI13Phase1(t *testing.T) {
    runSchemeTest(t, "srfi-13-tests-phase1.scm", 2*time.Minute, "SRFI-13 Phase 1 (top-five)")
}
```
Each subsequent phase appends a similarly-named function (`TestSRFI13Phase2`, etc.). Phase 8 also adds `TestWileStrings` for the `(wile strings)` extras.

**Verification**: `go test ./integration -run TestSRFI13Phase1` clean.

### Task 1.4 — Final Phase 1 gate

Run:
```
make lint
make covercheck
make ci
```

All clean. Commit. Open PR. Per `plans/CLAUDE.md` workflow §3, request Copilot review and dispatch `/crosscheck:crosscheck all`.

---

## Phase 2 — SRFI-13 predicates + selection

**Procedures shipped**: `string-null?`, `string-every`, `string-any`, `string-take`, `string-drop`, `string-take-right`, `string-drop-right`, `substring/shared` (alias to `substring`), `string-tabulate`, `string-prefix-ci?`, `string-suffix-ci?`, `string-prefix-length`, `string-suffix-length`, `string-prefix-length-ci`, `string-suffix-length-ci`. (15 procedures.)

**All Scheme.** No Go changes.

**Commit message**: `feat(strings): SRFI-13 predicates, selection, full prefix/suffix family (15 procedures)`

### Task 2.1 — Predicates

Append to or create `stdlib/lib/srfi/13/predicates.scm`:
- `string-null?` — `(zero? (string-length s))`
- `string-every` — `(string-every criterion s [start [end]])` walks chars, returns `#t` iff `(%match-char? criterion ch)` for all. Returns `#f` on first mismatch.
- `string-any` — `(string-any criterion s [start [end]])` walks chars, returns the first truthy criterion result, or `#f`.

Add to `13.sld` `(export …)` list.

**Verification**: in-file unit test or wait for integration test in 2.5.

### Task 2.2 — Selection

Create `stdlib/lib/srfi/13/selection.scm`:
- `string-take`, `string-drop` — wrappers over `substring`.
- `string-take-right`, `string-drop-right` — same with reversed math.
- `substring/shared` — alias `(define substring/shared substring)`.
- `string-tabulate` — `(string-tabulate proc len)` builds a string of length `len` where char at index `i` is `(proc i)`. **Per Min-4: build a list (right-to-left) and call `list->string`** — single allocation for the result string, no mutation, idiomatic Scheme.

Add to `13.sld` `(export …)` and `(include …)`.

### Task 2.3 — Full prefix/suffix family

Append to `stdlib/lib/srfi/13/prefix-suffix.scm`:
- `string-prefix-length`, `string-suffix-length` — walk char-pairs, return count of matching prefix/suffix.
- `string-prefix-length-ci`, `string-suffix-length-ci` — same after `string-foldcase` on both inputs.
- `string-prefix-ci?`, `string-suffix-ci?` — built on `-length-ci` variants or directly via foldcase + R7RS `string=?`.

Add to `13.sld` `(export …)` list.

### Task 2.4 — Update `(wile strings)` re-exports

`stdlib/lib/wile/strings.sld` `(export …)` adds the 15 new procedures (re-exporting from `(srfi 13)`). No new code.

### Task 2.5 — Integration test for Phase 2

Create `integration/testdata/srfi-13-tests-phase2.scm`. Target: ~50 cases.

### Task 2.6 — Phase 2 gate

`make ci`. Commit. PR.

---

## Phase 3 — Search family

**Procedures shipped**: `string-index`, `string-index-right`, `string-skip`, `string-skip-right`, `string-count`. (5 procedures.)

**All Scheme.** Criterion = char or procedure; char-set raises an error per §5 of design.

**Commit message**: `feat(strings): SRFI-13 search family (5 procedures, char + predicate criteria)`

### Task 3.1 — `search.scm`

Create `stdlib/lib/srfi/13/search.scm`:
- `string-index` — `(string-index s criterion [start [end]])` linear scan, return first index or `#f`.
- `string-index-right` — same, scanning right-to-left.
- `string-skip` — `(string-skip s criterion [start [end]])` — return first index where criterion does **not** match.
- `string-skip-right` — same, right-to-left.
- `string-count` — count matches.

All five share the same dispatch on `%match-char?`. Wrap each as a thin `define` over a shared `%scan` helper (forward / reverse / count modes) inside `search.scm`.

### Task 3.2 — Wire into manifests

`13.sld` `(include "13/search.scm")` and add 5 exports. `wile/strings.sld` re-exports.

### Task 3.3 — Integration test

`integration/testdata/srfi-13-tests-phase3.scm`. Target: ~25 cases.

### Task 3.4 — Phase 3 gate

`make ci`. Commit. PR.

---

## Phase 4 — Trim/pad

**Procedures shipped**: `string-trim`, `string-trim-right`, `string-trim-both`, `string-trim-left` (alias for `string-trim` per Q-H), `string-pad`, `string-pad-right`. (6 names; 5 procedures + 1 alias.)

**All Scheme.**

**Commit message**: `feat(strings): SRFI-13 trim and pad (5 procedures + string-trim-left alias)`

### Task 4.1 — `trim-pad.scm`

Create `stdlib/lib/srfi/13/trim-pad.scm`:
- `string-trim` (leading), `string-trim-right` (trailing), `string-trim-both` — built on `string-skip`/`string-skip-right` from Phase 3 + `substring`. Default criterion is `char-whitespace?`.
- **`string-trim-left`** — one-line alias `(define string-trim-left string-trim)` per Q-H. Document in the docstring: "Alias for `string-trim`. SRFI-13 uses the unqualified name; the `-left` suffix is provided for ergonomics matching `string-trim-right` / `string-trim-both`."
- `string-pad s len [char [start [end]]]` — produces a string of exactly length `len`. **If `(string-length s)` ≥ `len`: keep the rightmost `len` characters (truncate from the left).** If shorter: prepend `(- len (string-length s))` copies of `char` (right-aligned output). Default `char` is `#\space`. Per SRFI-13 reference impl.
- `string-pad-right s len [char [start [end]]]` — symmetric. **If `(string-length s)` ≥ `len`: keep the leftmost `len` characters (truncate from the right).** If shorter: append padding (left-aligned output). Per SRFI-13 reference impl.

### Task 4.2 — Wire into manifests

`13.sld` `(include …)` + 6 exports (5 procedures + `string-trim-left`). `wile/strings.sld` re-exports.

### Task 4.3 — Integration test

`integration/testdata/srfi-13-tests-phase4.scm`. Target: ~22 cases. Edge cases: pad to length less than current (truncate), already-correct-length (no-op), all-whitespace string trim. Include explicit assertion that `(eq? string-trim-left string-trim)` is `#t` (one binding, not two).

### Task 4.4 — Phase 4 gate

`make ci`. Commit. PR.

---

## Phase 5 — Comparison family

**Procedures shipped**: `string=`, `string<`, `string>`, `string<=`, `string>=`, `string<>`, `string-ci=`, `string-ci<`, `string-ci>`, `string-ci<=`, `string-ci>=`, `string-ci<>`, `string-compare`, `string-compare-ci`. (14 procedures.)

**All Scheme** — wrappers over `substring` + R7RS comparison + `string-foldcase`.

**Commit message**: `feat(strings): SRFI-13 comparison family with optional ranges (14 procedures)`

### Task 5.1 — `comparison.scm`

Create `stdlib/lib/srfi/13/comparison.scm`:
- All 12 boolean comparisons accept `(s1 s2 [start1 [end1 [start2 [end2]]]])`. Use `%string-range` twice to slice each input, then dispatch to R7RS `string=?` / `<?` / etc. (or `string-foldcase` first for `-ci` variants).
- `string-compare s1 s2 proc< proc= proc> [start1 …]` — three-way: walks chars to find the first difference, calls `proc<` / `proc=` / `proc>` with the index where it diverged (or with the end-of-string index for equal prefixes that differ in length). Reference SRFI-13 spec carefully — semantics are subtle.
- `string-compare-ci` — same with foldcase.

### Task 5.2 — Wire into manifests

`(srfi 13)` adds 14 exports (the SRFI-13 names: `string=`, `string<`, …, `string-compare`, `string-compare-ci`).

**`(wile strings)` re-export policy** (per Imp-3 resolution): the kitchen-sink library re-exports both the R7RS names and the SRFI-13 names as distinct bindings. Concretely, `wile/strings.sld` `(export …)` lists:
- R7RS forms (already there from `(scheme base)` re-export): `string=?`, `string<?`, `string>?`, `string<=?`, `string>=?` (variadic, no ranges).
- R7RS-extras (already there from `(scheme char)` re-export): `string-ci=?`, `string-ci<?`, `string-ci>?`, `string-ci<=?`, `string-ci>=?`.
- SRFI-13 forms (newly added in Phase 5): `string=`, `string<`, `string>`, `string<=`, `string>=`, `string<>`, `string-ci=`, `string-ci<`, `string-ci>`, `string-ci<=`, `string-ci>=`, `string-ci<>`, `string-compare`, `string-compare-ci` (binary, with optional ranges).

Both families coexist — `string=?` and `string=` are distinct names with distinct shapes. Document the difference in `wile/strings/CLAUDE.local.md` so consumers can pick: "use the R7RS variadic form for arbitrary-arity equality; use the SRFI-13 form when you need optional `[start1 end1 start2 end2]` slicing."

### Task 5.3 — Integration test

Target: ~30 cases. Cover the optional `[start1 end1 start2 end2]` paths explicitly — every comparison procedure has 5 arities (no ranges, start1, start1/end1, start1/end1/start2, full).

### Task 5.4 — Phase 5 gate

`make ci`. Commit. PR.

---

## Phase 6 — Reverse + replace + tokenize + concat + fold/map

**Procedures shipped**: `string-reverse`, `string-reverse!`, `string-replace`, `string-tokenize`, `string-filter`, `string-delete`, `string-concatenate`, `reverse-list->string`, `string-for-each-index`, `string-map` (with optional [start end]), `string-fold`, `string-fold-right`. (12 procedures, all Scheme.)

**Commit message**: `feat(strings): SRFI-13 reverse, replace, tokenize, concat, fold (12 procedures)`

### Task 6.1 — Scheme: `string-reverse` + `string-reverse!`

Create `stdlib/lib/srfi/13/reverse.scm`:
- `string-reverse s [start [end]]` — `(list->string (reverse (string->list s start end)))`. Use `%string-range` to resolve optional args, then call R7RS `string->list` with the resolved range. Two allocations (the list and the resulting string) — acceptable; promote to Go later if profiling shows hot.
- `string-reverse! s [start [end]]` — in-place via `string-set!` swap loop. For each `i` in `[start, (start+end)/2)`, swap `(string-ref s i)` with `(string-ref s (- end 1 (- i start)))`. Returns `(values)` (R7RS unspecified-return idiom).

### Task 6.2 — Scheme: replace + tokenize + filter + delete

Append to `stdlib/lib/srfi/13/replace.scm`:
- `string-replace s1 s2 i j [start2 [end2]]` — splice form: replace `s1[i:j]` with `s2[start2:end2]`. Two `substring` + one `string-append`.
- `string-tokenize s [token-set [start [end]]]` — split into tokens. The criterion identifies *which characters belong to a token* (not which characters separate tokens — this is the SRFI-13 sense). SRFI-13 spec defaults to `char-set:graphic`; since SRFI-14 is deferred, **v1's default is `(lambda (ch) (not (char-whitespace? ch)))`** — the predicate stand-in for `char-set:graphic`. `(string-tokenize "hello world")` → `("hello" "world")`. Implementation: `string-skip` to find the start of a token, `string-index` to find its end (using `not` of the criterion), `substring` to extract; repeat. Document the SRFI-14 deferral in the docstring.
- `string-filter criterion s [start [end]]` — keep matching chars.
- `string-delete criterion s [start [end]]` — drop matching chars.

### Task 6.3 — Scheme: concat

Create `stdlib/lib/srfi/13/concat.scm`:
- `string-concatenate string-list` — `(apply string-append string-list)`.
- `reverse-list->string char-list` — `(list->string (reverse char-list))`.

### Task 6.4 — Scheme: fold/map

Create `stdlib/lib/srfi/13/fold.scm`:
- `string-fold kons knil s [start [end]]` — left fold over chars.
- `string-fold-right kons knil s [start [end]]` — right fold.
- `string-for-each-index proc s [start [end]]` — apply `proc` to each index.
- `string-map proc s [start [end]]` — SRFI-13 variant with optional range. Distinct from R7RS `string-map` in `(scheme base)`; SRFI-13 export shadows R7RS within `(srfi 13)` scope.

R7RS `string-map` shadowing note: `(srfi 13)` and `(scheme base)` both export `string-map`. When a program imports both, the importer must rename one (or use prefixed import). Document this in the SRFI-13 docstring.

### Task 6.5 — Wire into manifests

`13.sld` `(include …)` adds `reverse.scm` + `concat.scm` + `fold.scm`. 12 exports. `wile/strings.sld` re-exports.

### Task 6.6 — Integration test

Target: ~38 cases.

### Task 6.7 — Phase 6 gate

`make ci`. Commit. PR.

---

## Phase 7 — Mutating case

**Procedures shipped**: `string-upcase!`, `string-downcase!`. (2 procedures.)

**All Scheme** — built on `string-set!` per-char loop with `char-upcase` / `char-downcase` from `(scheme char)`.

**Commit message**: `feat(strings): SRFI-13 mutating case forms (2 procedures)`

### Task 7.1 — `case.scm`

Create `stdlib/lib/srfi/13/case.scm`:
- `string-upcase! s [start [end]]` — mutate each char in range to upper.
- `string-downcase! s [start [end]]` — mutate each char in range to lower.

Note: R7RS `string-upcase` (non-bang, in `(wile all)` extension) does full Unicode case mapping which can change string length (`ß → SS`). The mutating versions cannot change length, so they fall back to *simple* case mapping (`char-upcase` per character). Document this in the docstrings — it's a real semantic divergence.

### Task 7.2 — Wire into manifests

2 exports.

### Task 7.3 — Integration test

Target: ~10 cases. Include explicit assertions about non-length-changing simple mapping vs the full Unicode mapping in the non-bang forms (e.g. `ß` stays `ß` under the bang form, becomes `SS` under the non-bang).

### Task 7.4 — Phase 7 gate

`make ci`. Commit. PR.

---

## Phase 8 — `(wile strings)` extras

**Procedures shipped**: `string-replace-all`, `string-byte-length`, `string-blank?`, `string-repeat`. (4 procedures, all Scheme. `string-split` already in Phase 1.)

**Commit message**: `feat(strings): (wile strings) extras (4 procedures)`

### Task 8.1 — Implement extras in Scheme

Append to `stdlib/lib/wile/strings/extras.scm`:

- `string-replace-all s from to` — repeatedly find `from` in `s` via `string-contains` (now available since Phase 1), splice in `to`, continue from after the splice. Loop builds result via `string-append` — accept the O(n) per-replacement allocation cost in v1.
  ```scheme
  (define (string-replace-all s from to)
    "Replace all occurrences of FROM (a string) with TO in S.
     Examples:
       (string-replace-all \"foo bar foo\" \"foo\" \"baz\")  => \"baz bar baz\"
     Parameters: s : string  from : string  to : string
     Returns: string
     Category: strings
     Keywords: replace, gsub, substitute"
    (let ((from-len (string-length from)))
      (cond ((zero? from-len) s)  ; empty pattern — return s unchanged (matches Go)
            (else
             (let loop ((i 0) (parts '()))
               (let ((j (string-contains s from i)))
                 (cond (j  (loop (+ j from-len)
                                 (cons to (cons (substring s i j) parts))))
                       (else (apply string-append
                                    (reverse (cons (substring s i (string-length s))
                                                   parts)))))))))))
  ```

- `string-byte-length s` — `(bytevector-length (string->utf8 s))`. One line. R7RS guarantees both primitives in `(scheme base)`. Profile-promote to a Go FFI primitive only if a hot path emerges.

- `string-blank? s` — `(string-every char-whitespace? s)` using Phase-2's `string-every`.

- `string-repeat s n` — `(apply string-append (make-list n s))`. Acceptable for v1; the underlying `string-append` does a single concat allocation.

### Task 8.2 — Wire into `(wile strings)`

Update `wile/strings.sld` exports to include all four.

### Task 8.3 — Integration test

`integration/testdata/wile-strings-tests.scm` (these are `(wile strings)` extras, not SRFI-13). Wired via a new `TestWileStrings` function in `integration/r7rs_test.go`, mirroring the `TestSRFI13Phase<N>` shape. Target: ~22 cases. Edge cases for `string-replace-all`: empty `from` (return `s`), `from` not present, `from` overlapping with `to` (e.g. `(string-replace-all "aaa" "aa" "b")` — the SRFI default is left-to-right non-overlapping, result `"ba"`).

### Task 8.4 — Test file layout (per Min-3)

**Leave per-phase test files separate.** No consolidation in Phase 8. If a future need arises (e.g. test discovery slows materially, or a CI step needs a single canonical test file), open a follow-up plan to consolidate then. Until then, the eight `srfi-13-tests-phase<N>.scm` files plus `wile-strings-tests.scm` stay as the authoritative test layout.

### Task 8.5 — Phase 8 gate

`make ci`. Commit. PR.

---

## Post-implementation

### wile-goast cutover (separate PR in sibling repo)

After Phase 1 merges, open a PR in `wile-goast` that:
1. Adds `(wile strings)` (or `(srfi 13)`) to the relevant `define-library` import lists.
2. Removes the hand-rolled `string-contains` at `utils.scm:95`.
3. Removes the hand-rolled `string-join` at `utils.scm:147`.
4. Removes the hand-rolled `string-suffix?` at `fca-recommend.scm:24`.
5. Verifies the wile-goast test suite passes against the new wile build.

This PR is small and easy — but it is in the sibling repo, so it's tracked outside this plan.

### TODO.md update

After Phase 8 merges:
- Move SRFI-13 entry from "missing" to "done" in `TODO.md`.
- Add a note about deferred items (`string-titlecase`, `string-hash`, `string-unfold`, `xsubstring`, `*/shared`, char-set criteria) so future work can find them.

### Plan retirement

Both `2026-05-03-string-primitives-design.md` and `2026-05-03-string-primitives-impl.md` move from `plans/` to `memory/` after Phase 8 merges. Update `plans/CLAUDE.md` index to mark the plan as Shipped.

---

## Risk register

| Risk | Mitigation |
|---|---|
| SRFI-13 `string-compare` semantics in Phase 5 are subtler than the spec reads — divergence between SRFI-13 reference impl and ours. | Cross-check against the SRFI-13 reference `string-lib.scm` in the upstream srfi-13 repo. Add explicit test cases citing reference outputs. |
| R7RS `string-map` vs SRFI-13 `string-map` shadowing in Phase 6 confuses users. | Document the shadowing in the SRFI-13 docstring and in `13/CLAUDE.local.md`. Add an integration test that exercises both via prefixed import. |
| Mutating case forms (Phase 7) silently differ from non-mutating forms on cross-codepoint mappings (`ß` etc). | Document the divergence in docstrings *and* in the integration test. Treat it as a deliberate semantic, not a bug. |
| Naive Scheme `string-contains` is unacceptably slow on a real wile-goast workload (regressing post-Phase-1 cutover). | Benchmark wile-goast pre/post in the cutover PR. If regression > 2× on a real path, the cutover PR opens a follow-up profile-driven promotion plan per §6 of the design doc. The Scheme `(srfi 13)` implementation stays as the fallback while the Go ext is built. |
| `string-trim-left` alias drift — someone redefines `string-trim` later but not `string-trim-left`, breaking the equivalence. | Phase 4's integration test asserts `(eq? string-trim-left string-trim)`. If that ever fails, the alias broke. |
| `string-tokenize` v1 default (`(lambda (ch) (not (char-whitespace? ch)))`) drifts from SRFI-14's `char-set:graphic` when SRFI-14 lands. | When SRFI-14 lands, swap the default to `char-set:graphic`. The behavioral surface for whitespace-separated tokens is identical for ASCII; differences appear only on Unicode control-but-not-whitespace characters. Add a regression test at SRFI-14 integration time. |

---

## Quick-reference checklist (one line per phase)

- [ ] **Phase 1** — Scaffolding + top-five (6 procs, 35 tests). Branch + commit 1.
- [ ] **Phase 2** — Predicates + selection + prefix/suffix completion (15 procs, 50 tests).
- [ ] **Phase 3** — Search family (5 procs, 25 tests).
- [ ] **Phase 4** — Trim/pad + `string-trim-left` alias (5 procs + 1 alias, 22 tests).
- [ ] **Phase 5** — Comparison family (14 procs, 30 tests).
- [ ] **Phase 6** — Reverse + replace + concat + fold (12 procs, 40 tests).
- [ ] **Phase 7** — Mutating case (2 procs, 10 tests).
- [ ] **Phase 8** — `(wile strings)` extras (4 procs, 22 tests).
- [ ] **Post-merge** — wile-goast cutover PR.
- [ ] **Post-merge** — TODO.md update + plan retirement.

**Final tally**: 6 (Phase 1) + 15 + 5 + 5 + 14 + 12 + 2 + 4 = **63 procedures + 1 alias = 64 user-visible names**, ~234 test cases. All Scheme.
