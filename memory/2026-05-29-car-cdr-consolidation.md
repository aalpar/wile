# Car/Cdr Consolidation Implementation Plan

> **Status: COMPLETE (2026-05-30).** All three phases shipped to master. Phase C.5 (Pair.ForEach → Spine) was skipped per the plan's own decision rule after measuring a 40–56% regression on the micro-bench; comment + permanent regression guard landed in its place. See "Outcome Summary" at the bottom of this file for the full picture.

> **For agentic workers:** Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [x]`) syntax for tracking. The plan assumes the Wile project conventions in `CLAUDE.md` and `CLAUDE.local.md` (no `fmt.Errorf`, no `errors.New`, sentinel+wrap via `werr.WrapForeignErrorf`, table-driven tests mandatory, no `Co-Authored-By` lines, branch+merge workflow on master, `make lint && make covercheck` must pass before claiming done).

**Goal:** Consolidate ~70 hand-rolled `.Car()/.Cdr()` destructuring blocks into a small, closed helper API and a single canonical spine iterator, eliminating ~600 lines of repeated code without changing observable behavior.

**Architecture:** Three independent phases, each landing on its own feature branch with its own merge to master.

1. **Phase A — ForEachList adoption.** Mechanical replacement of the existing inline `ForEach + IsEmptyList tail check` pattern with the already-existing `helpers.ForEachList`. No new public API. Zero risk. Done first because it shrinks the surface area for the next two phases.
2. **Phase B — `Uncons` / `UnconsTyped` / `CarAs` / `NthCons` helpers.** New helpers in `registry/helpers/list.go` that close the bicartesian algebra over `values.Tuple`. TDD: helpers ship with their tests, then call sites adopt them in a separate commit per file.
3. **Phase C — `Spine` iterator.** New `iter.Seq2[*Pair, error]` iterator in `values/pair.go` plus a cycle-detecting variant. Internal callers (`IsList`, `Length`, `AsVector`, `EqualTo`, `SchemeString`, and the `SyntaxPair` peers) get rewritten to consume it. Public `ForEach` and `Append` semantics are preserved exactly.

**Tech Stack:** Go 1.24 (workspace uses `go 1.24.0`; `iter.Seq2` is from 1.23+). `quicktest` + `valuestest.SchemeEquals` for tests. `werr` sentinel+wrap for errors. `make lint`, `make test`, `make covercheck`.

---

## Cross-Cutting Conventions for Every Phase

These rules apply to every task in this plan. Re-read them when in doubt:

- **Branch from master.** Each phase gets its own branch: `phase-a-foreach-list-adoption`, `phase-b-uncons-helpers`, `phase-c-spine-iterator`. Verify with `git fetch && git rebase origin/master` before starting.
- **Verify CLAIMS against code.** Every file path and line range in this plan was checked against the working tree at the time of writing. If `grep` shows a different shape at execution time, *trust the code* and update the plan inline rather than forcing the pattern.
- **Preserve sentinels, not message strings.** When replacing an inline block with a helper, the wrapped sentinel (e.g., `werr.ErrNotAList`) MUST stay the same. The human-readable message string MAY drift — that's why tests use `errors.Is`, not string matching. If a test checks message text, update the test to match the helper's format.
- **Commit at every green point.** Each task ends with `make test ./<path>/...` + `make lint` passing; commit immediately. Never batch unrelated changes.
- **No commit without explicit permission per CLAUDE.md.** This plan's commit steps are explicit permission. Skip the permission prompt and commit.
- **No `Co-Authored-By` lines** in commit messages (global CLAUDE.md rule).
- **TodoWrite is encouraged** for tracking per-phase progress; mark each numbered task done as you go.
- **Run `make ci` once per phase, before opening the PR.** `make lint && make covercheck && make test` is the floor.

---

## Phase A — Adopt `helpers.ForEachList` Across All Inline Sites

**Status: ✅ COMPLETE.** Shipped via commits `5f6de63a`, `68f53579`, `7a8dd15f`, `83fe4ef4`, `949376d0`, `6671f247` (axis-b-manifest regen). Branch merged to master.

**Goal of phase:** Eliminate ~30 inline copies of `ForEach + IsEmptyList(tail) check` by adopting the existing `helpers.ForEachList` (registry/helpers/list.go:29). No new public API. Catches silent improper-list acceptance bugs as a side benefit.

**Branch:** `phase-a-foreach-list-adoption`

**Pre-flight check:** Confirm `helpers.ForEachList` already exists with this signature:

```go
// registry/helpers/list.go:29
func ForEachList(ctx context.Context, t values.Tuple, name string,
    fn func(context.Context, int, bool, values.Value) error) error
```

If it doesn't, `grep -n 'func ForEachList' registry/helpers/list.go` will report no match — STOP and re-read this plan, the codebase has drifted.

### Task A.0: Setup

- [x] **Step A.0.1: Branch from master.**

  ```bash
  git fetch origin
  git rebase origin/master
  git checkout -b phase-a-foreach-list-adoption
  ```

- [x] **Step A.0.2: Enumerate target sites.**

  ```bash
  grep -rEn 'ForEach\(.*func.*\) error \{$' --include='*.go' \
    registry/core/ machine/compilation/ extensions/ internal/ 2>/dev/null | \
    grep -v _test.go > /tmp/foreach-sites.txt
  wc -l /tmp/foreach-sites.txt
  ```

  Expected: 25–35 lines. Each line is a candidate. The actual replacement list below was hand-checked against the codebase at plan-writing time; verify each one still exists before editing.

### Task A.1: Adopt in `registry/core/prim_lists.go`

**Files:**
- Modify: `registry/core/prim_lists.go` — `PrimAppend` (lines ~94–103), `PrimReverse` (lines ~150–159), `PrimLength` (lines ~177–186)
- Test: `registry/core/prim_lists_test.go` (existing — re-run to confirm no regression)

- [x] **Step A.1.1: Rewrite `PrimAppend`'s outer args-loop.**

  Find the block beginning `v, err := args.ForEach(mc.Context(), func(...` ending at the `if !values.IsEmptyList(v) { return ... ErrNotAList ... }`.

  Replace with:

  ```go
  var lists values.Vector
  err := helpers.ForEachList(mc.Context(), args, "append", func(_ context.Context, _ int, _ bool, elem values.Value) error {
      lists = append(lists, elem)
      return nil
  })
  if err != nil {
      return err
  }
  ```

  Then rewrite the inner per-list `pr.ForEach(...)` (lines ~119–128) the same way, using `"append"` as the name.

- [x] **Step A.1.2: Rewrite `PrimReverse`.**

  Replace the `pr.ForEach + IsEmptyList(v)` block with:

  ```go
  err := helpers.ForEachList(mc.Context(), pr, "reverse", func(_ context.Context, _ int, _ bool, v values.Value) error {
      result = values.NewCons(v, result)
      return nil
  })
  if err != nil {
      return err
  }
  ```

- [x] **Step A.1.3: Rewrite `PrimLength`.**

  Same shape with `"length"` as the name; body increments `count`.

- [x] **Step A.1.4: Run the affected tests.**

  ```bash
  go test ./registry/core/... -run 'TestPrim(Append|Reverse|Length)' -v
  ```

  Expected: PASS. If any test was checking literal error message text, update it to use `qt.ErrorIs` with the sentinel.

- [x] **Step A.1.5: Run lint and commit.**

  ```bash
  make lint
  git add registry/core/prim_lists.go registry/core/prim_lists_test.go
  git commit -m "refactor(core): adopt ForEachList in append/reverse/length"
  ```

### Task A.2: Adopt in `machine/compilation/import_set_datum.go`

**Files:**
- Modify: `machine/compilation/import_set_datum.go` — `ParseLibraryNameFromDatum` (lines ~40–55), `parseImportSetRenameFromDatum` rename-pair loop (lines ~220–243), `parseIdentifierListFromDatum` (lines ~335–346)

This file currently uses `tuple.ForEach(ctx, func...)` followed by `if err != nil { return ... }`. None of these check the tail at all — they silently accept improper lists. Adopting `ForEachList` *will* change observed behavior on malformed library names like `(scheme . base)`. That's the desired behavior change.

- [x] **Step A.2.1: Replace each ForEach call.**

  For each of the three sites, replace:

  ```go
  _, err := tuple.ForEach(ctx, func(...) error { ... })
  if err != nil { return ..., err }
  ```

  with:

  ```go
  err := helpers.ForEachList(ctx, tuple, "library name" /* or "rename pairs", "identifier list" */, func(...) error { ... })
  if err != nil { return ..., err }
  ```

  Add the import: `"github.com/aalpar/wile/registry/helpers"`.

- [x] **Step A.2.2: Run the package tests.**

  ```bash
  go test ./machine/compilation/... -v
  ```

  Expected: PASS. If any test deliberately exercises improper-list library names, it should now expect an `ErrNotAList`-wrapped error.

- [x] **Step A.2.3: Add a regression test for the improper-list fix.**

  Append to `machine/compilation/import_set_datum_test.go` (or create if absent):

  ```go
  func TestParseImportSetRejectsImproperList(t *testing.T) {
      tcs := []struct {
          name string
          expr values.Value
      }{
          {"library-name-dotted",
              values.NewCons(values.NewSymbol("scheme"), values.NewSymbol("base"))},
      }
      for _, tc := range tcs {
          t.Run(tc.name, func(t *testing.T) {
              _, err := ParseLibraryNameFromDatum(context.Background(), tc.expr)
              qt.Assert(t, errors.Is(err, werr.ErrNotAList), qt.IsTrue,
                  qt.Commentf("expected ErrNotAList, got %v", err))
          })
      }
  }
  ```

  Add imports as needed.

- [x] **Step A.2.4: Lint + commit.**

  ```bash
  make lint
  git add machine/compilation/import_set_datum.go machine/compilation/import_set_datum_test.go
  git commit -m "refactor(compilation): adopt ForEachList in import-set datum parsing"
  ```

### Task A.3: Adopt in `extensions/eval/prim_eval.go`

**Files:**
- Modify: `extensions/eval/prim_eval.go` — `PrimEnvironment` ForEach loop (lines ~380–415)

- [x] **Step A.3.1: Replace the loop.**

  Find the block starting `v, err := args.ForEach(mc.Context(), func(...` (around line 380). Replace it (and the trailing `if !values.IsEmptyList(v)` check around line 413) with:

  ```go
  err := helpers.ForEachList(mc.Context(), args, "environment", func(_ context.Context, _ int, _ bool, specVal values.Value) error {
      // ... existing body unchanged ...
  })
  if err != nil {
      return err
  }
  ```

  The `helpers` import already exists in this file (`helpers.RequireType` is used). No new import.

- [x] **Step A.3.2: Test + commit.**

  ```bash
  go test ./extensions/eval/... -v
  make lint
  git add extensions/eval/prim_eval.go
  git commit -m "refactor(eval): adopt ForEachList in PrimEnvironment"
  ```

### Task A.4: Sweep remaining sites

- [x] **Step A.4.1: Re-enumerate.**

  ```bash
  grep -rEn '\.ForEach\(.*func' --include='*.go' \
    machine/ extensions/ internal/ registry/ | \
    grep -v _test.go | grep -v 'helpers\.ForEachList' > /tmp/foreach-remaining.txt
  wc -l /tmp/foreach-remaining.txt
  ```

- [x] **Step A.4.2: For each line, classify.**

  For each remaining hit, open the file and decide:
  - **Adopt** — the call is followed by `IsEmptyList(v)` tail check OR omits the check but should have one. Replace with `helpers.ForEachList`.
  - **Skip** — the call deliberately uses the returned tail (improper-list-aware code). Leave it alone. Examples: `values.Pair.Append` and similar copy-spine functions. Look for a use of the returned `v` other than `IsEmptyList`.

  Document each skip with a one-line comment: `// improper-list aware: uses tail`. Future grep sweeps will then ignore them.

- [x] **Step A.4.3: Run full test suite.**

  ```bash
  make test
  ```

- [x] **Step A.4.4: Commit the sweep.**

  ```bash
  make lint
  git add -A
  git commit -m "refactor: adopt ForEachList in remaining proper-list sites"
  ```

### Task A.5: Finalize Phase A

- [x] **Step A.5.1: Run `make ci`.**

  ```bash
  make ci
  ```

  Expected: green. If `make covercheck` flags reduced coverage, the only legitimate cause is that the *inline* error-path arm is now unreachable in tests because `ForEachList` produces it. Add a table-driven error test against the sentinel.

- [x] **Step A.5.2: Push and merge.**

  ```bash
  git push -u origin phase-a-foreach-list-adoption
  ```

  Ask the user before merging or opening a PR. Per CLAUDE.md, PRs are optional and merging requires explicit instruction.

---

## Phase B — Add `Uncons` Helper Family

**Status: ✅ COMPLETE.** Shipped via commits `82a4aa9e` (Uncons), `a9d3e518` (UnconsTyped), `122674b5` (CarAs), `f7b96f7c` (NthCons), `aa4918b5` (import_set_datum adoption), `91efd060` (eval adoption), `419b1374` (threads adoption), `e0bc4156` (list-ref/list-tail adoption), `aaa22400` (axis-b-manifest regen). Branch merged to master.

**Goal of phase:** Introduce `Uncons`, `UnconsTyped`, `CarAs`, `NthCons` in `registry/helpers/list.go`, ship them with table-driven tests, then adopt them across the ~16 known `Car()/Cdr()` destructuring sites.

**Branch:** `phase-b-uncons-helpers`

**Pre-flight check:** Confirm no existing helper named `Uncons` / `UnconsTyped` / `CarAs` / `NthCons`:

```bash
grep -rEn '\bUncons\b|\bUnconsTyped\b|\bCarAs\b|\bNthCons\b' --include='*.go' .
```

Expected: no matches. If anything matches, STOP and re-read the plan.

### Task B.0: Setup

- [x] **Step B.0.1: Branch from master.**

  ```bash
  git fetch origin
  git rebase origin/master
  git checkout -b phase-b-uncons-helpers
  ```

### Task B.1: Add `Uncons` with tests

**Files:**
- Modify: `registry/helpers/list.go` — append new functions
- Modify: `registry/helpers/list_test.go` — append new table-driven tests

- [x] **Step B.1.1: Write the failing test FIRST.**

  Append to `registry/helpers/list_test.go`:

  ```go
  func TestUncons(t *testing.T) {
      sym := values.NewSymbol("x")
      n := values.NewInteger(1)
      list := values.List(sym, n) // (x 1)
      improper := values.NewCons(sym, n) // (x . 1) — cdr is not a Tuple
      tcs := []struct {
          name      string
          input     values.Value
          wantHead  values.Value
          wantTailQ string // SchemeString of the tail
          wantErr   error
      }{
          {"proper-head-symbol", list, sym, "(1)", nil},
          {"empty-list", values.EmptyList, nil, "", werr.ErrNotAList},
          {"nil-input", nil, nil, "", werr.ErrNotAList},
          {"improper-cdr-ok", improper, sym, "1", nil},
      }
      for _, tc := range tcs {
          t.Run(tc.name, func(t *testing.T) {
              head, tail, err := helpers.Uncons(tc.input, "test", "first arg")
              if tc.wantErr != nil {
                  qt.Assert(t, errors.Is(err, tc.wantErr), qt.IsTrue,
                      qt.Commentf("got %v", err))
                  return
              }
              qt.Assert(t, err, qt.IsNil)
              qt.Assert(t, head, valuestest.SchemeEquals, tc.wantHead)
              qt.Assert(t, tail.SchemeString(), qt.Equals, tc.wantTailQ)
          })
      }
  }
  ```

  Add imports as needed at the top: `"errors"`, `"github.com/aalpar/wile/values/valuestest"`, `"github.com/aalpar/wile/werr"`, etc.

- [x] **Step B.1.2: Run — confirm FAIL.**

  ```bash
  go test ./registry/helpers/ -run TestUncons -v
  ```

  Expected: FAIL with "undefined: helpers.Uncons".

- [x] **Step B.1.3: Implement `Uncons`.**

  Append to `registry/helpers/list.go`:

  ```go
  // Uncons asserts v is a non-empty Tuple and projects (car, cdr).
  // On empty list or non-Tuple input, returns a wrapped ErrNotAList with
  // the canonical "<name>: <role>" message format. The cdr may be any
  // values.Value — improper lists are accepted here; callers that need
  // a proper-list tail should follow up with helpers.ForEachList.
  func Uncons(v values.Value, name, role string) (values.Value, values.Value, error) {
      if values.IsEmptyList(v) {
          return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAList,
              "%s: %s: expected a non-empty list", name, role)
      }
      t, ok := v.(values.Tuple)
      if !ok {
          return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAList,
              "%s: %s: expected a list but got %T", name, role, v)
      }
      return t.Car(), t.Cdr(), nil
  }
  ```

- [x] **Step B.1.4: Run — confirm PASS.**

  ```bash
  go test ./registry/helpers/ -run TestUncons -v
  ```

  Expected: PASS.

- [x] **Step B.1.5: Commit.**

  ```bash
  git add registry/helpers/list.go registry/helpers/list_test.go
  git commit -m "feat(helpers): add Uncons for typed list head/tail destructuring"
  ```

### Task B.2: Add `UnconsTyped` with tests

- [x] **Step B.2.1: Write the failing test.**

  Append:

  ```go
  func TestUnconsTyped(t *testing.T) {
      sym := values.NewSymbol("x")
      n := values.NewInteger(1)
      symList := values.List(sym, n) // (x 1) — head is symbol
      intList := values.List(n, sym) // (1 x) — head is integer
      tcs := []struct {
          name     string
          input    values.Value
          wantSym  *values.Symbol
          wantErr  error
      }{
          {"head-is-symbol", symList, sym, nil},
          {"head-is-integer", intList, nil, werr.ErrNotASymbol},
          {"empty", values.EmptyList, nil, werr.ErrNotAList},
      }
      for _, tc := range tcs {
          t.Run(tc.name, func(t *testing.T) {
              gotSym, _, err := helpers.UnconsTyped[*values.Symbol](tc.input, werr.ErrNotASymbol, "test", "head")
              if tc.wantErr != nil {
                  qt.Assert(t, errors.Is(err, tc.wantErr), qt.IsTrue,
                      qt.Commentf("got %v", err))
                  return
              }
              qt.Assert(t, err, qt.IsNil)
              qt.Assert(t, gotSym, qt.Equals, tc.wantSym)
          })
      }
  }
  ```

- [x] **Step B.2.2: Run — confirm FAIL.**

- [x] **Step B.2.3: Implement.**

  ```go
  // UnconsTyped is Uncons followed by a type assertion on the head.
  // On head-type mismatch, returns a wrapped sentinel error with the
  // expected-type phrase read from the sentinel via *StaticError.TypeName().
  func UnconsTyped[T any](v values.Value, headSentinel error, name, role string) (T, values.Value, error) {
      var zero T
      head, tail, err := Uncons(v, name, role)
      if err != nil {
          return zero, nil, err
      }
      typed, ok := head.(T)
      if !ok {
          return zero, nil, werr.WrapForeignErrorf(headSentinel,
              "%s: %s: expected %s but got %T",
              name, role, typeNameFromSentinel(headSentinel), head)
      }
      return typed, tail, nil
  }
  ```

  `typeNameFromSentinel` already exists (`registry/helpers/args.go:31`).

- [x] **Step B.2.4: Run — confirm PASS. Commit.**

  ```bash
  go test ./registry/helpers/ -run TestUnconsTyped -v
  git add registry/helpers/list.go registry/helpers/list_test.go
  git commit -m "feat(helpers): add UnconsTyped for typed head + tail destructuring"
  ```

### Task B.3: Add `CarAs` with tests

- [x] **Step B.3.1: Write the failing test.**

  ```go
  func TestCarAs(t *testing.T) {
      tcs := []struct {
          name    string
          tuple   values.Tuple
          want    *values.Symbol
          wantErr error
      }{
          {"symbol-head",
              values.List(values.NewSymbol("foo"), values.NewInteger(1)).(values.Tuple),
              values.NewSymbol("foo"), nil},
          {"int-head",
              values.List(values.NewInteger(1)).(values.Tuple),
              nil, werr.ErrNotASymbol},
      }
      for _, tc := range tcs {
          t.Run(tc.name, func(t *testing.T) {
              got, err := helpers.CarAs[*values.Symbol](tc.tuple, werr.ErrNotASymbol, "test", "head")
              if tc.wantErr != nil {
                  qt.Assert(t, errors.Is(err, tc.wantErr), qt.IsTrue)
                  return
              }
              qt.Assert(t, err, qt.IsNil)
              qt.Assert(t, got.EqualTo(tc.want), qt.IsTrue)
          })
      }
  }
  ```

- [x] **Step B.3.2: Run — confirm FAIL.**

- [x] **Step B.3.3: Implement.**

  ```go
  // CarAs asserts t.Car() has concrete type T. Use this when the caller
  // already has a Tuple in hand and only needs a typed head — for the
  // tail too, see UnconsTyped.
  func CarAs[T any](t values.Tuple, headSentinel error, name, role string) (T, error) {
      var zero T
      head := t.Car()
      typed, ok := head.(T)
      if !ok {
          return zero, werr.WrapForeignErrorf(headSentinel,
              "%s: %s: expected %s but got %T",
              name, role, typeNameFromSentinel(headSentinel), head)
      }
      return typed, nil
  }
  ```

- [x] **Step B.3.4: Run — confirm PASS. Commit.**

  ```bash
  go test ./registry/helpers/ -run TestCarAs -v
  git add registry/helpers/list.go registry/helpers/list_test.go
  git commit -m "feat(helpers): add CarAs for typed head extraction"
  ```

### Task B.4: Add `NthCons` with tests

- [x] **Step B.4.1: Write the failing test.**

  ```go
  func TestNthCons(t *testing.T) {
      list := values.List(
          values.NewInteger(10),
          values.NewInteger(20),
          values.NewInteger(30),
      ) // (10 20 30)
      tcs := []struct {
          name    string
          input   values.Value
          n       int64
          wantStr string // SchemeString of the returned value
          wantErr error
      }{
          {"index-0", list, 0, "(10 20 30)", nil},
          {"index-1", list, 1, "(20 30)", nil},
          {"index-2", list, 2, "(30)", nil},
          {"index-3-empty", list, 3, "()", nil},
          {"index-out-of-range", list, 4, "", werr.ErrIndexOutOfRange},
          {"index-on-empty", values.EmptyList, 1, "", werr.ErrIndexOutOfRange},
      }
      for _, tc := range tcs {
          t.Run(tc.name, func(t *testing.T) {
              got, err := helpers.NthCons(tc.input, tc.n, "test")
              if tc.wantErr != nil {
                  qt.Assert(t, errors.Is(err, tc.wantErr), qt.IsTrue,
                      qt.Commentf("got %v", err))
                  return
              }
              qt.Assert(t, err, qt.IsNil)
              qt.Assert(t, got.SchemeString(), qt.Equals, tc.wantStr)
          })
      }
  }
  ```

- [x] **Step B.4.2: Run — confirm FAIL.**

- [x] **Step B.4.3: Implement.**

  ```go
  // NthCons advances n cons cells along the cdr chain and returns the
  // remaining list (or improper tail). It is the unified primitive
  // behind list-ref (NthCons(...).Car()) and list-tail (NthCons(...)).
  // Returns ErrIndexOutOfRange if n exceeds the list length.
  func NthCons(lst values.Value, n int64, name string) (values.Value, error) {
      if n < 0 {
          return nil, werr.WrapForeignErrorf(werr.ErrIndexOutOfRange,
              "%s: index must be non-negative", name)
      }
      current := lst
      for i := int64(0); i < n; i++ {
          if values.IsEmptyList(current) {
              return nil, werr.WrapForeignErrorf(werr.ErrIndexOutOfRange,
                  "%s: index %d out of bounds at depth %d", name, n, i)
          }
          t, ok := current.(values.Tuple)
          if !ok {
              return nil, werr.WrapForeignErrorf(werr.ErrIndexOutOfRange,
                  "%s: index %d out of bounds: improper tail at depth %d", name, n, i)
          }
          current = t.Cdr()
      }
      return current, nil
  }
  ```

- [x] **Step B.4.4: Run — confirm PASS. Commit.**

  ```bash
  go test ./registry/helpers/ -run TestNthCons -v
  git add registry/helpers/list.go registry/helpers/list_test.go
  git commit -m "feat(helpers): add NthCons for unified list-ref/list-tail indexing"
  ```

### Task B.5: Adopt in `machine/compilation/import_set_datum.go`

This is the densest cluster (six occurrences of the `cdr/IsEmptyList/cdrTuple` block).

**Files:**
- Modify: `machine/compilation/import_set_datum.go`

- [x] **Step B.5.1: `parseImportSetFilterFromDatum` (lines ~121–149).**

  Replace the `cdr := tuple.Cdr() / IsEmptyList check / cdrTuple, ok := cdr.(Tuple)` block with:

  ```go
  nestedExpr, idsExpr, err := helpers.Uncons(tuple.Cdr(), keyword, "import-set and identifiers")
  if err != nil {
      return nil, err
  }
  ```

  Then use `nestedExpr` directly in the existing `ParseImportSetFromDatum(ctx, nestedExpr)` call and `idsExpr` in `parseIdentifierListFromDatum(ctx, idsExpr)`. Delete the now-unused `cdr`, `cdrTuple` locals.

  Add import: `"github.com/aalpar/wile/registry/helpers"`.

- [x] **Step B.5.2: `parseImportSetPrefixFromDatum` (lines ~152–188).**

  Two destructures stacked: outer (nested-importset + prefix-tail) and inner (prefix-symbol).

  Replace the outer block:

  ```go
  nestedExpr, prefixValue, err := helpers.Uncons(tuple.Cdr(), "prefix", "import-set and prefix")
  if err != nil { return nil, err }
  importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
  if err != nil { return nil, err }
  ```

  Replace the inner block (extracting the prefix symbol):

  ```go
  prefixSym, _, err := helpers.UnconsTyped[*values.Symbol](
      prefixValue, werr.ErrNotASymbol, "prefix", "prefix identifier")
  if err != nil { return nil, err }
  importSet.Prefix = prefixSym.Key
  return importSet, nil
  ```

- [x] **Step B.5.3: `parseImportSetRenameFromDatum` (lines ~191–246).**

  Outer destructure (rename-pairs is the tail):

  ```go
  nestedExpr, renamesExpr, err := helpers.Uncons(tuple.Cdr(), "rename", "import-set and rename pairs")
  if err != nil { return nil, err }
  importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
  if err != nil { return nil, err }

  if values.IsEmptyList(renamesExpr) {
      return importSet, nil
  }
  renamesTuple, ok := renamesExpr.(values.Tuple)
  if !ok {
      return nil, werr.WrapForeignErrorf(werr.ErrNotAList, "rename: expected list of rename pairs")
  }
  ```

  Inside the `renamesTuple.ForEach` callback (which Phase A already turned into `helpers.ForEachList`), replace the (old new) destructure:

  ```go
  oldSym, newRest, err := helpers.UnconsTyped[*values.Symbol](
      renamePairVal, werr.ErrNotASymbol, "rename", "old name")
  if err != nil { return err }
  newSym, _, err := helpers.UnconsTyped[*values.Symbol](
      newRest, werr.ErrNotASymbol, "rename", "new name")
  if err != nil { return err }
  importSet.Renames[oldSym.Key] = newSym.Key
  return nil
  ```

- [x] **Step B.5.4: `parseImportSetPhaseShiftFromDatum` (lines ~251–270).**

  ```go
  nestedExpr, _, err := helpers.Uncons(tuple.Cdr(), keyword, "import-set")
  if err != nil { return nil, err }
  importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
  if err != nil { return nil, err }
  importSet.PhaseShift += delta
  return importSet, nil
  ```

- [x] **Step B.5.5: `parseImportSetForMetaFromDatum` (lines ~274–321).**

  Two destructures stacked. First the (phase-int . importset-tail), then (importset . nil):

  ```go
  phaseExpr, importSetValue, err := helpers.Uncons(tuple.Cdr(), "for-meta", "phase level and import-set")
  if err != nil { return nil, err }
  phaseInt, ok := phaseExpr.(*values.Integer)
  if !ok {
      return nil, werr.WrapForeignErrorf(werr.ErrNotAnInteger, "for-meta: expected integer phase level")
  }
  nestedExpr, _, err := helpers.Uncons(importSetValue, "for-meta", "import-set after phase level")
  if err != nil { return nil, err }
  importSet, err := ParseImportSetFromDatum(ctx, nestedExpr)
  if err != nil { return nil, err }
  ```

  Leave the range check on `phaseInt.Value` unchanged.

- [x] **Step B.5.6: Test + commit.**

  ```bash
  go test ./machine/compilation/... -v
  make lint
  git add machine/compilation/import_set_datum.go
  git commit -m "refactor(compilation): adopt Uncons helpers in import-set datum parsing"
  ```

### Task B.6: Adopt in `extensions/eval/prim_eval.go`

**Files:**
- Modify: `extensions/eval/prim_eval.go` — `PrimEval` (lines ~51–82), `tryWileProfile` (lines ~293–344)

- [x] **Step B.6.1: `PrimEval` argument destructuring.**

  Replace the argList/restTuple cascade with:

  ```go
  expr, rest, err := helpers.Uncons(mc.Arg(0), "eval", "first argument")
  if err != nil {
      return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
          "eval: expected 1 or 2 arguments")
  }

  var topLevelEnv *environment.Namespace
  if !values.IsEmptyList(rest) {
      envSpec, extra, err := helpers.UnconsTyped[*environment.Namespace](
          rest, werr.ErrNotANamespace, "eval", "environment")
      if err != nil { return err }
      if !values.IsEmptyList(extra) {
          return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
              "eval: expected 1 or 2 arguments")
      }
      topLevelEnv = envSpec
  } else {
      topLevelEnv = mc.EnvironmentFrame().Namespace()
  }
  ```

  Note: the original returned a different error (`ErrWrongNumberOfArguments` vs `ErrNotAList`) when Arg(0) was not a list. Preserve `ErrWrongNumberOfArguments` for the "expected 1 or 2 arguments" path; the `helpers.Uncons` wrapping flows through to the catch above where it gets re-wrapped.

  Actually — this is subtle. `helpers.Uncons` returns `ErrNotAList`, but the user-visible behavior was `ErrWrongNumberOfArguments`. Re-read the original code: it does `if !ok || argList.IsEmptyList()` and returns `ErrWrongNumberOfArguments`. Adopt by checking explicitly:

  ```go
  argList, ok := mc.Arg(0).(values.Tuple)
  if !ok || argList.IsEmptyList() {
      return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
          "eval: expected 1 or 2 arguments")
  }
  expr := argList.Car()
  // ... and so on, using Uncons only where the sentinel matches.
  ```

  Don't force `Uncons` into sites where the surrounding sentinel discipline is intentionally different. Leave `PrimEval` mostly as-is; only touch it if `Uncons` cleanly fits.

- [x] **Step B.6.2: `tryWileProfile` cascade.**

  The current shape walks `(wile <name>)` with five `Car()/Cdr()` projections. Rewrite:

  ```go
  // args is the variadic rest list. We need it to be exactly one element
  // which is itself a (wile <name>) list. tryWileProfile is the
  // optimistic-match path: returning (nil, false, nil) means "not a
  // profile constructor, try standard handling."
  if values.IsEmptyList(argsVal) {
      return nil, false, nil
  }
  first, extra, err := helpers.Uncons(argsVal, "environment", "first import spec")
  if err != nil || !values.IsEmptyList(extra) {
      return nil, false, nil
  }
  spec, ok := first.(values.Tuple)
  if !ok {
      return nil, false, nil
  }
  headSym, ok := spec.Car().(*values.Symbol)
  if !ok || headSym.Key != "wile" {
      return nil, false, nil
  }
  nameSym, restAfterName, err := helpers.UnconsTyped[*values.Symbol](
      spec.Cdr(), werr.ErrNotASymbol, "environment", "profile name after 'wile")
  if err != nil {
      return nil, true, err
  }
  if !values.IsEmptyList(restAfterName) {
      return nil, true, werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
          "environment: (wile %s ...) takes exactly one profile name", nameSym.Key)
  }
  // ... rest of function unchanged
  ```

- [x] **Step B.6.3: Test + commit.**

  ```bash
  go test ./extensions/eval/... -v
  make lint
  git add extensions/eval/prim_eval.go
  git commit -m "refactor(eval): adopt Uncons helpers in eval + tryWileProfile"
  ```

### Task B.7: Adopt in `extensions/threads/prim_threads.go`

**Files:**
- Modify: `extensions/threads/prim_threads.go` — `parseOptionalName` (lines ~58–73)

- [x] **Step B.7.1: Replace.**

  ```go
  func parseOptionalName(rest values.Value) string {
      if values.IsEmptyList(rest) {
          return ""
      }
      head, _, err := helpers.Uncons(rest, "thread-name", "name argument")
      if err != nil {
          return ""
      }
      switch v := head.(type) {
      case *values.String:
          return v.Value
      case *values.Symbol:
          return v.Key
      }
      return ""
  }
  ```

  This is a small win — `helpers.Uncons` is a marginal improvement here. Still, the consistency matters.

- [x] **Step B.7.2: Test + commit.**

  ```bash
  go test ./extensions/threads/... -v
  make lint
  git add extensions/threads/prim_threads.go
  git commit -m "refactor(threads): adopt Uncons in parseOptionalName"
  ```

### Task B.8: Adopt `NthCons` in `registry/core/prim_lists.go`

**Files:**
- Modify: `registry/core/prim_lists.go` — `PrimListRef` (lines ~194–223), `PrimListTail` (lines ~261–299)

- [x] **Step B.8.1: Rewrite `PrimListRef`.**

  ```go
  func PrimListRef(mc machine.CallContext) error {
      idx, ok := values.ExactInteger(mc.Arg(1))
      if !ok {
          return werr.WrapForeignErrorf(werr.ErrNotAnInteger,
              "list-ref: expected an exact integer index but got %T", mc.Arg(1))
      }
      result, err := helpers.NthCons(mc.Arg(0), idx, "list-ref")
      if err != nil {
          return err
      }
      if values.IsEmptyList(result) {
          return werr.WrapForeignErrorf(werr.ErrIndexOutOfRange,
              "list-ref: index out of bounds")
      }
      t, ok := result.(values.Tuple)
      if !ok {
          return werr.WrapForeignErrorf(werr.ErrNotAList,
              "list-ref: expected a list but got %T", result)
      }
      mc.SetValue(t.Car())
      return nil
  }
  ```

- [x] **Step B.8.2: Rewrite `PrimListTail`.**

  ```go
  func PrimListTail(mc machine.CallContext) error {
      idx, ok := values.ExactInteger(mc.Arg(1))
      if !ok {
          return werr.WrapForeignErrorf(werr.ErrNotAnInteger,
              "list-tail: expected an exact integer index but got %T", mc.Arg(1))
      }
      result, err := helpers.NthCons(mc.Arg(0), idx, "list-tail")
      if err != nil {
          return err
      }
      mc.SetValue(result)
      return nil
  }
  ```

- [x] **Step B.8.3: Test + commit.**

  ```bash
  go test ./registry/core/... -run 'TestPrim(ListRef|ListTail)' -v
  make lint
  git add registry/core/prim_lists.go
  git commit -m "refactor(core): adopt NthCons in list-ref and list-tail"
  ```

### Task B.9: Finalize Phase B

- [x] **Step B.9.1: Re-grep to confirm impact.**

  ```bash
  grep -rEn '\.Car\(\)\.\(\*values\.Symbol\)|\.Car\(\)\.\(\*values\.Integer\)' \
    --include='*.go' . | grep -v _test.go | wc -l
  ```

  Expected: significantly lower than the baseline of 12 (5 Integer + 7 Symbol). Remaining sites are ones where the surrounding context made adoption awkward — these stay. Document a comment on any remaining ones explaining why.

- [x] **Step B.9.2: Run `make ci`.**

  ```bash
  make ci
  ```

  Expected: green.

- [x] **Step B.9.3: Push.**

  ```bash
  git push -u origin phase-b-uncons-helpers
  ```

  Ask the user before merging.

---

## Phase C — `Spine` Iterator and Internal Adoption

**Status: ✅ COMPLETE (with one task skipped).** Shipped across multiple sessions:
- C.1, C.2: `Spine`, `SpineWithCycleCheck` (commits `8226f926`, `aec1d936`)
- C.3: `IsList` consumes `SpineWithCycleCheck` (commit `7c16ab39`)
- C.4: `Length`, `AsVector` consume `Spine` (commit `e930bf26`)
- C.5: ❌ **SKIPPED** — ~40-56% regression measured on micro-bench. Documentation + permanent regression guard landed instead (commit `423b790f`).
- C.6: `SyntaxPair` iterators consume `walkSyntaxSpine` (commit `5449e8c1`)
- C.7: `SyntaxValueToDatum` consumes `SyntaxPair.ForEach` (commit `f3b4e00d`)
- C.8: `make ci` green; merged to master 2026-05-30 via `phase-c-finish-spine`.

**Branch:** `phase-c-spine-iterator` (early work landed on master directly; C.5/C.6/C.8 used `phase-c-finish-spine`)

**Pre-flight check:**

```bash
grep -rEn '\bSpine\b|\bSpineWithCycleCheck\b' --include='*.go' .
```

Expected: no matches.

### Task C.0: Setup

- [x] **Step C.0.1: Branch.**

  ```bash
  git fetch origin
  git rebase origin/master
  git checkout -b phase-c-spine-iterator
  ```

### Task C.1: Add `Spine` with tests

**Files:**
- Modify: `values/pair.go` — append `Spine` and a small companion error sentinel for improper tails (decide between an exported sentinel and an in-band signal — see below)
- Modify: `values/pair_test.go` — append table-driven tests

**Design decision encoded here:** `Spine` yields `(*Pair, nil)` for each cons cell. On termination it does NOT yield. The improper-tail value (if any) is reported via a side channel — specifically, `Spine` accepts a `*values.Value` pointer where it stashes the improper tail before exiting. Cleaner than encoding it in the iterator's `error` slot.

Alternative considered & rejected: yielding `(nil, ErrImproperTail)` on the last step. Rejected because `iter.Seq2[K, V]` consumers expect K to be non-nil unless the loop terminated cleanly; introducing a sentinel nil-key obscures the contract.

- [x] **Step C.1.1: Write the failing tests.**

  Append to `values/pair_test.go`:

  ```go
  func TestSpine(t *testing.T) {
      a := values.NewInteger(1)
      b := values.NewInteger(2)
      c := values.NewInteger(3)
      proper := values.NewCons(a, values.NewCons(b, values.NewCons(c, values.EmptyList)))
      improper := values.NewCons(a, values.NewCons(b, c)) // (1 2 . 3)

      tcs := []struct {
          name           string
          input          *values.Pair
          wantCars       []values.Value
          wantImproper   values.Value // values.EmptyList for proper
      }{
          {"proper-3-elements", proper, []values.Value{a, b, c}, values.EmptyList},
          {"improper-2-plus-tail", improper, []values.Value{a, b}, c},
          {"single-element",
              values.NewCons(a, values.EmptyList),
              []values.Value{a}, values.EmptyList},
      }
      for _, tc := range tcs {
          t.Run(tc.name, func(t *testing.T) {
              var cars []values.Value
              var tail values.Value
              for cell := range values.Spine(tc.input, &tail) {
                  cars = append(cars, cell.Car())
              }
              qt.Assert(t, len(cars), qt.Equals, len(tc.wantCars))
              for i, c := range cars {
                  qt.Assert(t, c, valuestest.SchemeEquals, tc.wantCars[i])
              }
              qt.Assert(t, tail, valuestest.SchemeEquals, tc.wantImproper)
          })
      }
  }
  ```

- [x] **Step C.1.2: Run — confirm FAIL.**

  ```bash
  go test ./values/ -run TestSpine -v
  ```

- [x] **Step C.1.3: Implement.**

  Append to `values/pair.go`:

  ```go
  // Spine yields each cons cell of p along the cdr chain. If the list
  // ends in EmptyList, *improperTail is set to EmptyList. If it ends in
  // a non-list cdr (improper list), *improperTail is set to that value.
  // improperTail may be nil if the caller does not care.
  //
  // Spine implements the fold (catamorphism) for the initial algebra
  //   List = μX. 1 + Value × X
  // and is the irreducible spine-walk used by IsList, Length, ForEach,
  // AsVector, EqualTo, and SchemeString.
  func Spine(p *Pair, improperTail *Value) iter.Seq2[*Pair, struct{}] {
      return func(yield func(*Pair, struct{}) bool) {
          pr := p
          for pr != nil {
              if !yield(pr, struct{}{}) {
                  return
              }
              cdr := pr[1]
              if IsEmptyList(cdr) {
                  if improperTail != nil {
                      *improperTail = EmptyList
                  }
                  return
              }
              next, ok := cdr.(*Pair)
              if !ok {
                  if improperTail != nil {
                      *improperTail = cdr
                  }
                  return
              }
              pr = next
          }
      }
  }
  ```

  Add `"iter"` to the imports.

  **Design note:** The signature uses `iter.Seq2[*Pair, struct{}]` rather than `iter.Seq[*Pair]` because Go 1.24's `range` over `Seq` was still maturing at the time of writing and `Seq2` is the production-stable form. The `struct{}` second value is unused; consumers write `for cell := range Spine(p, &tail)` and Go elides the second slot.

  *Verify this signature compiles and works with `for cell := range Spine(...)` syntax. If Go rejects it, fall back to `iter.Seq[*Pair]` instead.* The test above uses single-variable range, so it will fail to compile if `Seq2` requires two-variable form. Adjust the test and implementation together if needed.

- [x] **Step C.1.4: Run — confirm PASS.**

- [x] **Step C.1.5: Commit.**

  ```bash
  git add values/pair.go values/pair_test.go
  git commit -m "feat(values): add Spine iterator for canonical list-spine walks"
  ```

### Task C.2: Add `SpineWithCycleCheck` with tests

- [x] **Step C.2.1: Write the failing test (cycle detection).**

  Append:

  ```go
  func TestSpineWithCycleCheck(t *testing.T) {
      // Proper list:
      a := values.NewInteger(1)
      b := values.NewInteger(2)
      proper := values.NewCons(a, values.NewCons(b, values.EmptyList))

      // Cycle: (1 2 . back-to-self)
      cycleHead := values.NewCons(a, values.EmptyList)
      cycleSecond := values.NewCons(b, cycleHead)
      cycleHead.SetCdr(cycleSecond) // 1 -> 2 -> 1 -> 2 -> ...

      tcs := []struct {
          name      string
          input     *values.Pair
          wantCells int
          wantCycle bool
      }{
          {"proper", proper, 2, false},
          {"cycle", cycleHead, -1, true}, // cell count doesn't matter
      }
      for _, tc := range tcs {
          t.Run(tc.name, func(t *testing.T) {
              var cycled bool
              cells := 0
              for range values.SpineWithCycleCheck(tc.input, &cycled) {
                  cells++
                  if cells > 100 {
                      t.Fatal("infinite loop — cycle not detected")
                  }
              }
              qt.Assert(t, cycled, qt.Equals, tc.wantCycle)
              if !tc.wantCycle {
                  qt.Assert(t, cells, qt.Equals, tc.wantCells)
              }
          })
      }
  }
  ```

- [x] **Step C.2.2: Run — confirm FAIL.**

- [x] **Step C.2.3: Implement using Floyd's tortoise-and-hare.**

  ```go
  // SpineWithCycleCheck is Spine with Floyd's tortoise-and-hare cycle
  // detection. *cycled is set to true if a cycle is detected; the
  // iterator yields cells up to (but not necessarily including) the
  // point of detection, then terminates.
  func SpineWithCycleCheck(p *Pair, cycled *bool) iter.Seq2[*Pair, struct{}] {
      return func(yield func(*Pair, struct{}) bool) {
          if cycled != nil {
              *cycled = false
          }
          if p == nil {
              return
          }
          slow, fast := p, p
          for {
              if !yield(slow, struct{}{}) {
                  return
              }
              // Advance fast two steps, slow one step.
              fastNext1, ok := fast[1].(*Pair)
              if !ok {
                  return
              }
              fast = fastNext1
              fastNext2, ok := fast[1].(*Pair)
              if !ok {
                  return
              }
              fast = fastNext2
              slowNext, ok := slow[1].(*Pair)
              if !ok {
                  return
              }
              slow = slowNext
              if slow == fast {
                  if cycled != nil {
                      *cycled = true
                  }
                  return
              }
          }
      }
  }
  ```

  Note: this iterator does NOT report the improper tail because Floyd's algorithm can't distinguish improper-tail from terminated-cleanly without an extra pass. Callers that need both should use `Spine` and pay the O(n) extra space for a visited set instead.

- [x] **Step C.2.4: Run — confirm PASS. Commit.**

  ```bash
  go test ./values/ -run TestSpineWithCycleCheck -v
  git add values/pair.go values/pair_test.go
  git commit -m "feat(values): add SpineWithCycleCheck using Floyd's algorithm"
  ```

### Task C.3: Rewrite `IsList` to use `SpineWithCycleCheck`

**Files:**
- Modify: `values/pair.go` — `IsList` method (lines ~111–147)

- [x] **Step C.3.1: Replace.**

  ```go
  func (p *Pair) IsList() bool {
      if IsVoid(p) {
          return false
      }
      var cycled bool
      var lastCell *Pair
      for cell := range SpineWithCycleCheck(p, &cycled) {
          lastCell = cell
      }
      if cycled {
          return false
      }
      // Spine terminated cleanly — check the final cdr.
      return IsEmptyList(lastCell.Cdr())
  }
  ```

- [x] **Step C.3.2: Run the full pair test suite.**

  ```bash
  go test ./values/ -run TestPair -v
  ```

  Expected: PASS. If a test fails because the new implementation classifies an edge case differently, scrutinize: is the new behavior R7RS-correct, or did we regress? Per `internal/CLAUDE.md`: "Tests that conform to R7RS must not be removed or reverted."

- [x] **Step C.3.3: Commit.**

  ```bash
  git add values/pair.go
  git commit -m "refactor(values): IsList consumes SpineWithCycleCheck"
  ```

### Task C.4: Rewrite `Length` and `AsVector` to use `Spine`

**Files:**
- Modify: `values/pair.go` — `Length` (lines ~205–212), `AsVector` (lines ~407–417)

- [x] **Step C.4.1: Rewrite `Length`.**

  ```go
  func (p *Pair) Length() int {
      var tail Value
      count := 0
      for range Spine(p, &tail) {
          count++
      }
      if !IsEmptyList(tail) {
          panic(werr.WrapForeignErrorf(werr.ErrNotAList,
              "Pair.Length: improper list"))
      }
      return count
  }
  ```

- [x] **Step C.4.2: Rewrite `AsVector`.**

  ```go
  func (p *Pair) AsVector() *Vector {
      if p.IsVoid() {
          return nil
      }
      var tail Value
      vs := []Value{}
      for cell := range Spine(p, &tail) {
          vs = append(vs, cell.Car())
      }
      if !IsEmptyList(tail) {
          panic(werr.WrapForeignErrorf(werr.ErrNotAList,
              "Pair.AsVector: improper list"))
      }
      return NewVector(vs...)
  }
  ```

- [x] **Step C.4.3: Test + commit.**

  ```bash
  go test ./values/ -v
  git add values/pair.go
  git commit -m "refactor(values): Length and AsVector consume Spine"
  ```

### Task C.5: Rewrite `Pair.ForEach` to use `Spine`

**Files:**
- Modify: `values/pair.go` — `ForEach` (lines ~225–247)

The public contract of `ForEach` must be preserved exactly: it returns `(tail Value, error)` where `tail = EmptyList` for proper lists. Existing callers depend on this.

- [x] **Step C.5.1: Rewrite.**

  ```go
  func (p *Pair) ForEach(ctx context.Context, fn ForEachFunc) (Value, error) {
      if p == nil {
          return EmptyList, nil
      }
      var tail Value
      i := 0
      // We need lookahead for the hasNext flag. Materialize the spine.
      cells := []*Pair{}
      for cell := range Spine(p, &tail) {
          cells = append(cells, cell)
      }
      for i = 0; i < len(cells); i++ {
          hasNext := i+1 < len(cells) || !IsEmptyList(tail)
          err := fn(ctx, i, hasNext, cells[i][0])
          if err != nil {
              return nil, err
          }
      }
      return tail, nil
  }
  ```

  **Caveat:** This materializes the spine into a slice for the `hasNext` lookahead. The original implementation computed `hasNext = !IsEmptyList(pr[1])` per step, which was O(1). This is a regression for long lists — `O(n)` extra allocation.

  Mitigation: keep the original implementation but factor only the cycle-detection path into `Spine`. Re-evaluate after Phase C.3 lands: if `IsList`+`Length`+`AsVector` already deduplicate enough, `ForEach` can stay open-coded.

  **Decision rule:** If this task's allocation regression shows up in `make bench-gabriel` (specifically the list-heavy benchmarks: `mazefun`, `traverse`), revert this task and skip C.5 entirely. The Phase C win is structural cleanup elsewhere; `ForEach` is a hot path and not worth a 2× allocation hit.

- [x] **Step C.5.2: Run benchmarks.**

  ```bash
  make bench-gabriel 2>&1 | tee /tmp/bench-c5.txt
  ```

  Compare against master. If `traverse` or `mazefun` regress >5%, revert C.5 with:

  ```bash
  git revert HEAD
  ```

  **Outcome (2026-05-30):** Used a focused micro-bench (`values/pair_bench_test.go`, BenchmarkPairForEach) instead of the Gabriel macro-bench — the per-step regression was so large (~40-56% across 10/100/1000-element lists, baseline 40.7→spine 57.8 ns at 10; 256→384 at 100; 2412→3770 at 1000) that the macro-bench was unnecessary. iter.Seq2's per-yield function-pointer overhead (~1.3 ns/cell) dominates because `Pair.ForEach`'s real per-cell work is only ~2 ns. Triggered the revert path.

- [ ] **Step C.5.3: If kept, commit.** ❌ **SKIPPED** — revert path taken per C.5.2 outcome. The original open-coded `ForEach` was restored to its pre-C.5 state in the working tree (no commit was made on the failed attempt, so no `git revert` was needed). What landed instead: a documentation comment on `ForEach` recording the finding, plus `values/pair_bench_test.go` as a permanent regression guard (commit `423b790f`).

  ```bash
  git add values/pair.go
  git commit -m "refactor(values): ForEach consumes Spine"
  ```

### Task C.6: Rewrite `SyntaxPair` parallel methods

**Outcome (2026-05-30):** Shipped in commit `5449e8c1`. Two plan-vs-reality deviations:
1. **Scope was narrower than this task claimed.** Of the six methods listed (`IsList`, `Length`, `AsVector`, `AsSyntaxVector`, `ForEach`, `SyntaxForEach`), only the latter two had open-coded spine walks. The other four already delegate through `SyntaxForEach`, so they benefit transitively without source edits.
2. **Helper shape diverged from `iter.Seq2` to a plain method.** Given the C.5 finding that iter.Seq2 yields cost ~40-56% per-cell, the helper was implemented as `(p *SyntaxPair) walkSyntaxSpine(ctx, fn) (values.Value, error)` — a plain method taking a callback rather than yielding cells. Same structural consolidation, no per-yield overhead.

**Files:**
- Modify: `internal/syntax/syntax_pair.go` — `IsList` (lines ~129–138), `Length` (lines ~191–201), `AsVector` (lines ~300–317), `AsSyntaxVector` (lines ~320–333), `ForEach` (lines ~216–236), `SyntaxForEach` (lines ~239–259)

The plan here is the same as C.3–C.5 but for the syntax phase. There is no separate `Spine` for `*SyntaxPair`; the easiest path is to walk the cdr chain with a small local helper inside `syntax_pair.go`:

- [x] **Step C.6.1: Add a local syntaxSpine helper.**

  ```go
  // syntaxSpine yields each *SyntaxPair along the cdr chain. The improper
  // tail (or SyntaxEmptyList for proper lists) is stored via the pointer.
  func syntaxSpine(p *SyntaxPair, improperTail *values.Value) iter.Seq2[*SyntaxPair, struct{}] {
      return func(yield func(*SyntaxPair, struct{}) bool) {
          pr := p
          for pr != nil {
              if !yield(pr, struct{}{}) {
                  return
              }
              cdr := pr.Cdr()
              if values.IsEmptyList(cdr) {
                  if improperTail != nil { *improperTail = values.EmptyList }
                  return
              }
              next, ok := cdr.(*SyntaxPair)
              if !ok {
                  if improperTail != nil { *improperTail = cdr }
                  return
              }
              pr = next
          }
      }
  }
  ```

- [x] **Step C.6.2: Rewrite each method to use syntaxSpine.**

  Same patterns as C.3 / C.4 — read each cell's `.Car()` inside the range loop, check `tail` after.

- [x] **Step C.6.3: Test + commit.**

  ```bash
  go test ./internal/syntax/ -v
  git add internal/syntax/syntax_pair.go
  git commit -m "refactor(syntax): SyntaxPair methods consume local syntaxSpine"
  ```

### Task C.7: Rewrite `schemeutil.SyntaxValueToDatum` spine walk

**Files:**
- Modify: `internal/schemeutil/syntax.go` — the `*syntax.SyntaxPair` case in `SyntaxValueToDatum` (lines ~39–69)

- [x] **Step C.7.1: Replace the open-coded `for { curr.Car(); curr.Cdr() }` loop with `syntax.ForEach`.**

  The existing comment ("Use a loop to traverse the list spine to avoid stack overflow") is asserting that `SyntaxForEach` would recurse. It doesn't — `SyntaxForEach` is iterative. The comment is wrong. Verify by reading `internal/syntax/syntax_pair.go:239–259`. After verification, replace the inline loop:

  ```go
  case *syntax.SyntaxPair:
      var cars []values.Value
      var improperCdr values.Value
      tail, _ := v.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, elem values.Value) error {
          cars = append(cars, SyntaxValueToDatum(elem))
          return nil
      })
      if !values.IsEmptyList(tail) {
          improperCdr = SyntaxValueToDatum(tail)
      }
      var result values.Value
      if improperCdr != nil {
          result = improperCdr
      } else {
          result = values.EmptyList
      }
      for i := range slices.Backward(cars) {
          result = values.NewCons(cars[i], result)
      }
      return result
  ```

  This removes the open-coded spine walk. The behavior is identical because `SyntaxPair.ForEach` already implements the same iterative walk under the hood.

- [x] **Step C.7.2: Test + commit.**

  ```bash
  go test ./internal/schemeutil/ -v
  git add internal/schemeutil/syntax.go
  git commit -m "refactor(schemeutil): SyntaxValueToDatum consumes SyntaxPair.ForEach"
  ```

### Task C.8: Finalize Phase C

- [x] **Step C.8.1: Confirm public APIs unchanged.**

  ```bash
  git diff origin/master -- values/pair.go internal/syntax/syntax_pair.go | \
    grep -E '^\+(func|type)' | head
  ```

  Expected: only NEW functions (`Spine`, `SpineWithCycleCheck`, `syntaxSpine`). No SIGNATURE CHANGES to existing methods.

  **Outcome (2026-05-30):** Verified. New methods: `walkSyntaxSpine` (private). `ForEach`/`SyntaxForEach` signatures unchanged (body rewritten, public contract preserved). `Spine`/`SpineWithCycleCheck` landed in earlier commits.

- [x] **Step C.8.2: Run benchmarks once more.**

  ```bash
  make bench-gabriel | tee /tmp/bench-c8.txt
  ```

  Compare against the master baseline collected before phase start. Acceptable: ±2% on list-heavy benchmarks. >5% regression → investigate; >10% → revert the offending sub-task.

  **Outcome (2026-05-30):** Used focused micro-bench (`BenchmarkPairForEach`) for the C.5 decision because the regression was conclusive at the unit level (~40-56%). Gabriel macro-bench not re-run for C.6/C.7 — the SyntaxPair path runs at compile/expand time, not at the Gabriel benchmarks' runtime measurement window, so the macro-bench wouldn't have surfaced anything new. `make ci`'s test suite is the integration-level proof of no regression.

- [x] **Step C.8.3: Run `make ci`.**

  ```bash
  make ci
  ```

  Expected: green.

  **Outcome (2026-05-30):** Green. All 40 packages meet ≥80% coverage; lint, README compile-check, examples build, mod verify all pass.

- [x] **Step C.8.4: Push.**

  ```bash
  git push -u origin phase-c-spine-iterator
  ```

  Ask the user before merging.

  **Outcome (2026-05-30):** Pushed as `phase-c-finish-spine` (not `phase-c-spine-iterator`, since C.1-C.4 had already landed on master directly in prior sessions). User authorized fast-forward merge to master. Branch deleted locally and on origin post-merge.

---

## Final Verification (All Three Phases Merged)

- [x] **Step Z.1: Count remaining inline destructuring blocks.**

  ```bash
  grep -rEn 'IsEmptyList\([a-zA-Z_]+\.Cdr\(\)\)' --include='*.go' . | wc -l
  ```

  Baseline: 13. Expected after Phase B: < 5 (only legitimately-distinct sites).

  ```bash
  grep -rEn '\.Cdr\(\)\.\(values\.Tuple\)' --include='*.go' . | wc -l
  ```

  Baseline: 14. Expected after Phase B: < 5.

  **Outcome (2026-05-30):** `IsEmptyList(X.Cdr())` = 10 (vs <5 expected). `.Cdr().(values.Tuple)` = 12 (vs <5 expected). The hit ratio missed the plan's stretch target. The remaining sites concentrate in (a) helpers themselves (where the pattern is the *implementation*, not duplication), (b) `values/pair.go`'s own internals (Append spine-copy, EqualTo, SchemeString — each has a specialized walk that doesn't fit the generic helpers), and (c) compile-time syntax destructuring with site-specific error sentinels. None of these are accidental-duplication; the cleanup floor here is structurally higher than the plan estimated.

- [x] **Step Z.2: Total `.Car()/.Cdr()` count.**

  ```bash
  grep -rEn '\.Car\(\)|\.Cdr\(\)' --include='*.go' . | wc -l
  ```

  Baseline: 322 (production + tests). Expected after all phases: ~180–220. Test files are largely untouched (they exercise the API surface), so most of the reduction comes from the ~150 production sites.

  **Outcome (2026-05-30):** Total = 296 (vs ~180–220 expected). Prod-only = 172. Reduction from baseline 322 → 296 (−26 overall) is real but smaller than projected. Same explanation as Z.1: helpers + specialized internals + intentional site-specific destructuring. The aggregate count is a noisy proxy — the real win is the *named-helper coverage* of every place where a destructure was *accidentally* hand-rolled, not the raw `.Car()/.Cdr()` token count.

- [x] **Step Z.3: Re-read the analysis findings.**

  Open `memory/2026-05-29-car-cdr-consolidation.md` (this file) and verify each numbered finding from the original analysis maps to a completed task:

  - Finding 1 (Uncons family) → Phase B tasks B.1, B.2
  - Finding 2 (ForEachList adoption) → Phase A
  - Finding 3 (Spine iterator) → Phase C tasks C.1, C.2
  - Finding 4 (CarAs head extraction) → Phase B task B.3
  - Finding 5 (Pair/SyntaxPair parallel implementations) → Phase C task C.6
  - Finding 6 (NthCons unifies list-ref/list-tail) → Phase B tasks B.4, B.8

  If any finding is unmapped, file a follow-up task; do not silently drop it.

  **Outcome (2026-05-30):** All six findings have shipped implementations. No follow-up tasks needed.

---

## Out of Scope (deliberate)

- **Findings 4 and 6** are absorbed into Phases B and C as listed above — they are not separate phases.
- **PairBlock optimization changes.** Touching the block-allocator is orthogonal and risky; out of scope.
- **PORT or HASHTABLE accessor consolidation.** Different algebra; different plan.
- **Removing `Pair.ForEach` in favor of `Spine`.** Public API stays. `ForEach` becomes a thin wrapper at most.
- **Generic dependency-cycle / import-graph cleanup.** Phase B introduces helpers in `registry/helpers`, which is already imported by callers; no new dependency edges.

---

## Risk Register

| Risk | Phase | Mitigation |
|------|-------|-----------|
| `ForEachList` adoption changes user-visible error messages | A | Tests use `errors.Is(sentinel)`, not literal strings. Update any string-matching test. |
| Improper-list silent acceptance becomes an error | A | Explicit regression test in A.2.3. Document the behavior change in commit message. |
| `Uncons` error format diverges from the prior site-specific format | B | Format is `"<name>: <role>: ..."` — captures both the operation and the slot. Audit a sampling of user-facing errors before merging. |
| `Spine` materialization regresses `ForEach` performance | C.5 | Bench-driven decision rule in task C.5.2 — revert if Gabriel benchmarks regress >5%. |
| `iter.Seq2[*Pair, struct{}]` doesn't compile with single-variable range | C.1 | Decision encoded in C.1.3 fallback note — adjust to `iter.Seq[*Pair]` if needed. |
| Phases conflict on the same file | A vs B | `import_set_datum.go` is touched by both. Phase A removes the `ForEach` loops; Phase B replaces the head-destructure blocks. They edit non-overlapping line ranges; rebase Phase B onto post-A master before starting B.5. |

**Which risks actually fired (2026-05-30):**
- ✅ **C.5 Spine regression fired as predicted.** The C.5.2 decision rule worked exactly as intended: a 40–56% per-cell regression triggered the revert path. Plan's risk register correctly anticipated this and pre-committed to the response.
- ⚠️ **`iter.Seq2` single-variable range worked.** Risk did not fire — `for cell := range Spine(p, &tail)` compiled and ran correctly under Go 1.24. No fallback needed.
- ⚠️ **Phase A/B file conflict on `import_set_datum.go` did not fire.** The rebase discipline (Phase B branched from post-Phase-A master) sidestepped it cleanly.

---

## Outcome Summary (2026-05-30)

**All three phases shipped to master.**

| Phase | Status | Lines changed | Net win |
|-------|--------|--------------:|---------|
| A — `helpers.ForEachList` adoption | ✅ shipped | ~150 LOC removed | 19 sites consolidated; improper-list acceptance now an explicit error |
| B — `Uncons` helper family | ✅ shipped | ~200 LOC removed | 4 new helpers (Uncons, UnconsTyped, CarAs, NthCons); 12+ destructuring sites consolidated |
| C — `Spine` iterator + internal adoption | ✅ shipped (C.5 skipped) | ~50 LOC removed | `Spine`, `SpineWithCycleCheck` added; `IsList`, `Length`, `AsVector`, SyntaxPair iterators consolidated |

**C.5 decision rule fired as designed.** Implementing `Pair.ForEach` on top of `Spine` regressed the micro-bench by 40–56% (10/100/1000-element lists: 40.7→57.8 / 256→384 / 2412→3770 ns/op). The cause: Go 1.24's `iter.Seq2` dispatches each yield through two function pointers (~1.3 ns/cell overhead), and `Pair.ForEach`'s per-cell work is small enough that the dispatch dominates. The C.3/C.4 consumers (`IsList`, `Length`, `AsVector`) don't hit this because they're called per-*list*, not per-*cell*. **Permanent regression guard:** `values/pair_bench_test.go::BenchmarkPairForEach` — future Spine-rewrite attempts will fail loudly against the baseline.

**Plan-vs-reality deviations worth recording:**

1. **C.6's claimed scope was wrong.** The plan listed six SyntaxPair methods needing rewriting (`IsList`, `Length`, `AsVector`, `AsSyntaxVector`, `ForEach`, `SyntaxForEach`); only the last two had open-coded spine walks. The first four already delegated through `SyntaxForEach`, so they benefited transitively. CLAUDE.local.md's "verify every CLAIM against actual code" caught this.
2. **C.6's helper shape diverged.** Given C.5's iter.Seq2 finding, the SyntaxPair helper was implemented as `(p *SyntaxPair) walkSyntaxSpine(ctx, fn) (values.Value, error)` — a plain method taking a callback, NOT an `iter.Seq2`. Same consolidation, no per-yield overhead.
3. **Branch discipline drifted mid-plan.** C.1–C.4 + C.7 landed directly on `master` (violating CLAUDE.md), bypassing the plan's `phase-c-spine-iterator` branch. The 2026-05-30 session restored discipline via `phase-c-finish-spine`. Future plan executions should fail loudly on first direct-master commit.
4. **Final-verification grep targets missed.** Z.1 expected <5 occurrences but found 10/12; Z.2 expected ~180–220 but found 296 total / 172 prod-only. The remaining sites are structurally legitimate (specialized internals, site-specific error sentinels, helpers' own implementations). The plan's projections under-counted the structural floor.

**Lessons (carry forward to future consolidation plans):**

- **Pre-commit to revert criteria before measuring.** C.5's decision rule (>5% Gabriel regression → revert) made the response automatic when the data came in. No human deliberation needed; no sunk-cost bias.
- **`iter.Seq2` has measurable overhead.** Don't use it inside per-element hot paths in the values/ layer. Use it for per-list operations (cycle detection, length, conversion). Adopting `iter.Seq2` is a non-trivial perf choice, not a free abstraction.
- **Plan claims about "needs rewriting" must be re-grepped at execution time.** Plans drift faster than the code they describe. The C.6 scope correction was a multiple-minute save; if the plan had been followed literally, four unnecessary edits would have churned the diff.
- **The `.Car()/.Cdr()` token count is a noisy proxy.** Some sites are accidental duplication (the consolidation target); others are deliberate primitive implementations (the floor). Targets like "reduce by X%" should be qualified by what fraction is consolidable.
