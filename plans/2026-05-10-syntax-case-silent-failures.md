# `compilation/operation_syntax_case.go` silent-failure cleanup

**Date**: 2026-05-10
**Source**: `pr-review-toolkit:silent-failure-hunter` lens during PR #731 crosscheck (4 findings, all pre-existing — flagged because the file was in scope, not introduced by that PR)
**Status**: Implementation in progress on `fix/operation-syntax-case-silent-failures`
**Priority**: Medium-High — two HIGH findings (real silent corruption + error swallow), two MED (poor error messages)

## Why this plan exists

PR #731's silent-failure-hunter agent surfaced four error-handling defects in
`machine/compilation/operation_syntax_case.go`. The findings were
**out-of-scope** for that PR (a type-narrowing refactor) and were tracked as
a single TODO entry for follow-up. This plan scopes the cleanup as a
cohesive sweep — same file, same theme (error-handling quality in
syntax-case expansion), reviewable as one unit.

## Verification before action

Two of the agent's prescriptions needed correction after reading the
actual code (the *"Verification & Claims"* discipline from CLAUDE.md):

| Agent claim | Reality | Effect on plan |
|---|---|---|
| *"define a `match.ErrNoMatch` sentinel in `internal/match`"* | `match.ErrNotAMatch` already exists (re-export of `werr.ErrNotAMatch` at `internal/match/syntax_compiler.go`). The bug is **not** "no sentinel" but "call site doesn't use `errors.Is`". | Phase 1 simplifies to a 3-line site-local change. No cross-package edits. |
| *"`MaybeCreateLocalBinding` error return discarded with `_`"* | `MaybeCreateLocalBinding` returns `(*LocalIndex, bool)` — **not** `(*LocalIndex, error)`. The `_` is a `created vs. existed` flag. There is no error to discard. | Phase 3 reframes as "guard against nil-value bind when pattern var is declared but not matched" — the actual bug. |

Findings 3 and 4 were correctly diagnosed as written.

## Findings

### Finding 1 — Match-error swallow at `operation_syntax_case.go:94-102`

**Severity**: HIGH
**Where**: `machine/compilation/operation_syntax_case.go:94-102`
**Current shape**:
```go
err := matcher.Match(mc.Context(), input)
if err != nil {
    // Match failed
    mc.SetValue(values.FalseValue)
    mc.IncrPC()
    // Intentionally clear the matcher error: a failed match is normal control flow for syntax-case,
    // so we record #f in the value register and return no runtime error.
    return mc, nil // nolint:errcheck, nilerr
}
```

**Bug**: `matcher.Match` returns errors for *multiple* reasons:
- **Expected**: `match.ErrNotAMatch` (pattern doesn't match — normal control flow).
- **Unexpected**: `ctx.Err()` (context cancellation — `match.go:267`), malformed input,
  ellipsis-depth invariant violations, internal matcher bugs, future error variants.

All collapse to "set #f and continue", masking real failures as "no matching clause"
when the next operation eventually emits `OperationSyntaxCaseNoMatch`. Context
cancellation in particular gets silently translated to "no match" — a user
hitting a deadline-exceeded gets a misleading macro-expansion error instead of
the cancellation diagnostic.

The `nolint:nilerr` comment is the smoking gun: the `nilerr` linter detected
exactly this pattern ("returns nil despite having a non-nil error in scope")
and was silenced rather than addressed.

**Fix**:
```go
err := matcher.Match(mc.Context(), input)
if errors.Is(err, match.ErrNotAMatch) {
    mc.SetValue(values.FalseValue)
    mc.IncrPC()
    return mc, nil
}
if err != nil {
    return nil, mc.WrapError(err, "syntax-case: matcher error")
}
// matched — fall through to bind capture
```

**Why this works**:
- `match.ErrNotAMatch` is the documented expected return (per `internal/match/match.go:30, 145` doc comments).
- All other errors propagate via the project's `mc.WrapError` boundary helper, surfacing the real diagnostic.
- The `nolint:nilerr` comment becomes obsolete and can be deleted.

**Files touched**: `machine/compilation/operation_syntax_case.go` only. Add
import for `errors` (stdlib) if not already present.

**Estimated size**: XS (3 lines + import).

### Finding 2 — Type-assertion `_` discard conflates nil-vs-mismatch

**Severity**: MEDIUM
**Where**: `operation_syntax_case.go:81, 142, 220` (three sites)
**Current shape (representative)**:
```go
sc, _ := mc.SyntaxCaseState().(*syntaxCaseState)
if sc == nil || sc.input == nil {
    return nil, mc.Error("syntax-case: no input available")
}
```

**Bug**: The pattern conflates two distinct failure modes:
- *Field never set*: `mc.SyntaxCaseState() == nil` (no syntax-case expansion in
  flight) — produces "no input available". Reasonable.
- *Field set to wrong concrete type*: type assertion fails (`ok == false`),
  `sc` becomes nil, same error message — misleading.

The marker-interface revert (PR #731 Q-c) means the field is `any`-typed,
so a wrong type *can* in principle be stored without compile-time rejection.
The encapsulation argument from PR #731 says no production caller will do
this, but the assertion-discard pattern silently degrades the diagnostic if
it ever happens.

**Fix** (apply at lines 81, 142, 220):
```go
raw := mc.SyntaxCaseState()
if raw == nil {
    return nil, mc.Error("syntax-case: no input available (state field not set)")
}
sc, ok := raw.(*syntaxCaseState)
if !ok {
    return nil, mc.Error(fmt.Sprintf(
        "syntax-case: unexpected state type %T on MachineContext.syntaxCase", raw))
}
// proceed with sc.input / sc.bindings / sc.matcher checks per site
```

**Estimated size**: S (3 sites, mechanical pattern).

### Finding 3 — Bind-loop nil-value silent corruption

**Severity**: HIGH
**Where**: `operation_syntax_case.go:153-164` (`OperationBindPatternVars.Apply`)
**Current shape**:
```go
for _, varName := range p.PatternVars {
    sym := values.NewSymbol(varName)
    li, _ := childEnv.MaybeCreateLocalBinding(sym, environment.BindingTypeVariable, nil, nil)
    stxVal, ok := sc.bindings[varName]
    if ok && li == nil {
        continue
    }
    err := childEnv.SetLocalValue(li, stxVal)
    if err != nil {
        return nil, mc.WrapError(err, fmt.Sprintf("syntax-case: failed to bind pattern variable %s", varName))
    }
}
```

**Bug — actual mechanism** (corrected from the original review's claim of
"discarded error"; `MaybeCreateLocalBinding` returns `(*LocalIndex, bool)`,
not an error tuple):

The loop has four possible (li, ok) states; only two are handled correctly:

| `li`     | `ok`  | Current behavior                | Should be                                                |
|----------|-------|---------------------------------|----------------------------------------------------------|
| non-nil  | true  | `SetLocalValue(li, stxVal)` ✓   | (unchanged — happy path)                                 |
| nil      | true  | `continue` ✓                    | (unchanged — outer scope already has binding, skip)      |
| nil      | false | `SetLocalValue(nil, ...)` ✗     | error: pattern var declared but missing from bindings    |
| non-nil  | false | `SetLocalValue(li, nil)` ✗      | error: same — binding got created but no value to write  |

Both `ok == false` cases are silent corruption: a pattern variable in
`p.PatternVars` is by construction expected to have a binding in
`sc.bindings`. If it doesn't, that's a contract violation between the
matcher and the bind operation — should produce a diagnostic, not nil.

**Fix**:
```go
for _, varName := range p.PatternVars {
    sym := values.NewSymbol(varName)
    li, _ := childEnv.MaybeCreateLocalBinding(sym, environment.BindingTypeVariable, nil, nil)
    stxVal, ok := sc.bindings[varName]
    if !ok {
        return nil, mc.Error(fmt.Sprintf(
            "syntax-case: pattern variable %s missing from match bindings", varName))
    }
    if li == nil {
        // Outer scope already binds this name (or no local frame).
        // The variable resolves to the outer binding; skip the local set.
        continue
    }
    err := childEnv.SetLocalValue(li, stxVal)
    if err != nil {
        return nil, mc.WrapError(err, fmt.Sprintf("syntax-case: failed to bind pattern variable %s", varName))
    }
}
```

The three branches separate cleanly:
1. **Pattern var missing from match bindings** → contract violation, error.
2. **Binding exists at outer scope** → skip (R7RS hygiene allows this).
3. **Local binding to set** → set or report error from setter.

**Estimated size**: S (one site, the bug is one branch reordering).

### Finding 4 — Generic error messages lack actionable context

**Severity**: MEDIUM
**Where**: `operation_syntax_case.go:83, 144, 195, 222`
**Current shape**:
- L83 (also L142, L220 after Finding 2): `"syntax-case: no input available"`
- L144: `"syntax-case: no pattern bindings available"` (after Finding 2's split, this becomes its own message at 142)
- L195: `"syntax-case: no matching clause"` — emitted by `OperationSyntaxCaseNoMatch`
- L222: `"syntax: no pattern matcher available for template expansion"`

**Bug**: R7RS macro debugging is hard precisely because the output is
"syntax". Stripping the actual input from these messages forces users
into trial-and-error.

**Fix priorities** (apply discriminately — full-input dumps can themselves
be noisy):
- L83/142/220 (no input/bindings/matcher): include source location if
  available via `mc.SourceLocation()`. The state field being nil means
  no current expansion — caller-side context is the relevant info.
- L195 (`OperationSyntaxCaseNoMatch`): include the input form. This is
  *the* most-debugged macro error in practice.
  ```go
  func (p *OperationSyntaxCaseNoMatch) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
      raw := mc.SyntaxCaseState()
      if sc, ok := raw.(*syntaxCaseState); ok && sc.input != nil {
          return nil, mc.Error(fmt.Sprintf(
              "syntax-case: no matching clause for input %s", sc.input.SchemeString()))
      }
      return nil, mc.Error("syntax-case: no matching clause (input unavailable)")
  }
  ```
- L222: similarly include source location if available.

**Caveat**: The `mc.SourceLocation()` API needs verification — confirm it
exists and what it returns before using. If it doesn't expose what we
need, scope to L195 only and defer the others.

**Estimated size**: S–M (depends on `mc.SourceLocation()` availability;
~5 sites, simple enrichments).

## Phasing

Each phase produces one commit. Independent — can ship in any order;
recommended sequence is the order of risk-to-payoff:

| Phase | Finding | Size | Notes                                                        |
|-------|---------|------|--------------------------------------------------------------|
| 1     | 1       | XS   | Smallest diff, biggest correctness win (context cancellation no longer swallowed) |
| 2     | 2       | S    | Three identical site fixes — one mechanical pattern          |
| 3     | 3       | S    | One site, branch reordering                                  |
| 4     | 4       | S–M  | Last — depends on `mc.SourceLocation()` availability check  |

## Tests

Each phase needs a regression test demonstrating the new error path:

- **Phase 1**: a context-cancellation test — start a syntax-case match,
  cancel the context mid-expansion, expect the cancellation to surface
  as an error (not swallowed as no-match).
- **Phase 2**: an "unexpected state type" test — store an arbitrary
  non-`*syntaxCaseState` value via `SetSyntaxCaseState` (test-only,
  bypasses the encapsulation), expect the new "unexpected state type %T"
  diagnostic.
- **Phase 3**: a contract-violation test — stage `sc.bindings` to omit a
  pattern variable that's in `PatternVars`, expect "missing from match
  bindings" error rather than silent nil-bind.
- **Phase 4**: a no-match input-context test — assert that the
  `OperationSyntaxCaseNoMatch` error message includes the input form's
  string representation.

Tests live in `machine/compilation/operation_syntax_case_test.go`.

## Done criteria

- All four phases shipped on the same branch, each with a commit and a test.
- `make ci` passes locally (lint, test, covercheck).
- Pre-existing scheme-level tests
  (`machine/compilation/syntax_case_scheme_test.go`) continue to pass —
  this is a *no-functional-regression* sweep; legitimate macro
  expansion paths are unchanged.
- The `nolint:errcheck, nilerr` comment at the original site (L101) is
  removed, not silenced — the linter no longer has cause to flag.

## Cross-references

- TODO.md Tier 1 entry — the umbrella tracking item.
- PR #731 review aggregation — original silent-failure-hunter agent
  output (in conversation transcript, not a checked-in artifact).
- `internal/match/syntax_compiler.go` — defines `ErrNotAMatch` re-export
  (pre-existing — Phase 1 consumer-only).
- `internal/match/match.go:267` — where `ctx.Err()` is returned by the
  matcher (the cancellation case Phase 1 must propagate).
- `environment/environment_frame.go` — `MaybeCreateLocalBinding`
  signature reference (returns `(*LocalIndex, bool)`, not error tuple —
  contradicts original review claim, see "Verification before action"
  section above).
- `machine/compilation/syntax_case_scheme_test.go` — existing
  scheme-level happy-path coverage; regression guarantee for the sweep.
