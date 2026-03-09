# Machine Package Technical Debt Reduction

**Status:** Complete (PR #444)

**Goal:** Eliminate confirmed structural debt in the `machine/` package — duplicated logic, oversized files, dead aliases, stale comments, divergent code paths.

---

## Completed Phases

| Phase | Summary |
|-------|---------|
| 1. Quick Wins | Removed `EffectiveOperations` backward-compat alias, cleaned stale TODOs in `syntax_rules_test.go` |
| 2. Arity Dedup | Extracted `checkArity`/`bindArgs` into `arity.go`, unifying 3 arity-check + binding sites |
| 3. Closure Extraction | Moved closure compilation infrastructure (~150 lines) from `compile_validated.go` to `compile_closure.go` |
| 4. Expander Decomposition | Moved body-processing logic (~170 lines) from `expander_time_continuation.go` to `expander_body.go` |
| 5. Letrec* Unification | Extracted shared `predeclareBinding` into `letrec_semantics.go`; generic `LetrecPreScanner` evaluated and rejected (type heterogeneity makes abstraction more complex than duplication) |
| 6. Library Import | Extracted `findLibraryBinding` helper, deduplicating binding search in library import paths |

---

## Items NOT Included (Assessed and Deferred)

| Finding | Why Deferred |
|---------|-------------|
| **Operation struct boilerplate** (272 lines across 8 zero-field ops) | Type safety and IDE navigability outweigh the boilerplate cost. Code generation is an option if more operations are added, but the current count (8) doesn't justify it. |
| **VMCounters.String() hand-unrolled** (25 format args) | Changes rarely. Reflection-based alternatives would be slower and harder to read. |
| **NativeTemplate literal dedup O(n) fallback** | Theoretical scaling wall for non-hashable values. No evidence of real-world impact. |
| **Operation naming verbosity** | Names encode instruction format. Renaming would touch many test files for cosmetic benefit. |
| **MachineContext decomposition** (1669 lines) | Already tracked in TODO.md as F10. Depends on other refactorings settling. |
| **CompileDefineLibrary callback pattern** | The `SetLibraryCallback` side-channel is a kludge but changing the return type requires coordinating loader and compiler signatures. Medium risk, low urgency. |
