# LOW Priority Remaining Fixes

**Status:** Effectively complete — 15/19 fixed, 4 deferred

---

## Deferred Issues

| Issue | Location | Problem | Priority | Defer Until |
|-------|----------|---------|----------|-------------|
| L3 | `values/channel.go:253` | `ChannelSelect` busy-spins without `reflect.Select` | LOW | User reports issue or channel redesign |
| L11 | `internal/extensions/eval/prim_eval.go:35` | `eval` doesn't inherit dynamic context (parameterize) | MEDIUM | R7RS test suite or user reports |
| L15 | `internal/extensions/threads/prim_threads.go:214` | `thread-sleep!` ignores context cancellation | MEDIUM | Shutdown behavior matters |
| L19 | `internal/tokenizer/tokenizer.go:2280` | `isExtendedExponentMarkerForRadix` ignores radix | LOW | R7RS test suite requires |

### Fix Sketches

**L3:** Replace polling loop with `reflect.Select` for efficient blocking.

**L11:** Inherit parameter stack from caller in `PrimEval` sub-context. Non-trivial — parameter system is complex.

**L15:** Replace `time.Sleep(duration)` with `select { case <-time.After(duration): ... case <-ctx.Done(): return ctx.Err() }`.

**L19:** Add `if radix != 10 { return false }` to reject exponent markers in non-decimal radixes.
