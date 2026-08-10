// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Package gorules defines custom lint rules for the Wile project.
// These rules are loaded by gocritic's ruleguard checker at lint time.
//
// See: https://github.com/quasilyte/go-ruleguard
package gorules

import "github.com/quasilyte/go-ruleguard/dsl"

// noCompoundIf flags if-statements that use an init clause.
// Project convention: separate the init and the condition into
// distinct statements for readability.
//
//	// Wrong:
//	if err := f(); err != nil { ... }
//
//	// Right:
//	err := f()
//	if err != nil { ... }
func noCompoundIf(m dsl.Matcher) { //nolint:unused // loaded by gocritic ruleguard checker at lint time
	m.Match(`if $init; $cond { $*_ }`).
		Report(`compound if-init statement: separate "$init" from the condition`)
}

// noErrorsNew flags uses of errors.New in production code.
// Project convention: use werr.WrapForeignErrorf(sentinel, msg) or
// werr.NewForeignErrorf(msg) instead, so callers can match with errors.Is.
// Skips test files — tests legitimately create ad-hoc errors.
func noErrorsNew(m dsl.Matcher) { //nolint:unused // loaded by gocritic ruleguard checker at lint time
	m.Match(`errors.New($msg)`).
		Where(!m.File().Name.Matches(`_test\.go$`)).
		Report(`use werr.WrapForeignErrorf(sentinel, msg) instead of errors.New`)
}

// noFmtErrorf flags uses of fmt.Errorf in production code.
// Project convention: use werr.WrapForeignErrorf(sentinel, msg, args...)
// or werr.NewForeignErrorf(msg, args...) instead.
// Skips test files — tests legitimately create ad-hoc errors.
func noFmtErrorf(m dsl.Matcher) { //nolint:unused // loaded by gocritic ruleguard checker at lint time
	m.Match(`fmt.Errorf($*args)`).
		Where(!m.File().Name.Matches(`_test\.go$`)).
		Report(`use werr.WrapForeignErrorf(sentinel, msg, args...) instead of fmt.Errorf`)
}

// noBareSentinelPanic flags panic calls with bare werr sentinel errors.
// Project convention: always wrap with WrapForeignErrorf for site context.
// Skips test files — tests legitimately panic with bare sentinels.
//
//	// Wrong:
//	panic(werr.ErrNotAList)
//
//	// Right:
//	panic(werr.WrapForeignErrorf(werr.ErrNotAList, "site: what failed"))
func noBareSentinelPanic(m dsl.Matcher) { //nolint:unused // loaded by gocritic ruleguard checker at lint time
	m.Match(`panic(werr.$err)`).
		Where(m["err"].Text.Matches(`^Err[A-Z]`) && !m.File().Name.Matches(`_test\.go$`)).
		Report(`panic with bare sentinel: wrap with werr.WrapForeignErrorf(werr.$err, "site: context")`)
}

// noStringifiedErrorPanic flags panics that stringify an error via .Error(),
// discarding the error chain so a recover()/errors.Is/As cannot see the
// underlying failure.
//
// This deliberately targets only the .Error() pattern. Pure string and
// fmt.Sprintf panics that report a programmer-error invariant ("can't happen"
// — e.g. a missing table entry) carry no error to lose and are left alone.
//
// There is no m["err"].Type filter: ruleguard cannot resolve the builtin
// error interface (Implements("error") silently matches nothing), and a
// ".Error()" call whose string result is concatenated into a panic message
// is the stringify-an-error antipattern by construction — a non-error type
// reaching that shape is not a realistic case.
//
//	// Wrong:
//	panic("stdlib: failed to create sub-FS: " + err.Error())
//
//	// Right (preserves err in the chain):
//	panic(werr.WrapForeignErrorf(err, "stdlib: failed to create sub-FS"))
func noStringifiedErrorPanic(m dsl.Matcher) { //nolint:unused // loaded by gocritic ruleguard checker at lint time
	m.Match(
		`panic($msg + $err.Error())`,
		`panic($err.Error())`,
	).
		Where(!m.File().Name.Matches(`_test\.go$`)).
		Report(`panic stringifies an error via .Error(): wrap it instead — panic(werr.WrapForeignErrorf(err, "site: context")) — so the error chain survives`)
}

// noDirectValueRegisterAccess flags direct reads or writes of the
// machine/vmState value-register fields (singleValue, multiValues) outside
// machine/vm_state.go. The fields form a split-representation register
// with a documented mutual-exclusion invariant ("at most one field is
// active at any time"; see machine/vm_state.go) that is unenforced by the
// type system. All access must go through one of the vmState accessor
// methods: SetValue, SetValues, GetValue, GetValues, PushValues,
// pushValueRegisterTo, copyValueRegisterFrom, cloneValueRegisterFrom.
//
// Skips test files for two distinct reasons:
//   - machine_continuation_test.go, pool_test.go, vm_state_test.go:
//     these legitimately exercise the vmState fields directly to assert
//     invariant transitions and pool-reset behaviour.
//   - operation_test.go: its test-case struct has local fields named
//     'singleValue' and 'multiValues' that coincidentally match the rule
//     pattern via 'tc.singleValue' / 'tc.multiValues' selector
//     expressions — those are not vmState accesses, just name collision.
//
// See memory/2026-05-06-machine-structural-reduction.md (Finding 3) and
// memory/2026-05-11-machine-sr-finding3-impl.md.
//
//	// Wrong:
//	mc.singleValue = v
//	mc.multiValues = nil
//
//	// Right:
//	mc.SetValue(v)
func noDirectValueRegisterAccess(m dsl.Matcher) { //nolint:unused // loaded by gocritic ruleguard checker at lint time
	m.Match(
		`$x.singleValue`,
		`$x.multiValues`,
	).
		Where(!m.File().Name.Matches(`vm_state\.go$`) &&
			!m.File().Name.Matches(`_test\.go$`)).
		Report(`direct value-register access: use SetValue, SetValues, GetValue, GetValues, PushValues, pushValueRegisterTo, copyValueRegisterFrom, or cloneValueRegisterFrom on *vmState (Finding 3)`)
}

// noUncheckedArgCast flags an unchecked single-value type assertion on the
// result of a CallContext's Arg(...) — the primitive-argument (user-input)
// boundary. A wrong-type argument makes the assertion panic with a
// runtime.Error, and the VM boundary recover in RunWithEscapeHandling
// deliberately catches only *werr.ForeignError and re-raises runtime.Error —
// so the panic crashes the process instead of surfacing as a catchable Scheme
// condition. Use the checked extraction helpers (helpers.RequireArg[T],
// helpers.ExtractInteger, ...) or the comma-ok form, which the whole primitive
// surface already does.
//
// Only the single-value forms are matched. The comma-ok form
// (`x, ok := mc.Arg(0).(*values.Integer)`) is a distinct two-LHS AST shape and
// is the safe, idiomatic pattern — it is intentionally NOT flagged. The
// patterns below cover the four expression positions in which this tree
// actually writes a single-value assertion: assignment, selector chain,
// return, and call argument. Positions no call site uses (composite literal,
// index, binary operand) are deliberately absent rather than enumerated.
//
// Scoped to `*values.$t` (concrete pointer asserts) — the panic-prone shape the
// finding targets; interface asserts (values.Number, values.Tuple) are almost
// always written comma-ok anyway.
//
// See memory/ffi-badcast-vm-boundary-recover.md.
//
//	// Wrong (panics on wrong type → process crash):
//	n := mc.Arg(0).(*values.Integer)
//	v := mc.Arg(0).(*values.Vector).Get(i)
//	use(mc.Arg(0).(*values.Integer))
//
//	// Right:
//	n, err := helpers.RequireArg[*values.Integer](mc, 0, werr.ErrNotAnInteger, "name")
//	n, ok := mc.Arg(0).(*values.Integer)
func noUncheckedArgCast(m dsl.Matcher) { //nolint:unused // loaded by gocritic ruleguard checker at lint time
	m.Match(
		`$lhs := $mc.Arg($n).(*values.$t)`,
		`$lhs = $mc.Arg($n).(*values.$t)`,
		`$mc.Arg($n).(*values.$t).$sel`,
		`return $mc.Arg($n).(*values.$t)`,
		`$f($*_, $mc.Arg($n).(*values.$t), $*_)`,
	).
		Where(!m.File().Name.Matches(`_test\.go$`)).
		Report(`unchecked type assertion on Arg(): a wrong-type argument panics (runtime.Error) and crashes the VM — the boundary recover catches only *werr.ForeignError. Use helpers.RequireArg[T](mc, n, sentinel, name) or the comma-ok form`)
}
