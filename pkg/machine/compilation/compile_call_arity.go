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

package compilation

import (
	"strconv"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// checkCallArity reports a compile-time error when a call site passes an
// argument count its callee cannot accept.
//
// It fires in every compile, not only under wile --check. An arity mismatch
// against a callee that cannot be rebound is a guaranteed runtime error, so
// reporting it at compile time is strictly earlier and never a behavior change
// for a program that would have run correctly. tryInlineCall already sets this
// precedent for let-bound lambdas (compile_call.go).
//
// A nil return means "not checked" as well as "checked and fine". The two are
// deliberately indistinguishable to the caller, which must not read a clean
// return as proof of anything: most call sites are unreachable to this check.
func (p *CompileTimeContinuation) checkCallArity(v *validate.ValidatedCall) error {
	sym, ok := v.Proc().(*validate.ValidatedSymbol)
	if !ok {
		return nil
	}

	// GetBinding searches locals before globals, so it is the authority on what
	// this operator denotes. It allocates nothing.
	denoted := p.env.GetBinding(sym.Symbol.Sym, syntax.ScopesOf(sym.Symbol.Scopes()))
	if denoted == nil {
		return nil
	}

	argCount := len(v.Body())
	var want string

	callee, isCallable := denoted.Value().(values.Callable)
	if isCallable {
		// The callee's value already exists: an ambient primitive or an imported
		// procedure. Its rebind-stability is the binding's own business, because
		// it was defined in some other unit whose StableInUnit evidence is not
		// available here.
		if !denoted.IsStable() {
			return nil
		}
		if callee.AcceptsArity(argCount) {
			return nil
		}
		want = arityDescription(callee)
	} else {
		// No callable value. The only callee this path can say anything about is
		// a same-unit define, whose binding exists (that is what makes forward
		// references resolve) but whose closure the emitted code builds at run
		// time, so the validator's formals table answers instead. Everything
		// else reaching here — an uninitialised non-procedure, a local — misses
		// that table, or is caught by the global check below.
		//
		// Membership in that table IS this path's soundness evidence, and the
		// binding's Stable flag deliberately is not consulted. The flag is only
		// the compiler's later transcription of the same StableInUnit fact, and
		// it is not stamped until the define itself compiles — which is after
		// this call site for a forward reference. Gating on it would silently
		// skip exactly the case whole-program compilation exists to support. The
		// table is populated only under an immutable top level, so the other half
		// of the Stable argument holds too.
		info, found := p.unitArity[sym.Symbol.Sym.Key]
		if !found {
			return nil
		}
		if acceptsUnitArity(info, argCount) {
			return nil
		}
		want = describeArity(unitParamCount(info), info.Variadic)
	}

	// Only now, on the brink of reporting, confirm the operator really denotes a
	// global. Deferring it this far is what keeps the check off the compiler's
	// hot path: it is the one allocating step, and a call site that accepts its
	// callee's arity reports nothing either way, so the answer cannot change.
	if !p.denotesGlobal(sym.Symbol, denoted) {
		return nil
	}
	return arityError(v, sym, want, argCount)
}

// arityError renders the one diagnostic this file emits, anchored at the call
// form so the location names the offending argument list.
func arityError(v *validate.ValidatedCall, sym *validate.ValidatedSymbol, want string, got int) error {
	return wrapSourcedError(v.Source(), werr.WrapForeignErrorf(
		werr.ErrWrongNumberOfArguments,
		"call to %s: expected %s argument(s), got %d",
		sym.Symbol.Sym.Key, want, got,
	))
}

// denotesGlobal reports whether denoted — the binding sym already resolved to —
// is a global one, by resolving globals-only and requiring the same *Binding
// back.
//
// This is what rejects a local that shadows a same-named global; comparing the
// two spellings would not (see "Binding Identity" in the root CLAUDE.local.md).
// Without it, (define (k car) (car 1 2)) is a false compile error against the
// primitive. Both resolutions run within one caller off one p.env and nothing
// between them creates a binding, so neither pointer can go stale.
//
// It matters most to the unit-table path, which is keyed by bare name: reaching
// that table with an operator that does not denote the top-level define would
// apply a stranger's arity to it.
func (p *CompileTimeContinuation) denotesGlobal(sym *syntax.SyntaxSymbol, denoted *environment.Binding) bool {
	gi := p.env.GetGlobalIndexWithScopes(sym.Sym, syntax.ScopesOf(sym.Scopes()))
	if gi == nil {
		return false
	}
	return p.env.GetGlobalBinding(gi) == denoted
}

// acceptsUnitArity mirrors MachineClosure.AcceptsArity for a define whose closure
// does not exist yet. UnitArityInfo.RequiredCount excludes the rest parameter, so
// the variadic minimum is RequiredCount directly.
func acceptsUnitArity(info validate.UnitArityInfo, argCount int) bool {
	if info.Variadic {
		return argCount >= info.RequiredCount
	}
	return argCount == info.RequiredCount
}

// unitParamCount converts UnitArityInfo to NativeTemplate's convention, which
// counts the rest parameter, so describeArity's paramCount-1 lands on the right
// minimum. This is the single point where the two conventions meet.
func unitParamCount(info validate.UnitArityInfo) int {
	if info.Variadic {
		return info.RequiredCount + 1
	}
	return info.RequiredCount
}

// arityDescription renders a callable's accepted arity for a diagnostic, e.g.
// "2" or "at least 1". Message text only: AcceptsArity owns every accept/reject
// decision, and nothing here may feed back into one. A second arity formula in
// the compiler would be a new instance of the "one concept, N tables that drift"
// debt this codebase already tracks.
func arityDescription(callee values.Callable) string {
	switch c := callee.(type) {
	case *machine.MachineClosure:
		return describeArity(c.Template().ParameterCount(), c.Template().IsVariadic())
	case *machine.ForeignClosure:
		return describeArity(c.ParameterCount(), c.IsVariadic())
	case *machine.CaseLambdaClosure:
		return "one of its clause arities"
	}
	// Reached only by a callable this function does not name. Continuations are
	// the live example, and they accept every arity (R7RS §6.10), so they never
	// reach here at all.
	return "a different number of"
}

// describeArity renders a (paramCount, isVariadic) pair. Both NativeTemplate and
// ForeignClosure count the rest parameter in paramCount, so a variadic minimum is
// paramCount-1 — the same conversion MachineClosure.AcceptsArity and
// ForeignClosure.AcceptsArity make.
func describeArity(paramCount int, isVariadic bool) string {
	if isVariadic {
		return "at least " + strconv.Itoa(paramCount-1)
	}
	return strconv.Itoa(paramCount)
}
