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

package validate

import (
	"fmt"
	"strings"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// DefaultMaxOriginDepth is the default maximum number of macro expansions to show
// in error messages. Set to 0 for unlimited depth.
const DefaultMaxOriginDepth = 10

// ValidationError captures location and details for error reporting
type ValidationError struct {
	Source  *syntax.SourceContext
	Message string
	Form    string // e.g., "if", "define", "lambda"
}

func (p ValidationError) Error() string {
	return p.ErrorWithMaxOriginDepth(DefaultMaxOriginDepth)
}

// ErrorWithMaxOriginDepth returns the error message with a configurable origin chain depth.
func (p ValidationError) ErrorWithMaxOriginDepth(maxDepth int) string {
	var msg string
	if loc := p.Source.Location(); loc != "" {
		msg = fmt.Sprintf("%s: %s in %s form", loc, p.Message, p.Form)

		// Add origin chain if present
		if p.Source.Origin != nil {
			msg += syntax.FormatOriginChain(p.Source.Origin, maxDepth)
		}
	} else {
		msg = fmt.Sprintf("%s in %s form", p.Message, p.Form)
	}
	return msg
}

// ValidationResult collects all errors from validation
type ValidationResult struct {
	Expr   ValidatedExpr     // nil if validation failed
	Errors []ValidationError // All errors encountered

	// mutated records every set! target in the unit, keyed by BindingRef so a
	// single map names both halves of the binding domain. Each set! records
	// two facts at the two precisions its consumers need:
	//   - a precise LocalRef when the target resolves to a local slot (read by
	//     markMutableBindings to set LetBinding.Mutable);
	//   - a conservative GlobalRef(name) unconditionally (read by
	//     finalizeStability for StableInUnit). The global arm is symbolic
	//     because a top-level binding is created by the compiler, not the
	//     validator, so no slot exists at validation time; marking it even for
	//     a local shadow of the name is the deliberate over-approximation that
	//     keeps the frame-reclaim Stable stamp sound (over-match ⇒ non-stable).
	mutated values.MapSet[environment.BindingRef]

	// definedKeyCount counts define occurrences by symbol Key across the unit.
	// A distinct signal from mutation: powers the defined-once half of
	// StableInUnit. See finalizeStability.
	definedKeyCount map[string]int
}

// markMutated records that the binding named by ref is a set! target. The ref
// is a precise LocalRef (frame + slot) for a resolved local — stable across
// []Binding realloc, unlike a *Binding pointer — or a symbolic GlobalRef(Key)
// for a global or a not-yet-created top-level binding.
func (p *ValidationResult) markMutated(ref environment.BindingRef) {
	if p.mutated == nil {
		p.mutated = values.NewMapSet[environment.BindingRef](0)
	}
	p.mutated.Set(ref)
}

// isMutated returns true if the binding named by ref was targeted by set!.
func (p *ValidationResult) isMutated(ref environment.BindingRef) bool {
	return p.mutated.ContainsOne(ref)
}

// recordDefinedKey counts a define of a symbol named key in the unit. Used to
// enforce defined-once: a Key defined twice (e.g. a top-level redefinition or a
// same-name internal define) is not in-unit-stable.
func (p *ValidationResult) recordDefinedKey(key string) {
	if p.definedKeyCount == nil {
		p.definedKeyCount = make(map[string]int)
	}
	p.definedKeyCount[key]++
}

// finalizeStability stamps StableInUnit on every top-level define once the unit
// is fully validated (so mutated/definedKeyCount are complete). A define is
// in-unit-stable iff its name is defined exactly once and never set! in the
// unit. Top-level only: internal/local defines never reach the compiler's
// global Stable stamp, so they are intentionally not visited here.
func (p *ValidationResult) finalizeStability() {
	if p.Expr == nil {
		return
	}
	collectTopLevelDefines([]ValidatedExpr{p.Expr}, func(d *ValidatedDefine) {
		key := d.name.Key()
		d.StableInUnit = p.definedKeyCount[key] == 1 && !p.isMutated(environment.GlobalRef(key))
	})
}

// UnitArityInfo is a top-level define's arity as parsed from its formals.
// RequiredCount excludes any rest parameter; Variadic is true when one exists.
// That is ValidatedParams' convention, NOT NativeTemplate's, which counts the
// rest parameter in its parameterCount — the two meet at exactly one conversion
// point in the compiler.
type UnitArityInfo struct {
	RequiredCount int
	Variadic      bool
}

// UnitArityOf returns the arity of every top-level define in unit whose name the
// unit neither redefines nor mutates. Callers may trust each entry without
// re-checking a flag: an unstable name is absent rather than
// present-but-doubtful. Run it after validation, which is what stamps
// StableInUnit.
//
// It answers the question Binding.Value() cannot for a same-unit define: the
// binding exists at compile time (that is what makes forward references
// resolve), but its closure is built by the emitted code at run time, so there
// is no callable to interrogate. The formals are the only compile-time evidence
// of that define's arity.
//
// Gating on StableInUnit does two jobs, and the second is the non-obvious one.
//
// The plain job: a name defined twice, or set! in unit, has no single arity, so
// there is nothing sound to record.
//
// The load-bearing job: it is also what makes the string key safe. A symbol
// Key() drops hygiene, so two hygiene-distinct top-level bindings of one name
// collide here. binding_ref.go makes the parallel over-match safe for BindingRef
// by arguing it "can only forfeit an optimization, never wrongly apply one" —
// and that argument does NOT transfer to arity, where applying one h's arity to
// a hygiene-distinct h would be a false compile error on correct code. A
// collision means definedKeyCount[key] >= 2, which forfeits StableInUnit, so
// neither entry is recorded and the call site falls through unchecked. Do not
// relax this to last-definition-wins without first replacing the key with
// something hygiene-aware — and note a reference's scope set is a superset of
// its binder's, so a ScopeFingerprint cannot serve as that key.
func UnitArityOf(unit []ValidatedExpr) map[string]UnitArityInfo {
	var q map[string]UnitArityInfo
	collectTopLevelDefines(unit, func(d *ValidatedDefine) {
		if !d.StableInUnit {
			return
		}
		params := unitDefineParams(d)
		if params == nil {
			return
		}
		if q == nil {
			q = make(map[string]UnitArityInfo)
		}
		q[d.name.Key()] = UnitArityInfo{
			RequiredCount: len(params.Required),
			Variadic:      params.Rest != nil,
		}
	})
	return q
}

// unitDefineParams returns the formals a define binds a procedure with, or nil
// when it binds something whose arity is not syntactically apparent.
//
// define has two shapes and the formals live in different places:
// (define (h x y) ...) carries them on the define itself, while
// (define h (lambda (x y) ...)) carries them on the lambda. Reading only the
// second would miss the function form, which is the common one.
//
// A define of anything else — a literal, a call, a case-lambda — yields nil, so
// no entry is recorded and no call site is checked against it.
func unitDefineParams(d *ValidatedDefine) *ValidatedParams {
	if d.IsFunction {
		return d.Params()
	}
	lam, ok := d.SubExp().(*ValidatedLambda)
	if !ok {
		return nil
	}
	return lam.Params()
}

// Ok returns true if no validation errors were encountered.
func (p *ValidationResult) Ok() bool {
	return len(p.Errors) == 0
}

func (p *ValidationResult) Error() string {
	if len(p.Errors) == 0 {
		return ""
	}
	if len(p.Errors) == 1 {
		return p.Errors[0].Error()
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "%d validation errors:\n", len(p.Errors))
	for _, e := range p.Errors {
		sb.WriteString("  ")
		sb.WriteString(e.Error())
		sb.WriteString("\n")
	}
	return sb.String()
}

func (p *ValidationResult) addError(source *syntax.SourceContext, form, message string) {
	p.Errors = append(p.Errors, ValidationError{
		Source:  source,
		Message: message,
		Form:    form,
	})
}

func (p *ValidationResult) addErrorf(source *syntax.SourceContext, form, format string, args ...any) {
	p.addError(source, form, fmt.Sprintf(format, args...))
}
