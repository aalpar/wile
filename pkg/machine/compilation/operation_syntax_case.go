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
	"errors"
	"fmt"
	"maps"
	"slices"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/match"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/schemeutil"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// OperationSyntaxCaseMatch performs pattern matching for syntax-case.
//
// Expects:
//   - Value register: *SyntaxCaseClause with compiled pattern
//   - Per-context syntaxCaseState.input: input syntax object (set by OperationStoreSyntaxCaseInput)
//
// Results:
//   - If match succeeds: value register = #t, pattern bindings stored in context
//   - If match fails: value register = #f
type OperationSyntaxCaseMatch struct {
	machine.OperationBase
}

// syntaxCaseState holds per-context state for syntax-case expansion.
// It is stored via MachineContext.SyntaxCaseState (an any-typed back-channel
// slot on the clustered expansion sub-record) so that concurrent macro
// expansions on different MachineContexts do not share state. machine/ cannot
// import this package (one-direction dependency), so the machine-side slot is
// any-typed; the constraint that only this concrete type is ever stored
// there is enforced by the slot's encapsulation rather than by the type
// system.
//
// It is NOT reentrant within a single context: the slot holds exactly one
// form's state, OperationStoreSyntaxCaseInput overwrites it and
// OperationClearSyntaxCaseInput nils it. Since compileSyntaxCaseClause
// compiles a clause body into the same template, a syntax-case nested inside a
// clause body runs on the same context and destroys the enclosing form's
// state; the enclosing form then fails with "no state on MachineContext".
type syntaxCaseState struct {
	bindings map[string]syntax.SyntaxValue // pattern variable bindings from last match
	matcher  *match.SyntaxMatcher          // matcher from last match (needed for ellipsis expansion)
	input    syntax.SyntaxValue            // input syntax object being matched
}

// ensureSyntaxCaseState lazily initializes the syntaxCaseState on the context.
func ensureSyntaxCaseState(mc *machine.MachineContext) *syntaxCaseState {
	sc, ok := mc.SyntaxCaseState().(*syntaxCaseState)
	if ok {
		return sc
	}
	sc = &syntaxCaseState{}
	mc.SetSyntaxCaseState(sc)
	return sc
}

// loadSyntaxCaseState fetches the *syntaxCaseState payload from
// MachineContext.SyntaxCaseState(). Discriminates the two failure modes:
// nil (no syntax-case expansion in flight) and wrong concrete type
// (contract violation — the slot is any-typed and the encapsulation
// argument relies on no other code storing alternatives).
func loadSyntaxCaseState(mc *machine.MachineContext) (*syntaxCaseState, error) {
	raw := mc.SyntaxCaseState()
	if raw == nil {
		return nil, mc.WrapError(werr.ErrInternal, "syntax-case: no state on MachineContext (no expansion in flight)")
	}
	sc, ok := raw.(*syntaxCaseState)
	if !ok {
		return nil, mc.WrapError(werr.ErrInternal, fmt.Sprintf(
			"syntax-case: unexpected state type %T from MachineContext.SyntaxCaseState()", raw))
	}
	return sc, nil
}

func NewOperationSyntaxCaseMatch() *OperationSyntaxCaseMatch {
	return &OperationSyntaxCaseMatch{
		OperationBase: machine.NewOperationBaseWithGoName("operation:syntax-case-match", "SyntaxCaseMatch"),
	}
}

func (p *OperationSyntaxCaseMatch) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	// Get the clause from value register
	clauseVal := mc.GetValue()
	clause, ok := clauseVal.(*SyntaxCaseClause)
	if !ok {
		return nil, mc.WrapError(werr.ErrInternal, fmt.Sprintf("syntax-case: expected clause in value register, got %T", clauseVal))
	}

	// Get input from per-context state (set by OperationStoreSyntaxCaseInput).
	// The state field is any-typed (see machine_context.go); discriminate
	// nil-vs-mismatch so the diagnostic identifies which contract was broken.
	sc, err := loadSyntaxCaseState(mc)
	if err != nil {
		return nil, err
	}
	if sc.input == nil {
		return nil, mc.WrapError(werr.ErrInternal, "syntax-case: state has no input (OperationStoreSyntaxCaseInput not run?)")
	}
	input := sc.input

	// Create a matcher
	matcher := match.NewSyntaxMatcher(clause.PatternVars, clause.Bytecode, &match.SyntaxMatcherOpts{
		EllipsisVars:   clause.EllipsisVars,
		EllipsisDepths: clause.EllipsisDepths,
	})

	// Try to match. ErrNotAMatch is normal control flow for syntax-case
	// (this clause didn't match — try the next one); any other error
	// (context cancellation, malformed input, ellipsis-depth invariant
	// violation, internal matcher bug) is a real failure and must surface.
	err = matcher.Match(mc.Context(), input)
	if errors.Is(err, match.ErrNotAMatch) {
		mc.SetValue(values.FalseValue)
		mc.IncrPC()
		return mc, nil
	}
	if err != nil {
		return nil, mc.WrapError(err, "syntax-case: matcher error")
	}

	// Match succeeded - store bindings and matcher in per-context state
	sc.bindings = matcher.GetBindings()
	sc.matcher = matcher
	mc.SetValue(values.TrueValue)
	mc.IncrPC()
	return mc, nil
}

func (p *OperationSyntaxCaseMatch) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationSyntaxCaseMatch)
	return machine.SameType(p, v, ok)
}

// OperationBindPatternVars binds pattern variables from the last match
// into a new local environment frame that is pushed onto the current environment.
//
// This operation creates a new environment frame with local slots for each
// pattern variable, binds the matched values, and makes this the current environment.
type OperationBindPatternVars struct {
	machine.OperationBase
	PatternVars []string // Ordered list for consistent indexing
}

func NewOperationBindPatternVars(patternVars map[string]struct{}) *OperationBindPatternVars {
	// Convert to a sorted list for consistent indexing
	vars := slices.Sorted(maps.Keys(patternVars))
	return &OperationBindPatternVars{
		OperationBase: machine.NewOperationBaseWithGoName("operation:bind-pattern-vars", "BindPatternVars"),
		PatternVars:   vars,
	}
}

func (p *OperationBindPatternVars) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	sc, err := loadSyntaxCaseState(mc)
	if err != nil {
		return nil, err
	}
	if sc.bindings == nil {
		return nil, mc.WrapError(werr.ErrInternal, "syntax-case: state has no bindings (OperationSyntaxCaseMatch did not succeed?)")
	}

	// Create a new local environment frame with slots for pattern variables
	localEnv := environment.NewLocalEnvironment(len(p.PatternVars))
	childEnv := environment.NewEnvironmentFrameWithParent(localEnv, mc.EnvironmentFrame())

	// Bind each pattern variable. MaybeCreateLocalBinding returns
	// (*LocalIndex, created bool) and the bool is discarded, because on this
	// frame it carries no information: localEnv is freshly allocated, so its
	// keys map is empty, no dedup lookup can hit, and every call creates a
	// slot. For the same reason li is never nil here (a non-nil keys map makes
	// hasLocal() true); the guard below is defensive only.
	//
	// The ellipsis signal is therefore not in the return values, it is the
	// ABSENCE of varName from sc.bindings. matcher.GetBindings() exposes only
	// the ROOT capture context; ellipsis-captured pattern variables (e.g. `x`
	// in `(_ x ...)`) live in that context's `children` field, walked at
	// template-expansion time (see internal/match/match.go's "Capture Context"
	// comment block and findMatchingEllipsisID). Indexing the missing key
	// yields a nil syntax.SyntaxValue, and writing that nil overrides the
	// slot's default `values.Void` initialization from NewLocalEnvironment.
	// The nil is the protocol signal: it tells the downstream
	// OperationSyntaxTemplateExpand to consult sc.matcher's child contexts
	// when expanding `(syntax (x ...))` templates.
	for _, varName := range p.PatternVars {
		sym := values.NewSymbol(varName)
		li, _ := childEnv.MaybeCreateLocalBinding(sym, environment.BindingTypeVariable, nil, nil)
		if li == nil {
			// No local frame to write to: nothing to bind.
			continue
		}
		// stxVal is nil when varName is absent from sc.bindings (the
		// ellipsis case). Writing nil here is the protocol signal —
		// see the comment block above.
		stxVal := sc.bindings[varName]
		err := childEnv.SetLocalValue(li, stxVal)
		if err != nil {
			return nil, mc.WrapError(err, fmt.Sprintf("syntax-case: failed to bind pattern variable %s", varName))
		}
	}

	// Switch to the new environment. childEnv was heap-allocated (not from
	// envFramePool), so clear envPooled to prevent RestoreAndRelease from
	// recycling it. See vm_state.go envPooled write-site table.
	mc.SetEnvironmentFrame(childEnv)
	mc.SetEnvPooled(false)
	mc.IncrPC()
	return mc, nil
}

func (p *OperationBindPatternVars) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationBindPatternVars)
	return machine.SliceMatches(p, v, ok,
		func(op *OperationBindPatternVars) []string {
			return op.PatternVars
		})
}

// OperationSyntaxCaseNoMatch is emitted at the end of syntax-case when no clause matches.
type OperationSyntaxCaseNoMatch struct {
	machine.OperationBase
}

func NewOperationSyntaxCaseNoMatch() *OperationSyntaxCaseNoMatch {
	return &OperationSyntaxCaseNoMatch{
		OperationBase: machine.NewOperationBaseWithGoName("operation:syntax-case-no-match", "SyntaxCaseNoMatch"),
	}
}

func (p *OperationSyntaxCaseNoMatch) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	// Include the input form in the diagnostic when it's still available.
	// Macro debugging is hard precisely because the output is "syntax";
	// stripping the actual input forces users into trial-and-error.
	raw := mc.SyntaxCaseState()
	if raw != nil {
		sc, ok := raw.(*syntaxCaseState)
		if ok && sc.input != nil {
			return nil, mc.WrapError(werr.ErrInvalidSyntax, fmt.Sprintf(
				"syntax-case: no matching clause for input %s", sc.input.SchemeString()))
		}
	}
	return nil, mc.WrapError(werr.ErrInvalidSyntax, "syntax-case: no matching clause (input unavailable)")
}

func (p *OperationSyntaxCaseNoMatch) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationSyntaxCaseNoMatch)
	return machine.SameType(p, v, ok)
}

// OperationSyntaxTemplateExpand expands a syntax template using the current
// pattern variable bindings. This is used for templates containing ellipsis,
// which require runtime expansion rather than compile-time code generation.
//
// The template is stored in the value register (loaded from literals).
// The result is the expanded syntax object, left in the value register.
type OperationSyntaxTemplateExpand struct {
	machine.OperationBase
	// FreeIds and PatternVarSyntax carry the enclosing syntax-case clause's
	// hygiene data, computed once at compile time by CompileSyntax. They mirror
	// the SyntaxRulesClause fields the syntax-rules transformer uses, so the
	// ellipsis template-expansion path is hygienic (R7RS §4.3): free template
	// identifiers resolve at the macro definition site and template-introduced
	// binders carry a fresh intro scope rather than capturing use-site identifiers.
	FreeIds          map[string]*FreeIdResolution
	PatternVarSyntax map[string]*syntax.SyntaxSymbol
}

func NewOperationSyntaxTemplateExpand(freeIds map[string]*FreeIdResolution, patternVarSyntax map[string]*syntax.SyntaxSymbol) *OperationSyntaxTemplateExpand {
	return &OperationSyntaxTemplateExpand{
		OperationBase:    machine.NewOperationBaseWithGoName("operation:syntax-template-expand", "SyntaxTemplateExpand"),
		FreeIds:          freeIds,
		PatternVarSyntax: patternVarSyntax,
	}
}

func (p *OperationSyntaxTemplateExpand) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	sc, err := loadSyntaxCaseState(mc)
	if err != nil {
		return nil, err
	}
	if sc.matcher == nil {
		return nil, mc.WrapError(werr.ErrInternal, "syntax: state has no matcher (OperationSyntaxCaseMatch did not succeed?)")
	}

	// Get the template from value register
	templateVal := mc.GetValue()
	template, ok := templateVal.(syntax.SyntaxValue)
	if !ok {
		return nil, mc.WrapError(werr.ErrInternal, fmt.Sprintf("syntax: expected syntax template, got %T", templateVal))
	}

	// Expand the template using the matcher (handles ellipsis). Mirror the
	// syntax-rules transformer (OperationSyntaxRulesTransform.Apply): a fresh
	// intro scope per expansion, plus the compile-time free-id and pattern-var
	// hygiene data. This makes the ellipsis path hygienic (R7RS §4.3) — the
	// non-ellipsis path achieves the same by emitting template symbols as
	// def-site-scoped literals. UseSiteCtx/Origin are intentionally nil:
	// syntax-case has no single macro-invocation use-site.
	introScope := syntax.NewScopeWithLabel("intro")
	freeIds := make(map[string]match.FreeIdResolver, len(p.FreeIds))
	for k, v := range p.FreeIds {
		freeIds[k] = v
	}
	expanded, err := sc.matcher.Expand(template, match.ExpandOptions{
		IntroScope:       introScope,
		FreeIds:          freeIds,
		PatternVarSyntax: p.PatternVarSyntax,
	})
	if err != nil {
		return nil, mc.WrapError(err, "syntax: template expansion error")
	}

	mc.SetValue(expanded)
	mc.IncrPC()
	return mc, nil
}

func (p *OperationSyntaxTemplateExpand) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationSyntaxTemplateExpand)
	return machine.SameType(p, v, ok)
}

// OperationStoreSyntaxCaseInput stores the value register into the per-context
// syntaxCaseState for use by OperationSyntaxCaseMatch.
type OperationStoreSyntaxCaseInput struct {
	machine.OperationBase
}

func NewOperationStoreSyntaxCaseInput() *OperationStoreSyntaxCaseInput {
	return &OperationStoreSyntaxCaseInput{
		OperationBase: machine.NewOperationBaseWithGoName("operation:store-syntax-case-input", "StoreSyntaxCaseInput"),
	}
}

func (p *OperationStoreSyntaxCaseInput) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	sc := ensureSyntaxCaseState(mc)
	val := mc.GetValue()
	// Convert to syntax value if needed (handles Pairs, Vectors, etc.)
	stx, ok := val.(syntax.SyntaxValue)
	if ok {
		sc.input = stx
	} else {
		sc.input = schemeutil.DatumToSyntaxValue(mc.Context(), nil, val)
	}
	mc.IncrPC()
	return mc, nil
}

func (p *OperationStoreSyntaxCaseInput) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationStoreSyntaxCaseInput)
	return machine.SameType(p, v, ok)
}

// OperationClearSyntaxCaseInput clears the per-context syntax-case state.
// This is called at the end of a syntax-case form.
type OperationClearSyntaxCaseInput struct {
	machine.OperationBase
}

func NewOperationClearSyntaxCaseInput() *OperationClearSyntaxCaseInput {
	return &OperationClearSyntaxCaseInput{
		OperationBase: machine.NewOperationBaseWithGoName("operation:clear-syntax-case-input", "ClearSyntaxCaseInput"),
	}
}

func (p *OperationClearSyntaxCaseInput) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	mc.SetSyntaxCaseState(nil)
	mc.IncrPC()
	return mc, nil
}

func (p *OperationClearSyntaxCaseInput) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationClearSyntaxCaseInput)
	return machine.SameType(p, v, ok)
}
