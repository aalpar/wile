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
	"sort"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/match"
	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
)

// OperationSyntaxCaseMatch performs pattern matching for syntax-case.
//
// Expects:
//   - Value register: syntaxCaseClause with compiled pattern
//   - Per-context syntaxCaseState.input: input syntax object (set by OperationStoreSyntaxCaseInput)
//
// Results:
//   - If match succeeds: value register = #t, pattern bindings stored in context
//   - If match fails: value register = #f
type OperationSyntaxCaseMatch struct {
	machine.OperationBase
}

// syntaxCaseState holds per-context state for syntax-case expansion.
// It is stored on MachineContext.syntaxCase (an any-typed back-channel field)
// so that syntax-case is reentrant and safe for concurrent macro expansion.
// machine/ cannot import this package (one-direction dependency), so the
// machine-side field is any-typed; the constraint that only this concrete
// type is ever stored there is enforced by the field's encapsulation rather
// than by the type system.
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
// MachineContext.syntaxCase. Discriminates the two failure modes:
// nil field (no syntax-case expansion in flight) and wrong concrete
// type (contract violation — the field is any-typed and the encapsulation
// argument relies on no other code storing alternatives).
func loadSyntaxCaseState(mc *machine.MachineContext) (*syntaxCaseState, error) {
	raw := mc.SyntaxCaseState()
	if raw == nil {
		return nil, mc.Error("syntax-case: no state on MachineContext (no expansion in flight)")
	}
	sc, ok := raw.(*syntaxCaseState)
	if !ok {
		return nil, mc.Error(fmt.Sprintf(
			"syntax-case: unexpected state type %T on MachineContext.syntaxCase", raw))
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
		return nil, mc.Error(fmt.Sprintf("syntax-case: expected clause in value register, got %T", clauseVal))
	}

	// Get input from per-context state (set by OperationStoreSyntaxCaseInput).
	// The state field is any-typed (see machine_context.go); discriminate
	// nil-vs-mismatch so the diagnostic identifies which contract was broken.
	sc, err := loadSyntaxCaseState(mc)
	if err != nil {
		return nil, err
	}
	if sc.input == nil {
		return nil, mc.Error("syntax-case: state has no input (OperationStoreSyntaxCaseInput not run?)")
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
	// Convert to ordered list for consistent indexing
	vars := make([]string, 0, len(patternVars))
	for v := range patternVars {
		vars = append(vars, v)
	}
	// Sort for consistent ordering
	sort.Strings(vars)
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
		return nil, mc.Error("syntax-case: state has no bindings (OperationSyntaxCaseMatch did not succeed?)")
	}

	// Create a new local environment frame with slots for pattern variables
	localEnv := environment.NewLocalEnvironment(len(p.PatternVars))
	childEnv := environment.NewEnvironmentFrameWithParent(localEnv, mc.EnvironmentFrame())

	// Bind each pattern variable. MaybeCreateLocalBinding returns
	// (*LocalIndex, created bool); the bool is unused here. The protocol
	// has four (li, ok) states, each with a distinct outcome:
	//
	//   li != nil, ok == true   — matched non-ellipsis var: write stxVal
	//   li == nil, ok == true   — outer scope binds the name: skip the
	//                              local set; the variable resolves via
	//                              the environment chain
	//   li != nil, ok == false  — ELLIPSIS-CAPTURED var: write nil. This
	//                              is intentional and is *not* silent
	//                              corruption (despite what it looks
	//                              like). Per internal/match/CLAUDE.local.md,
	//                              ellipsis-captured pattern variables
	//                              live in the matcher's captureContext
	//                              children, not in the root bindings
	//                              map that GetBindings() returns. The
	//                              nil at the local slot signals
	//                              "captured elsewhere — consult the
	//                              matcher during template expansion."
	//                              Patterns like (_ x ...) take this path.
	//   li == nil, ok == false  — outer scope binds the name AND the var
	//                              is ellipsis-captured: nothing to do
	//                              locally; matcher tracks the captures.
	//
	// The two ok=false cases have no top-level binding by design; the
	// downstream OperationSyntaxTemplateExpand consults sc.matcher's
	// child contexts when expanding `(syntax (... x ...))` templates.
	for _, varName := range p.PatternVars {
		sym := values.NewSymbol(varName)
		li, _ := childEnv.MaybeCreateLocalBinding(sym, environment.BindingTypeVariable, nil, nil)
		stxVal, ok := sc.bindings[varName]
		if li == nil {
			// Either an outer-scope binding wins (ok=true case) or no
			// local frame to write to (ok=false ellipsis case): skip.
			continue
		}
		if !ok {
			// Ellipsis-captured var: leave the local slot as the
			// zero value (nil). Template expansion reads from the
			// matcher's children, not this slot.
			continue
		}
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
	return nil, mc.Error("syntax-case: no matching clause")
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
}

func NewOperationSyntaxTemplateExpand() *OperationSyntaxTemplateExpand {
	return &OperationSyntaxTemplateExpand{
		OperationBase: machine.NewOperationBaseWithGoName("operation:syntax-template-expand", "SyntaxTemplateExpand"),
	}
}

func (p *OperationSyntaxTemplateExpand) Apply(mc *machine.MachineContext) (*machine.MachineContext, error) {
	sc, err := loadSyntaxCaseState(mc)
	if err != nil {
		return nil, err
	}
	if sc.matcher == nil {
		return nil, mc.Error("syntax: state has no matcher (OperationSyntaxCaseMatch did not succeed?)")
	}

	// Get the template from value register
	templateVal := mc.GetValue()
	template, ok := templateVal.(syntax.SyntaxValue)
	if !ok {
		return nil, mc.Error(fmt.Sprintf("syntax: expected syntax template, got %T", templateVal))
	}

	// Expand the template using the matcher (handles ellipsis)
	// Use nil for intro scope and freeIds for now - hygiene can be added later
	expanded, err := sc.matcher.Expand(template, match.ExpandOptions{})
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
