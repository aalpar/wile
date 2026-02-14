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

package machine

import (
	"context"
	"fmt"
	"sort"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/match"
	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

// OperationSyntaxCaseMatch performs pattern matching for syntax-case.
//
// Expects:
//   - wrt register: syntaxCaseClause with compiled pattern
//   - Top of eval stack: input syntax object
//
// Results:
//   - If match succeeds: value register = #t, pattern bindings stored in context
//   - If match fails: value register = #f
type OperationSyntaxCaseMatch struct {
	OperationBase
}

// syntaxCaseState holds per-context state for syntax-case expansion.
// It is stored on MachineContext (not as package globals) so that
// syntax-case is reentrant and safe for concurrent macro expansion.
type syntaxCaseState struct {
	bindings map[string]syntax.SyntaxValue // pattern variable bindings from last match
	matcher  *match.SyntaxMatcher          // matcher from last match (needed for ellipsis expansion)
	input    syntax.SyntaxValue            // input syntax object being matched
}

// ensureSyntaxCaseState lazily initializes the syntaxCaseState on the context.
func ensureSyntaxCaseState(mc *MachineContext) *syntaxCaseState {
	if mc.syntaxCase == nil {
		mc.syntaxCase = &syntaxCaseState{}
	}
	return mc.syntaxCase
}

func NewOperationSyntaxCaseMatch() *OperationSyntaxCaseMatch {
	return &OperationSyntaxCaseMatch{
		OperationBase: NewOperationBaseWithGoName("operation:syntax-case-match", "SyntaxCaseMatch"),
	}
}

func (p *OperationSyntaxCaseMatch) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	// Get the clause from value register
	clauseVal := mctx.GetValue()
	clause, ok := clauseVal.(*syntaxCaseClause)
	if !ok {
		return nil, mctx.Error(fmt.Sprintf("syntax-case: expected clause in value register, got %T", clauseVal))
	}

	// Get input from per-context state (set by OperationStoreSyntaxCaseInput)
	sc := mctx.syntaxCase
	if sc == nil || sc.input == nil {
		return nil, mctx.Error("syntax-case: no input available")
	}
	input := sc.input

	// Create a matcher
	matcher := match.NewSyntaxMatcherWithEllipsisVars(clause.patternVars, clause.bytecode, clause.ellipsisVars)

	// Try to match
	err := matcher.Match(ctx, input)
	if err != nil {
		// Match failed
		mctx.SetValue(values.FalseValue)
		mctx.pc++
		// Intentionally clear the matcher error: a failed match is normal control flow for syntax-case,
		// so we record #f in the value register and return no runtime error.
		return mctx, nil // nolint:errcheck, nilerr
	}

	// Match succeeded - store bindings and matcher in per-context state
	sc.bindings = matcher.GetBindings()
	sc.matcher = matcher
	mctx.SetValue(values.TrueValue)
	mctx.pc++
	return mctx, nil
}

func (p *OperationSyntaxCaseMatch) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationSyntaxCaseMatch)
	return sameType(p, v, ok)
}

// OperationBindPatternVars binds pattern variables from the last match
// into a new local environment frame that is pushed onto the current environment.
//
// This operation creates a new environment frame with local slots for each
// pattern variable, binds the matched values, and makes this the current environment.
type OperationBindPatternVars struct {
	OperationBase
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
		OperationBase: NewOperationBaseWithGoName("operation:bind-pattern-vars", "BindPatternVars"),
		PatternVars:   vars,
	}
}

func (p *OperationBindPatternVars) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	sc := mctx.syntaxCase
	if sc == nil || sc.bindings == nil {
		return nil, mctx.Error("syntax-case: no pattern bindings available")
	}

	// Create a new local environment frame with slots for pattern variables
	localEnv := environment.NewLocalEnvironment(len(p.PatternVars))
	childEnv := environment.NewEnvironmentFrameWithParent(localEnv, mctx.env)

	// Bind each pattern variable - use MaybeCreateLocalBinding to get the actual slot
	// which matches what the compiler does at compile time
	for _, varName := range p.PatternVars {
		sym := childEnv.InternSymbol(values.NewSymbol(varName))
		li, _ := childEnv.MaybeCreateLocalBinding(sym, environment.BindingTypeVariable)
		stxVal, ok := sc.bindings[varName]
		if ok && li == nil {
			continue
		}
		err := childEnv.SetLocalValue(li, stxVal)
		if err != nil {
			return nil, mctx.WrapError(err, fmt.Sprintf("syntax-case: failed to bind pattern variable %s", varName))
		}
	}

	// Switch to the new environment
	mctx.env = childEnv
	mctx.pc++
	return mctx, nil
}

func (p *OperationBindPatternVars) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationBindPatternVars)
	return sliceMatches(p, v, ok,
		func(op *OperationBindPatternVars) []string {
			return op.PatternVars
		})
}

// OperationSyntaxCaseNoMatch is emitted at the end of syntax-case when no clause matches.
type OperationSyntaxCaseNoMatch struct {
	OperationBase
}

func NewOperationSyntaxCaseNoMatch() *OperationSyntaxCaseNoMatch {
	return &OperationSyntaxCaseNoMatch{
		OperationBase: NewOperationBaseWithGoName("operation:syntax-case-no-match", "SyntaxCaseNoMatch"),
	}
}

func (p *OperationSyntaxCaseNoMatch) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	return nil, mctx.Error("syntax-case: no matching clause")
}

func (p *OperationSyntaxCaseNoMatch) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationSyntaxCaseNoMatch)
	return sameType(p, v, ok)
}

// OperationSyntaxTemplateExpand expands a syntax template using the current
// pattern variable bindings. This is used for templates containing ellipsis,
// which require runtime expansion rather than compile-time code generation.
//
// The template is stored in the value register (loaded from literals).
// The result is the expanded syntax object, left in the value register.
type OperationSyntaxTemplateExpand struct {
	OperationBase
}

func NewOperationSyntaxTemplateExpand() *OperationSyntaxTemplateExpand {
	return &OperationSyntaxTemplateExpand{
		OperationBase: NewOperationBaseWithGoName("operation:syntax-template-expand", "SyntaxTemplateExpand"),
	}
}

func (p *OperationSyntaxTemplateExpand) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	sc := mctx.syntaxCase
	if sc == nil || sc.matcher == nil {
		return nil, mctx.Error("syntax: no pattern matcher available for template expansion")
	}

	// Get the template from value register
	templateVal := mctx.GetValue()
	template, ok := templateVal.(syntax.SyntaxValue)
	if !ok {
		return nil, mctx.Error(fmt.Sprintf("syntax: expected syntax template, got %T", templateVal))
	}

	// Expand the template using the matcher (handles ellipsis)
	// Use nil for intro scope and freeIds for now - hygiene can be added later
	expanded, err := sc.matcher.Expand(template, match.ExpandOptions{})
	if err != nil {
		return nil, mctx.WrapError(err, "syntax: template expansion error")
	}

	mctx.SetValue(expanded)
	mctx.pc++
	return mctx, nil
}

func (p *OperationSyntaxTemplateExpand) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationSyntaxTemplateExpand)
	return sameType(p, v, ok)
}

// OperationStoreSyntaxCaseInput stores the value register into the per-context
// syntaxCaseState for use by OperationSyntaxCaseMatch.
type OperationStoreSyntaxCaseInput struct {
	OperationBase
}

func NewOperationStoreSyntaxCaseInput() *OperationStoreSyntaxCaseInput {
	return &OperationStoreSyntaxCaseInput{
		OperationBase: NewOperationBaseWithGoName("operation:store-syntax-case-input", "StoreSyntaxCaseInput"),
	}
}

func (p *OperationStoreSyntaxCaseInput) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	sc := ensureSyntaxCaseState(mctx)
	val := mctx.GetValue()
	// Convert to syntax value if needed (handles Pairs, Vectors, etc.)
	if stx, ok := val.(syntax.SyntaxValue); ok {
		sc.input = stx
	} else {
		sc.input = schemeutil.DatumToSyntaxValue(ctx, nil, val)
	}
	mctx.pc++
	return mctx, nil
}

func (p *OperationStoreSyntaxCaseInput) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationStoreSyntaxCaseInput)
	return sameType(p, v, ok)
}

// OperationClearSyntaxCaseInput clears the global syntax-case input.
// This is called at the end of a syntax-case form.
type OperationClearSyntaxCaseInput struct {
	OperationBase
}

func NewOperationClearSyntaxCaseInput() *OperationClearSyntaxCaseInput {
	return &OperationClearSyntaxCaseInput{
		OperationBase: NewOperationBaseWithGoName("operation:clear-syntax-case-input", "ClearSyntaxCaseInput"),
	}
}

func (p *OperationClearSyntaxCaseInput) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	mctx.syntaxCase = nil
	mctx.pc++
	return mctx, nil
}

func (p *OperationClearSyntaxCaseInput) EqualTo(other values.Value) bool {
	v, ok := other.(*OperationClearSyntaxCaseInput)
	return sameType(p, v, ok)
}
