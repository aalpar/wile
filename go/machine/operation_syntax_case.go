// Copyright 2025 Aaron Alpar
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

	"wile/environment"
	"wile/match"
	"wile/syntax"
	"wile/utils"
	"wile/values"
)

// OperationSyntaxCaseMatch performs pattern matching for syntax-case.
//
// Expects:
//   - Value register: syntaxCaseClause with compiled pattern
//   - Top of eval stack: input syntax object
//
// Results:
//   - If match succeeds: value register = #t, pattern bindings stored in context
//   - If match fails: value register = #f
type OperationSyntaxCaseMatch struct{}

// syntaxCaseBindings stores pattern variable bindings from a successful match.
// This is stored in the machine context and accessed by OperationBindPatternVars.
var currentSyntaxCaseBindings map[string]syntax.SyntaxValue

// currentSyntaxCaseMatcher stores the matcher from the last successful match.
// This is needed for template expansion with ellipsis.
var currentSyntaxCaseMatcher *match.SyntaxMatcher

// currentSyntaxCaseInput stores the input syntax object being matched.
// This is used instead of the eval stack to avoid interference with procedure calls in the body.
var currentSyntaxCaseInput syntax.SyntaxValue

func NewOperationSyntaxCaseMatch() *OperationSyntaxCaseMatch {
	return &OperationSyntaxCaseMatch{}
}

func (p *OperationSyntaxCaseMatch) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	// Get the clause from value register
	clauseVal := mctx.GetValue()
	clause, ok := clauseVal.(*syntaxCaseClause)
	if !ok {
		return nil, mctx.Error(fmt.Sprintf("syntax-case: expected clause in value register, got %T", clauseVal))
	}

	// Get input from global (set by OperationStoreSyntaxCaseInput)
	if currentSyntaxCaseInput == nil {
		return nil, mctx.Error("syntax-case: no input available")
	}
	input := currentSyntaxCaseInput

	// Create a matcher
	matcher := match.NewSyntaxMatcherWithEllipsisVars(clause.patternVars, clause.bytecode, clause.ellipsisVars)

	// Try to match
	err := matcher.Match(input)
	if err != nil {
		// Match failed
		mctx.SetValue(values.FalseValue)
		mctx.pc++
		return mctx, nil
	}

	// Match succeeded - store bindings and matcher
	currentSyntaxCaseBindings = matcher.GetBindings()
	currentSyntaxCaseMatcher = matcher
	mctx.SetValue(values.TrueValue)
	mctx.pc++
	return mctx, nil
}

func (p *OperationSyntaxCaseMatch) String() string {
	return "SyntaxCaseMatch"
}

func (p *OperationSyntaxCaseMatch) SchemeString() string {
	return "#<operation:syntax-case-match>"
}

func (p *OperationSyntaxCaseMatch) EqualTo(other values.Value) bool {
	_, ok := other.(*OperationSyntaxCaseMatch)
	return ok
}

func (p *OperationSyntaxCaseMatch) IsVoid() bool {
	return false
}

// OperationBindPatternVars binds pattern variables from the last match
// into a new local environment frame that is pushed onto the current environment.
//
// This operation creates a new environment frame with local slots for each
// pattern variable, binds the matched values, and makes this the current environment.
type OperationBindPatternVars struct {
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
	return &OperationBindPatternVars{PatternVars: vars}
}

func (p *OperationBindPatternVars) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	if currentSyntaxCaseBindings == nil {
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
		stxVal, ok := currentSyntaxCaseBindings[varName]
		if ok && li != nil {
			childEnv.SetLocalValue(li, stxVal)
		}
	}

	// Switch to the new environment
	mctx.env = childEnv
	mctx.pc++
	return mctx, nil
}

func (p *OperationBindPatternVars) String() string {
	return "BindPatternVars"
}

func (p *OperationBindPatternVars) SchemeString() string {
	return "#<operation:bind-pattern-vars>"
}

func (p *OperationBindPatternVars) EqualTo(other values.Value) bool {
	o, ok := other.(*OperationBindPatternVars)
	if !ok {
		return false
	}
	if len(p.PatternVars) != len(o.PatternVars) {
		return false
	}
	for i, v := range p.PatternVars {
		if o.PatternVars[i] != v {
			return false
		}
	}
	return true
}

func (p *OperationBindPatternVars) IsVoid() bool {
	return false
}

// OperationSyntaxCaseNoMatch is emitted at the end of syntax-case when no clause matches.
type OperationSyntaxCaseNoMatch struct{}

func NewOperationSyntaxCaseNoMatch() *OperationSyntaxCaseNoMatch {
	return &OperationSyntaxCaseNoMatch{}
}

func (p *OperationSyntaxCaseNoMatch) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	return nil, mctx.Error("syntax-case: no matching clause")
}

func (p *OperationSyntaxCaseNoMatch) String() string {
	return "SyntaxCaseNoMatch"
}

func (p *OperationSyntaxCaseNoMatch) SchemeString() string {
	return "#<operation:syntax-case-no-match>"
}

func (p *OperationSyntaxCaseNoMatch) EqualTo(other values.Value) bool {
	_, ok := other.(*OperationSyntaxCaseNoMatch)
	return ok
}

func (p *OperationSyntaxCaseNoMatch) IsVoid() bool {
	return false
}

// OperationSyntaxTemplateExpand expands a syntax template using the current
// pattern variable bindings. This is used for templates containing ellipsis,
// which require runtime expansion rather than compile-time code generation.
//
// The template is stored in the value register (loaded from literals).
// The result is the expanded syntax object, left in the value register.
type OperationSyntaxTemplateExpand struct{}

func NewOperationSyntaxTemplateExpand() *OperationSyntaxTemplateExpand {
	return &OperationSyntaxTemplateExpand{}
}

func (p *OperationSyntaxTemplateExpand) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	if currentSyntaxCaseMatcher == nil {
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
	expanded, err := currentSyntaxCaseMatcher.ExpandWithIntroScope(template, nil, nil)
	if err != nil {
		return nil, mctx.WrapError(err, "syntax: template expansion error")
	}

	mctx.SetValue(expanded)
	mctx.pc++
	return mctx, nil
}

func (p *OperationSyntaxTemplateExpand) String() string {
	return "SyntaxTemplateExpand"
}

func (p *OperationSyntaxTemplateExpand) SchemeString() string {
	return "#<operation:syntax-template-expand>"
}

func (p *OperationSyntaxTemplateExpand) EqualTo(other values.Value) bool {
	_, ok := other.(*OperationSyntaxTemplateExpand)
	return ok
}

func (p *OperationSyntaxTemplateExpand) IsVoid() bool {
	return false
}

// OperationStoreSyntaxCaseInput stores the value register into the global
// currentSyntaxCaseInput for use by OperationSyntaxCaseMatch.
type OperationStoreSyntaxCaseInput struct{}

func NewOperationStoreSyntaxCaseInput() *OperationStoreSyntaxCaseInput {
	return &OperationStoreSyntaxCaseInput{}
}

func (p *OperationStoreSyntaxCaseInput) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	val := mctx.GetValue()
	// Convert to syntax value if needed (handles Pairs, Vectors, etc.)
	if stx, ok := val.(syntax.SyntaxValue); ok {
		currentSyntaxCaseInput = stx
	} else {
		currentSyntaxCaseInput = utils.DatumToSyntaxValue(nil, val)
	}
	mctx.pc++
	return mctx, nil
}

func (p *OperationStoreSyntaxCaseInput) String() string {
	return "StoreSyntaxCaseInput"
}

func (p *OperationStoreSyntaxCaseInput) SchemeString() string {
	return "#<operation:store-syntax-case-input>"
}

func (p *OperationStoreSyntaxCaseInput) EqualTo(other values.Value) bool {
	_, ok := other.(*OperationStoreSyntaxCaseInput)
	return ok
}

func (p *OperationStoreSyntaxCaseInput) IsVoid() bool {
	return false
}

// OperationClearSyntaxCaseInput clears the global syntax-case input.
// This is called at the end of a syntax-case form.
type OperationClearSyntaxCaseInput struct{}

func NewOperationClearSyntaxCaseInput() *OperationClearSyntaxCaseInput {
	return &OperationClearSyntaxCaseInput{}
}

func (p *OperationClearSyntaxCaseInput) Apply(ctx context.Context, mctx *MachineContext) (*MachineContext, error) {
	currentSyntaxCaseInput = nil
	mctx.pc++
	return mctx, nil
}

func (p *OperationClearSyntaxCaseInput) String() string {
	return "ClearSyntaxCaseInput"
}

func (p *OperationClearSyntaxCaseInput) SchemeString() string {
	return "#<operation:clear-syntax-case-input>"
}

func (p *OperationClearSyntaxCaseInput) EqualTo(other values.Value) bool {
	_, ok := other.(*OperationClearSyntaxCaseInput)
	return ok
}

func (p *OperationClearSyntaxCaseInput) IsVoid() bool {
	return false
}
