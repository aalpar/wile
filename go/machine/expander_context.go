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
	"wile/environment"
	"wile/syntax"
)

// ExpanderContext provides access to the macro expander from within
// Scheme code during macro expansion. It is set on MachineContext
// when invoking macro transformers, enabling syntax-local-* primitives.
type ExpanderContext struct {
	env          *environment.EnvironmentFrame
	expander     *ExpanderTimeContinuation
	ectx         ExpandTimeCallContext
	introScope   *syntax.Scope // Introduction scope for current macro expansion
	useSiteScope *syntax.Scope // Use-site scope for binding forms
}

// NewExpanderContext creates a new ExpanderContext.
func NewExpanderContext(
	env *environment.EnvironmentFrame,
	expander *ExpanderTimeContinuation,
	ectx ExpandTimeCallContext,
) *ExpanderContext {
	return &ExpanderContext{
		env:      env,
		expander: expander,
		ectx:     ectx,
	}
}

// Env returns the environment frame associated with this context.
func (p *ExpanderContext) Env() *environment.EnvironmentFrame {
	return p.env
}

// Expand fully expands a syntax object.
func (p *ExpanderContext) Expand(stx syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expander.ExpandExpression(p.ectx, stx)
}

// ExpandOnce performs a single step of macro expansion.
// Returns (expanded-syntax, did-expand, error).
// If the input is a macro call, it expands it once and returns (result, true, nil).
// If the input is not a macro call, it returns (input, false, nil).
func (p *ExpanderContext) ExpandOnce(stx syntax.SyntaxValue) (syntax.SyntaxValue, bool, error) {
	return p.expander.ExpandOnce(p.ectx, stx)
}

// IntroductionScope returns the introduction scope for the current macro expansion.
// This scope is added to identifiers introduced by a macro and can be flipped
// using syntax-local-introduce.
func (p *ExpanderContext) IntroductionScope() *syntax.Scope {
	return p.introScope
}

// SetIntroductionScope sets the introduction scope for the current macro expansion.
func (p *ExpanderContext) SetIntroductionScope(scope *syntax.Scope) {
	p.introScope = scope
}

// UseSiteScope returns the use-site scope for binding forms.
// This scope is used by syntax-local-identifier-as-binding to mark
// identifiers as binding sites.
func (p *ExpanderContext) UseSiteScope() *syntax.Scope {
	return p.useSiteScope
}

// SetUseSiteScope sets the use-site scope for binding forms.
func (p *ExpanderContext) SetUseSiteScope(scope *syntax.Scope) {
	p.useSiteScope = scope
}
