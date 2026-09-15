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
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// Compile-time check: ExpanderContext must satisfy machine.ExpanderCtx.
var _ machine.ExpanderCtx = (*ExpanderContext)(nil)

// ExpanderContext provides access to the macro expander from within
// Scheme code during macro expansion. It is set on machine.MachineContext
// when invoking macro transformers, enabling syntax-local-* primitives.
type ExpanderContext struct {
	env          *environment.EnvironmentFrame
	expander     *ExpanderTimeContinuation
	introScope   *syntax.Scope // Introduction scope for current macro expansion
	useSiteScope *syntax.Scope // Use-site scope for binding forms
}

// NewExpanderContext creates a new ExpanderContext.
func NewExpanderContext(
	env *environment.EnvironmentFrame,
	expander *ExpanderTimeContinuation,
) *ExpanderContext {
	return &ExpanderContext{
		env:      env,
		expander: expander,
	}
}

// Env returns the environment frame associated with this context.
func (p *ExpanderContext) Env() *environment.EnvironmentFrame {
	if p == nil {
		return nil
	}
	return p.env
}

// Expand fully expands a syntax object.
func (p *ExpanderContext) Expand(stx syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	if p == nil {
		return stx, nil
	}
	return p.expander.ExpandExpression(stx)
}

// ExpandOnce performs a single step of macro expansion.
// Returns (expanded-syntax, did-expand, error).
// If the input is a macro call, it expands it once and returns (result, true, nil).
// If the input is not a macro call, it returns (input, false, nil).
func (p *ExpanderContext) ExpandOnce(stx syntax.SyntaxValue) (syntax.SyntaxValue, bool, error) {
	if p == nil {
		return stx, false, nil
	}
	return p.expander.ExpandOnce(stx)
}

// MacroValue resolves id through lookupMacroBinding's arms — the let-syntax
// frames under the identifier's own scopes, the definition-site pin,
// NextPhase(), the owner's sealed phase-1 tier, the library env named by its
// scopes — and returns the BindingTypeSyntax binding's value (design §2.3). The
// eval extension's probe this replaces used Env().Expand(), the owner's
// top-level phase-1 frame, so every local frame was dropped and a let-syntax
// keyword was invisible at every phase.
func (p *ExpanderContext) MacroValue(id *syntax.SyntaxSymbol) (values.Value, bool) {
	if p == nil || p.expander == nil {
		return nil, false
	}
	bnd := p.expander.lookupMacroBinding(id, id.Scopes())
	if bnd == nil {
		return nil, false
	}
	return bnd.Value(), true
}

// ResolveFreeIdentifier implements design §3.1: a ResolvedBinding pin (which
// quote-syntax stamps on a literal) wins before any env lookup. That is
// deliberately NOT lookupMacroBinding's D2 order, which consults the pin between
// arms 1 and 2 so a co-introduced keyword still shadows it: free-identifier=?
// asks what the identifier denoted where it was written, and the pin records
// exactly that, so a use-site binder of the same spelling has no claim on it.
// Otherwise lookupLiteralBinding resolves id in the use-site env under its own
// scopes — the frame's own lexical chain at its own phase, the dialect's bulk
// rows last — which is what the Go matchers apply to a pattern literal. No fallback
// phases: the use site's phase is a known fact, and a pinned literal never
// reaches this arm.
func (p *ExpanderContext) ResolveFreeIdentifier(id *syntax.SyntaxSymbol) (*environment.Binding, bool) {
	if p == nil {
		return nil, false
	}
	gi, ok := id.ResolvedBinding.(*environment.GlobalIndex)
	if ok && gi != nil && gi.Env != nil {
		pinned := gi.Env.GetOwnGlobalBinding(gi)
		if pinned != nil {
			return pinned, true
		}
	}
	return lookupLiteralBinding(p.env, id.Key(), id.Scopes(), nil)
}

// IntroductionScope returns the introduction scope for the current macro expansion.
// This scope is added to identifiers introduced by a macro and can be flipped
// using syntax-local-introduce.
func (p *ExpanderContext) IntroductionScope() *syntax.Scope {
	if p == nil {
		return nil
	}
	return p.introScope
}

// SetIntroductionScope sets the introduction scope for the current macro expansion.
func (p *ExpanderContext) SetIntroductionScope(scope *syntax.Scope) {
	if p == nil {
		return
	}
	p.introScope = scope
}

// UseSiteScope returns the use-site scope for binding forms.
// This scope is used by syntax-local-identifier-as-binding to mark
// identifiers as binding sites.
func (p *ExpanderContext) UseSiteScope() *syntax.Scope {
	if p == nil {
		return nil
	}
	return p.useSiteScope
}

// SetUseSiteScope sets the use-site scope for binding forms.
func (p *ExpanderContext) SetUseSiteScope(scope *syntax.Scope) {
	if p == nil {
		return
	}
	p.useSiteScope = scope
}
