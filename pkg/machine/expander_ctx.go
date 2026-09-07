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
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// ExpanderCtx abstracts the macro expansion context so that code needing
// expansion capabilities can depend on this interface rather than the
// concrete ExpanderContext. This enables the future machine/compilation
// sub-package to provide ExpanderContext without circular imports back
// to machine/.
type ExpanderCtx interface {
	Env() *environment.EnvironmentFrame
	Expand(syntax.SyntaxValue) (syntax.SyntaxValue, error)
	ExpandOnce(syntax.SyntaxValue) (syntax.SyntaxValue, bool, error)
	// MacroValue resolves id the way macro dispatch does and returns the value
	// its BindingTypeSyntax binding holds: a transformer, or a bare compile-time
	// value. The read side of syntax-local-value.
	MacroValue(id *syntax.SyntaxSymbol) (values.Value, bool)
	// ResolveFreeIdentifier answers the free-identifier=? question for id at the
	// current expansion: the definition-site pin first, then the use-site frame
	// under id's own scopes by the pattern-literal rules. ok is false on an
	// incomparable equal-cardinality tie.
	ResolveFreeIdentifier(id *syntax.SyntaxSymbol) (*environment.Binding, bool)
	IntroductionScope() *syntax.Scope
	SetIntroductionScope(*syntax.Scope)
	UseSiteScope() *syntax.Scope
	SetUseSiteScope(*syntax.Scope)
}

// Compile-time interface assertion lives in machine/compilation/expander_context.go:
//   var _ machine.ExpanderCtx = (*ExpanderContext)(nil)
