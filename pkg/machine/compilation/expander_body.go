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

// expander_body.go contains body-processing logic for the macro expander:
// expanding body forms with define-syntax support (R7RS §5.3), and the
// helpers that detect and compile define-syntax during expansion.
//
// Extracted from expander_time_continuation.go.

import (
	"context"

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// extractDefineName extracts the name being defined from a define form.
// Returns nil if the form is not a define or is malformed.
//
// Note: This intentionally excludes define-syntax forms. Macro bindings are
// handled separately by compileDefineSyntaxFromSyntax which stores them in the
// expand environment. We only pre-register define bindings so that macros can
// reference forward-declared variable definitions.
//
// Handles:
//   - (define name value)
//   - (define (name args...) body...)
func extractDefineName(form syntax.SyntaxValue) *syntax.SyntaxSymbol {
	// Only handle define, not define-syntax (macros are handled separately).
	pair, ok := asSyntaxFormWithKeyword(form, "define")
	if !ok {
		return nil
	}

	cdr := pair.SyntaxCdr()
	cdrPair, ok := cdr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdrPair) {
		return nil
	}

	second := cdrPair.SyntaxCar()
	switch s := second.(type) {
	case *syntax.SyntaxSymbol:
		// (define name ...)
		return s
	case *syntax.SyntaxPair:
		// (define (name args...) body...) - extract name from the pair
		if !syntax.IsSyntaxEmptyList(s) {
			nameExpr, ok := s.SyntaxCar().(*syntax.SyntaxSymbol)
			if ok {
				return nameExpr
			}
		}
	}
	return nil
}

// ExpandBodyWithDefineSyntax expands a sequence of body forms, compiling
// define-syntax forms as encountered so subsequent forms can use the macros.
//
// This unifies the expansion pattern used by:
// - Lambda bodies (internal define-syntax)
// - Library bodies (top-level define-syntax)
// - Include files (top-level define-syntax)
//
// R7RS §5.3: Internal define-syntax forms must be processed before expanding
// subsequent body expressions so that locally-defined macros are visible.
//
// R7RS §5.3.2: Bodies use letrec* semantics where all defined names are visible
// to all initializers. This enables forward references within macros - a macro
// can reference a definition that appears later in the same body.
func (p *ExpanderTimeContinuation) ExpandBodyWithDefineSyntax(
	forms []syntax.SyntaxValue,
) ([]syntax.SyntaxValue, error) {
	// Pre-scan: Register placeholder bindings for all define forms.
	// This enables forward hygienic references within the body (R7RS letrec* semantics).
	// Note: define-syntax is handled in pass 2 below — macro bindings live in the
	// expand environment, not the runtime environment pre-declared here.
	for _, form := range forms {
		nameSym := extractDefineName(form)
		if nameSym != nil {
			name := nameSym.Unwrap().(*values.Symbol)
			predeclareBinding(p.env, name, nameSym.Scopes(), nameSym.SourceContext())
		}
	}

	// Now expand sequentially with all bindings visible
	var result []syntax.SyntaxValue
	for _, form := range forms {
		expanded, err := p.ExpandExpression(form)
		if err != nil {
			return nil, wrapSourcedError(form.SourceContext(), werr.WrapForeignErrorf(err, "body: failed to expand expression"))
		}

		// If define-syntax, compile it now for subsequent forms
		if isSyntaxFormWithKeyword(expanded, "define-syntax") {
			pair := expanded.(*syntax.SyntaxPair)
			err = compileDefineSyntaxFromSyntax(p.ctx, p.env, pair, p.libraryScope, p.evaluator)
			if err != nil {
				return nil, wrapSourcedError(expanded.SourceContext(), werr.WrapForeignErrorf(err, "body: failed to compile define-syntax"))
			}
		}

		result = append(result, expanded)
	}
	return result, nil
}

// compileDefineSyntaxFromSyntax compiles a define-syntax form and stores the transformer
// in the expand environment.
//
// The env parameter is used for free identifier resolution during compilation (so macros
// can see local bindings like lambda parameters), while the actual macro binding is stored
// one phase up from the defining frame (env.NextPhase()) for lookup during expansion. The
// target is relative, not the absolute expand phase, so an internal define-syntax inside a
// transformer body climbs symmetrically with the expander's NextPhase() lookup rather than
// desyncing (both collapse to Expand() at phaseLevel 0 — level-0 identity).
func compileDefineSyntaxFromSyntax(ctx context.Context, env *environment.EnvironmentFrame, dsPair *syntax.SyntaxPair, libraryScope *syntax.Scope, evaluator machine.MacroEvaluator) error {
	expandEnv := env.NextPhase()

	// Extract (define-syntax keyword [docstring] transformer) — three or four
	// elements counting the keyword. Matches CompileDefineSyntax on the
	// top-level path, including the optional Guile-style docstring.
	parts, err := syntax.FormParts(dsPair, "define-syntax", 3, 4)
	if err != nil {
		return wrapSourcedError(dsPair.SourceContext(), err)
	}
	keywordSym, ok := parts[1].(*syntax.SyntaxSymbol)
	if !ok {
		return wrapSourcedError(dsPair.SourceContext(), werr.WrapForeignErrorf(werr.ErrNotASymbol, "define-syntax: keyword must be a symbol"))
	}
	keyword := keywordSym.Unwrap().(*values.Symbol)
	symbolScopes := keywordSym.Scopes()

	transformer, docstring, err := splitDefineSyntaxRest(parts[2:])
	if err != nil {
		return wrapSourcedError(dsPair.SourceContext(), err)
	}

	// Compile the transformer using the full environment for free identifier resolution
	// This allows macros to see local bindings (e.g., lambda parameters, forward references)
	// Supports both syntax-rules and lambda (procedural) transformers
	closure, err := compileTransformerToMachineClosure(ctx, env, transformer, libraryScope, evaluator)
	if err != nil {
		return wrapSourcedError(dsPair.SourceContext(), werr.WrapForeignErrorf(err, "define-syntax: failed to compile transformer for %s", keyword.Key))
	}

	// Store in the expand environment (for macro lookup during expansion)
	globalIndex, _ := expandEnv.MaybeCreateOwnGlobalBinding(keyword, environment.BindingTypeSyntax, nil)
	binding := expandEnv.GetGlobalBinding(globalIndex)
	if binding != nil {
		binding.UpdateMeta(func(m *environment.BindingMeta) bool {
			changed := false
			if symbolScopes != nil {
				m.Scopes = symbolScopes
				changed = true
			}
			symbolSource := keywordSym.SourceContext()
			if symbolSource != nil {
				m.Source = symbolSource
				changed = true
			}
			if docstring != "" {
				m.Doc = docstring
				changed = true
			}
			return changed
		})
	}
	return expandEnv.SetOwnGlobalValue(globalIndex, closure)
}
