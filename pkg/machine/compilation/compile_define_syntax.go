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
	"strings"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// CompileDefineSyntax handles (define-syntax keyword transformer-expr).
//
// This is the compile-time handler for R7RS define-syntax. Unlike most
// definitions, define-syntax is processed entirely at compile time:
//
//  1. Parse the form: (define-syntax keyword (syntax-rules ...))
//  2. Evaluate the right-hand side as an expression one phase up; the value is
//     the transformer
//  3. Store the transformer in the environment with BindingTypeSyntax
//  4. Emit NO runtime operations (the binding is already established)
//
// The BindingTypeSyntax marker is crucial: when the expander encounters
// a symbol, it checks if that symbol is bound to a syntax transformer.
// If so, it invokes the transformer closure to expand the macro.
//
// This is how derived expressions like 'let' work: they're defined as
// macros using define-syntax, and expand to lambda expressions:
//
//	(define-syntax let
//	  (syntax-rules ()
//	    ((let ((name val) ...) body)
//	     ((lambda (name ...) body) val ...))))
//
// Reference: R7RS Section 5.4 (Syntax definitions)
func (p *CompileTimeContinuation) CompileDefineSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	err := p.ensureState("define-syntax")
	if err != nil {
		return err
	}
	// expr is (keyword [docstring] transformer-expr) — the define-syntax keyword
	// was stripped by syntaxCompiler in register.go.
	parts, err := syntax.FormParts(expr, "define-syntax", 2, 3)
	if err != nil {
		return p.wrapCompilationError(err)
	}
	keywordSym, ok := parts[0].(*syntax.SyntaxSymbol)
	if !ok {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "define-syntax: keyword must be a symbol"))
	}
	keyword := keywordSym.Unwrap().(*values.Symbol)
	transformerExpr, docstring, err := splitDefineSyntaxRest(parts[1:])
	if err != nil {
		return p.wrapCompilationError(err)
	}

	// Store one phase up from the defining frame (relative, not the absolute
	// expand phase; see NextPhase). R7RS requires syntax bindings to live one
	// phase above the code that uses them; NextPhase() keeps this relative so a
	// define-syntax inside a transformer body climbs rather than collapsing into
	// phase 1. Lookup (expander_time_continuation.go) consults the same
	// NextPhase() so storage and lookup stay symmetric. At phaseLevel 0 this
	// equals Expand() (level-0 identity).
	//
	// The slot is created BEFORE the right-hand side compiles so a template's
	// reference to the macro's own name resolves to a real, pinned GlobalIndex
	// (its Env set, its value unset until the store below): the self-reference
	// pins to it directly, which is what pinTemplateSelfReferences used to
	// back-patch after the fact (design §2.2).
	//
	// The scope set is the CREATION key, not a post-stamp. Creation dedupes with
	// scopeSetsEqual, so creating under nil compares against the EMPTY set and a
	// macro-introduced {intro} keyword reuses — then re-stamps — a pre-existing
	// empty-scoped binding. The returned index is PINNED to the slot this call
	// landed on, at the writing view's own coordinates, so the write below cannot
	// drift onto another slot of the same name — in particular not onto a
	// same-named mutable entry when this view is the sealed-write one (a bootstrap
	// macro), which would write the transformer where a user can overwrite it.
	expandEnv := p.env.NextPhase()
	symbolScopes := keywordSym.Scopes()
	globalIndex, created, err := createPhaseBindingUnlessStable(expandEnv, keyword, environment.BindingTypeSyntax, symbolScopes, "define-syntax")
	if err != nil {
		return p.wrapCompilationError(err)
	}

	transformer, err := compileTransformerValue(ctctx.ctx, p.env, transformerExpr, p.libraryScope, p.evaluator)
	if err != nil {
		// A failed right-hand side must not leave the keyword bound to nothing:
		// a later reference would reach the empty slot and fail as "not a
		// closure". Only a slot THIS call created is removed; a redefinition
		// that fails keeps the previous transformer.
		if created {
			expandEnv.DeleteOwnGlobal(keyword, symbolScopes)
		}
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "could not compile transformer"))
	}

	// Provenance and docstring only — the scope set is now the creation key, not
	// a post-stamp. The metadata write goes through UpdateMeta (copy-on-write
	// CAS) rather than a write-through pointer, because this binding is a shared
	// global under concurrent SRFI-18 compiles.
	binding := expandEnv.GlobalEnvironment().GetOwnGlobalBinding(globalIndex)
	if binding != nil {
		binding.UpdateMeta(func(m *environment.BindingMeta) bool {
			// Top-level define-syntax supersedes an imported binding
			// (R7RS §5.3.1), mirroring the variable path in compile_define.go:
			// the transformer written below lands in the SAME slot, so the
			// import *provenance* goes with it. Without this, IsStable()
			// (m.Imported || m.Stable) keeps reporting "cannot be rebound"
			// about a macro the user has just rebound.
			m.Imported = false
			if docstring != "" {
				m.Doc = docstring
			}
			return true
		})
	}

	err = expandEnv.SetOwnGlobalValue(globalIndex, transformer)
	if err != nil {
		return p.wrapCompilationError(werr.WrapForeignErrorf(err, "define-syntax: failed to store transformer for %s", keyword.Key))
	}

	// define-syntax is compile-time only, emit no runtime operations
	return nil
}

// createPhaseBindingUnlessStable is the one-phase-up create used by every
// syntax-definition writer that stores through a pinned index rather than
// through DefineOwnGlobal: CompileDefineSyntax, CompileDefineForSyntax and
// compileDefineSyntaxFromSyntax. It refuses a write that would land on an
// already-Stable slot, and returns the pinned index otherwise.
//
// Those three sites deliberately do NOT use DefineOwnGlobal — a phase-1
// definition is R7RS-legal whatever the name, and the user-level case writes at
// (1, mutable) where nothing is Stable. But the SAME sites compile bootstrap
// sources, whose defining frame is the owner's phase-0 seal, so NextPhase()
// hands them the (1, SEALED) view — the exact coordinate registry.Apply's
// phaseTargets now writes its expand-phase primitive copies to. Without this
// guard a registry that supplies both an expand-phase primitive and a bootstrap
// macro of one name silently overwrote the primitive's slot in place, keeping
// its BindingTypeVariable tag and its Stable stamp: a transformer stored where
// no lookup finds it as syntax, under a stamp asserting the writer set is
// closed.
//
// m.Stable, not IsStable(): an IMPORTED binding must stay supersedable by a
// define-syntax (R7RS §5.3.1), which is what the m.Imported reset below the
// call sites exists for. registry.Apply is the only writer of the Stable field
// on a global, so this can only fire against a registry primitive copy.
//
// The second result reports whether THIS call created the slot. The two
// define-syntax sites predeclare before compiling the right-hand side, so they
// need it to know whether a failed right-hand side owes a DeleteOwnGlobal: a
// failed redefinition must keep the previous transformer.
func createPhaseBindingUnlessStable(
	expandEnv *environment.EnvironmentFrame,
	sym *values.Symbol,
	bt environment.BindingType,
	scopes []*syntax.Scope,
	form string,
) (*environment.GlobalIndex, bool, error) {
	gi, created := expandEnv.MaybeCreateOwnGlobalBinding(sym, bt, scopes)
	if created {
		return gi, true, nil
	}
	b := expandEnv.GlobalEnvironment().GetOwnGlobalBinding(gi)
	if b == nil {
		return gi, false, nil
	}
	m := b.Meta()
	if m == nil || !m.Stable {
		return gi, false, nil
	}
	return nil, false, werr.WrapForeignErrorf(
		werr.ErrImmutableBinding,
		"%s: cannot rebind stable binding %q at the defining frame's next phase",
		form, sym.Key,
	)
}

// splitDefineSyntaxRest separates an optional leading docstring from the
// transformer expression in a define-syntax form. rest is the operand slice
// that follows the macro keyword name — either [transformer] or
// [docstring, transformer]. When a docstring is present it must be a string
// literal, mirroring the Guile-style leading-string docstring that define
// accepts on procedure bodies. FormParts has already bounded len(rest) to
// [1, 2], so a length other than 1 is treated as the two-operand case.
func splitDefineSyntaxRest(rest []syntax.SyntaxValue) (syntax.SyntaxValue, string, error) {
	if len(rest) < 2 {
		return rest[0], "", nil
	}
	str, ok := rest[0].UnwrapAll().(*values.String)
	if !ok {
		return nil, "", werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "define-syntax: docstring must be a string literal")
	}
	return rest[1], strings.TrimSpace(str.Value), nil
}
