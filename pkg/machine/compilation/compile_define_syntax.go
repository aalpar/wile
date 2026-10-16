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

// CompileDefineSyntax handles (define-syntax keyword [docstring] transformer-expr)
// for the compiler.
//
// The right-hand side has ALREADY RUN and the keyword is already bound:
// expandDefineSyntax ran compileDefineSyntaxFromSyntax when the expander reached
// the form. So this only re-checks the shape and emits nothing. Running it here
// as well would evaluate the right-hand side twice, repeating its side effects
// (I138).
//
// Reference: R7RS Section 5.4 (Syntax definitions)
func (p *CompileTimeContinuation) CompileDefineSyntax(_ CompileTimeCallContext, expr syntax.SyntaxValue) error {
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
	_, ok := parts[0].(*syntax.SyntaxSymbol)
	if !ok {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "define-syntax: keyword must be a symbol"))
	}
	_, _, err = splitDefineSyntaxRest(parts[1:])
	if err != nil {
		return p.wrapCompilationError(err)
	}
	return nil
}

// createPhaseBindingUnlessStable is the one-phase-up create used by every
// syntax-definition writer that stores through a pinned index rather than
// through DefineOwnGlobal: compileDefineSyntaxFromSyntax and
// runDefineForSyntax. It refuses a write that would land on an
// already-Stable slot, and returns the pinned index otherwise.
//
// Those sites deliberately do NOT use DefineOwnGlobal — a phase-1
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
// define-syntax (R7RS §5.3.1), which is what compileDefineSyntaxFromSyntax's
// m.Imported reset exists for. registry.Apply is the only writer of the Stable field
// on a global, so this can only fire against a registry primitive copy.
//
// The second result reports whether THIS call created the slot. The
// define-syntax site predeclares before compiling the right-hand side, so it
// needs it to know whether a failed right-hand side owes a DeleteOwnGlobal: a
// failed redefinition must keep the previous transformer.
func createPhaseBindingUnlessStable(
	expandEnv *environment.EnvironmentFrame,
	sym *values.Symbol,
	bt environment.BindingType,
	scopes syntax.Scopes,
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
