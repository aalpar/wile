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

package compilation_test

import (
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/werr"
)

// A compile-time meaning referenced in value position is a compile error, not a
// leaked handler (design P6). BindingType is the single authority: the refusal
// keys on the tag at the one ordinary reaching site (emitCachedBindingLoad),
// never on which frame the walk landed in.
//
// See plans/2026-08-05-flat-binding-model-{design,impl}.local.md §6 / Task 1.

// Every syntax compiler name, in value position, at phase 0. The table is the
// registration table itself (SyntaxCompilerNamesForTest), so a new syntax
// compiler is born refused. Before this change every one of these printed
// #<syntax-compiler:NAME> (measured M1).
func TestSyntaxCompilerValuePositionRefused(t *testing.T) {
	for _, name := range compilation.SyntaxCompilerNamesForTest() {
		t.Run(name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, "(display "+name+")")
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrSyntacticKeywordAsVariable), qt.IsTrue,
				qt.Commentf("got: %v", err))
		})
	}
}

// Phase-1 value references to compile-time meanings are the same defect one
// phase up (measured M2: when leaked its transformer, if leaked its expander).
// The control row pins that a phase-1 VARIABLE — the expand-phase registry
// copy of car (M7) — still loads.
func TestPhaseOneHandlerValuePositionRefused(t *testing.T) {
	tcs := []struct {
		name    string
		code    string
		wantErr error // nil means must succeed
	}{
		{"bootstrap macro transformer", `(begin-for-syntax (display when))`, werr.ErrSyntacticKeywordAsVariable},
		{"primitive expander", `(begin-for-syntax (display if))`, werr.ErrSyntacticKeywordAsVariable},
		// RunSchemeCode reads a single top-level form (Parser.ReadSyntax reads one
		// datum per call; see pkg/parser/CLAUDE.local.md and the MEMORY.md gotcha on
		// EvalMultipleWithSource): a bare two-form string silently drops the second
		// form with no error. begin-wrapping is the established fix elsewhere in
		// this package's tests (e.g. pkg/internal/validate/validate_define_test.go).
		{"user transformer at its own phase", `(begin (define-syntax m (syntax-rules () ((_) 1)))
			(begin-for-syntax (display m)))`, werr.ErrSyntacticKeywordAsVariable},
		{"expand-phase primitive is a variable", `(begin-for-syntax (display car))`, nil},
		{"define-for-syntax value is a variable", `(begin (define-for-syntax fv 41)
			(begin-for-syntax (display fv)))`, nil},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.code)
			if tc.wantErr == nil {
				qt.Assert(t, err, qt.IsNil)
				return
			}
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, tc.wantErr), qt.IsTrue, qt.Commentf("got: %v", err))
		})
	}
}

// A phase-0 reference to a phase-1 meaning is refused, and WHICH sentinel it
// gets is decided by whether the name is also an ambient keyword, not by phase
// placement alone. The two answers are the point of the table.
//
//   - `if` is a compileTimeBindingSpecs name, so registerCompileTimeBinding
//     installs an ambient BindingTypePrimitive binding that a phase-0 probe
//     reaches as T3. refuseCompileTimeMeaning's type arm answers it first:
//     ErrSyntacticKeywordAsVariable, the more specific verdict, and the same
//     class Chez ("invalid syntax if") and Racket give.
//   - A bootstrap macro or a user macro has NO ambient keyword (it exists only
//     at phase 1), so nothing is reachable from phase 0 and the reference is
//     unbound. That arm still pins ErrNoSuchBinding, negative assertion included.
//
// The dialect removed-form contract no longer rides on the first row. Since the
// keywords became ambient it is carried explicitly, by WithoutBindings at engine
// init (removedFormNames, pkg/wile/engine.go): a form the dialect removed loses
// its keyword with it, so a reference to it is unbound rather than a keyword the
// engine does not have. TestWithDialect_RemovesForm is that pin.
func TestPhaseZeroCrossPhaseNamesStayUnbound(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want error
	}{
		{"primitive expander name", `(display if)`, werr.ErrSyntacticKeywordAsVariable},
		{"bootstrap macro name", `(display when)`, werr.ErrNoSuchBinding},
		// begin-wrapped for the same reason as TestPhaseOneHandlerValuePositionRefused
		// above: RunSchemeCode only reads/runs the first top-level form otherwise.
		{"user macro name", `(begin (define-syntax m2 (syntax-rules () ((_) 1))) (display m2))`, werr.ErrNoSuchBinding},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, tc.want), qt.IsTrue,
				qt.Commentf("got: %v", err))
			if errors.Is(tc.want, werr.ErrNoSuchBinding) {
				// The name has no ambient keyword, so the keyword refusal must not
				// be what answered; otherwise the row would pass for the wrong
				// reason and stop discriminating the two mechanisms.
				qt.Assert(t, errors.Is(err, werr.ErrSyntacticKeywordAsVariable), qt.IsFalse,
					qt.Commentf("sentinel must stay ErrNoSuchBinding: %v", err))
			}
		})
	}
}

// The same refusal on the PINNED path. A free identifier in a macro template
// carries its definition-time *GlobalIndex (cross-library hygiene), and
// tryResolvedBinding consumes that pin BEFORE ordinary resolution — so the tag
// check at the ordinary site above never sees these. Its own tag check is what
// refuses them, and it falls through rather than raising, because there the
// caller still has ordinary resolution to try. Where that chain ends is the same
// split TestPhaseZeroCrossPhaseNamesStayUnbound draws: a name that is also an
// ambient keyword (`if`) is answered by the type arm on the ordinary path, and
// one that exists only at phase 1 (a bootstrap or user macro) ends unbound.
//
// Without that check the pin is emitted as a cached load and the value world
// gets the transformer closure or the expander itself — exactly the leak the
// predecessor predicate (a compileTimeHandler type assertion) missed for a
// pinned macro TRANSFORMER, which is a plain machine closure and matched
// nothing.
func TestPinnedTemplateIdentifierRefusesCompileTimeMeaning(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want error
	}{
		{"bootstrap macro in template", `(begin (define-syntax m3 (syntax-rules () ((_) (display when))))
			(m3))`, werr.ErrNoSuchBinding},
		{"primitive expander in template", `(begin (define-syntax m4 (syntax-rules () ((_) (display if))))
			(m4))`, werr.ErrSyntacticKeywordAsVariable},
		{"user transformer in template", `(begin (define-syntax inner (syntax-rules () ((_) 1)))
			(define-syntax m5 (syntax-rules () ((_) (display inner))))
			(m5))`, werr.ErrNoSuchBinding},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, tc.want), qt.IsTrue,
				qt.Commentf("got: %v", err))
		})
	}
}

// Every compile-time-only NAME (auxiliary syntax and the special forms whose
// docstrings ride on a BindingSpec) is ambient, so a phase-0 reference in value
// position reaches it and is refused as a keyword. Before the relocation these
// sat at phase 2, unreachable from phase 0, and (display if) was the less
// specific "no such binding". Chez ("invalid syntax if") and Racket give the
// same class of verdict.
func TestKeywordValuePositionRefusedAtPhaseZero(t *testing.T) {
	for _, name := range []string{"if", "define", "lambda", "else", "=>"} {
		t.Run(name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, "(display "+name+")")
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrSyntacticKeywordAsVariable), qt.IsTrue,
				qt.Commentf("got: %v", err))
		})
	}
}
