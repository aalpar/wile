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

// Phase-0 references to phase-1 meanings were ALREADY refused — by phase
// placement, with ErrNoSuchBinding — and must stay exactly that (measured M3).
// This is the corrected form of the design's "if/lambda still ErrNoSuchBinding"
// expectation: it holds at phase 0, and Phase A must not change the sentinel
// there (the dialect removed-form contract depends on it).
func TestPhaseZeroCrossPhaseNamesStayUnbound(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"primitive expander name", `(display if)`},
		{"bootstrap macro name", `(display when)`},
		// begin-wrapped for the same reason as TestPhaseOneHandlerValuePositionRefused
		// above: RunSchemeCode only reads/runs the first top-level form otherwise.
		{"user macro name", `(begin (define-syntax m2 (syntax-rules () ((_) 1))) (display m2))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
				qt.Commentf("got: %v", err))
			qt.Assert(t, errors.Is(err, werr.ErrSyntacticKeywordAsVariable), qt.IsFalse,
				qt.Commentf("sentinel must stay ErrNoSuchBinding: %v", err))
		})
	}
}
