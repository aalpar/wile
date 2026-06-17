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

package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// forwardRefProgram references g from f's body before g is defined. As one
// (begin ...) compilation unit the reference resolves; compiled per-form it does
// not.
const forwardRefProgram = `(define (f) (g))
(define (g) 42)
(f)`

// TestEngine_EvalProgram_ForwardReference verifies EvalProgram compiles the whole
// input as a single (begin ...) unit, so a define may forward-reference a later
// define — file/program semantics (R22).
func TestEngine_EvalProgram_ForwardReference(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	result, err := eng.EvalProgram(ctx, forwardRefProgram, "<test>")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "42")
}

// TestEngine_EvalMultiple_ForwardReferenceFails documents the contrast EvalProgram
// encapsulates: EvalMultiple compiles each top-level form independently, so the
// forward reference fails.
func TestEngine_EvalMultiple_ForwardReferenceFails(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, forwardRefProgram)
	qt.Assert(t, err, qt.IsNotNil,
		qt.Commentf("EvalMultiple should fail the forward reference that EvalProgram allows"))
}

// TestEngine_EvalProgram_StructuralWrap covers the cases where building the
// (begin ...) wrapper structurally (parse-then-wrap) is more robust than the old
// string surgery ("(begin " + code + "\n)").
func TestEngine_EvalProgram_StructuralWrap(t *testing.T) {
	ctx := context.Background()

	t.Run("trailing line comment without newline", func(t *testing.T) {
		// A naive string wrap "(begin " + code + ")" would let the trailing comment
		// swallow the synthetic close paren; the structural wrapper has no such paren.
		eng, err := wile.NewEngine(ctx)
		qt.Assert(t, err, qt.IsNil)
		result, err := eng.EvalProgram(ctx,
			"(define x 41)\n(+ x 1) ; eof comment, no newline", "<test>")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result.SchemeString(), qt.Equals, "42")
	})

	t.Run("empty and comment-only programs are void", func(t *testing.T) {
		eng, err := wile.NewEngine(ctx)
		qt.Assert(t, err, qt.IsNil)
		for _, src := range []string{"", "   \n\t ", "; just a comment", "#| block |#"} {
			result, evalErr := eng.EvalProgram(ctx, src, "<test>")
			qt.Assert(t, evalErr, qt.IsNil, qt.Commentf("src=%q", src))
			qt.Assert(t, result.IsVoid(), qt.IsTrue, qt.Commentf("src=%q", src))
		}
	})
}
