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

// TestEngine_CheckProgram_UnboundBindingIsReported pins the check that motivates
// the whole mode: a name that resolves nowhere is a compile-time error, reported
// with the program's source label, even though the reference sits inside a
// procedure body that is never called.
func TestEngine_CheckProgram_UnboundBindingIsReported(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	err = eng.CheckProgram(ctx, `(define (g) (nope 1))`, "t.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, `no such binding "nope"`)
	qt.Assert(t, err.Error(), qt.Contains, "t.scm:")
}

// TestEngine_CheckProgram_DoesNotExecute is the differential proof that no user
// code runs: (car '()) compiles clean and fails only at run time, so CheckProgram
// returning nil where EvalProgram returns the runtime error isolates exactly the
// omitted execution step.
//
// Note this cannot be tested by observing whether a (define ...) became visible —
// compiling a define registers its binding (see
// TestEngine_CheckProgram_RegistersTopLevelBindings), so binding visibility
// proves nothing about execution either way.
func TestEngine_CheckProgram_DoesNotExecute(t *testing.T) {
	ctx := context.Background()
	const src = `(car '())`

	checkEng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, checkEng.CheckProgram(ctx, src, "t.scm"), qt.IsNil)

	evalEng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)
	_, evalErr := evalEng.EvalProgram(ctx, src, "t.scm")
	qt.Assert(t, evalErr, qt.IsNotNil,
		qt.Commentf("the differential is meaningless unless running this does fail"))
}

// TestEngine_CheckProgram_RegistersTopLevelBindings pins the surprising half of
// "compile without run": the top-level bindings ARE created, because the compiler
// must register them for forward references to resolve. Only the initialising
// values are never computed. Checking two programs on one engine therefore shares
// a namespace, and re-checking the same program trips the immutable redefine
// guard — both documented on CheckProgram.
func TestEngine_CheckProgram_RegistersTopLevelBindings(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	qt.Assert(t, eng.CheckProgram(ctx, `(define registered 1)`, "a.scm"), qt.IsNil)
	qt.Assert(t, eng.CheckProgram(ctx, `registered`, "b.scm"), qt.IsNil,
		qt.Commentf("a.scm's define must be visible to b.scm's reference"))

	err = eng.CheckProgram(ctx, `(define registered 2)`, "a.scm")
	qt.Assert(t, err, qt.IsNotNil,
		qt.Commentf("re-checking the same define must trip the redefine guard"))
	qt.Assert(t, err.Error(), qt.Contains, "cannot redefine immutable top-level binding")
}

// TestEngine_CheckProgram_EmptyInputIsClean covers the len(forms) == 0 early
// return: whitespace and comments are not an error.
func TestEngine_CheckProgram_EmptyInputIsClean(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	qt.Assert(t, eng.CheckProgram(ctx, "", "t.scm"), qt.IsNil)
	qt.Assert(t, eng.CheckProgram(ctx, "; just a comment\n", "t.scm"), qt.IsNil)
}

// TestEngine_CheckProgram_ParseErrorIsReported covers the read loop's non-EOF
// branch, which wraps as a parse error rather than a compile error.
func TestEngine_CheckProgram_ParseErrorIsReported(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	err = eng.CheckProgram(ctx, `(define (g) `, "t.scm")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "parse error")
}

// TestEngine_CheckProgram_ForwardReferenceResolves pins that CheckProgram keeps
// EvalProgram's whole-program semantics: every top-level form is spliced into one
// (begin ...) unit, so a define may reference a later one. Checking form-by-form
// would report a spurious unbound-name error here.
func TestEngine_CheckProgram_ForwardReferenceResolves(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)

	qt.Assert(t, eng.CheckProgram(ctx, forwardRefProgram, "t.scm"), qt.IsNil)
}
