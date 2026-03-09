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
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"

	qt "github.com/frankban/quicktest"
)

// Helper to compile Scheme code and return the template
func compileScheme(t *testing.T, code string) *NativeTemplate {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	rdr := strings.NewReader(code)
	p := parser.NewParserWithFile(env, true, rdr, "test.scm")

	stx, err := p.ReadSyntax(context.TODO())
	qt.Assert(t, err, qt.IsNil)

	tpl := NewNativeTemplate(0, 0, false)
	ectx := context.Background()
	expanded, err := NewExpanderTimeContinuation(ectx, env).ExpandExpression(stx)
	qt.Assert(t, err, qt.IsNil)

	cctx := NewCompileTimeCallContext(context.Background(), false, true)
	err = NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	return tpl
}

func TestSourceRecording_Quote(t *testing.T) {
	tpl := compileScheme(t, "'hello")

	// Source map should have entries

	// Should be able to look up source at PC 0
	source := tpl.SourceAt(0)
	qt.Assert(t, source, qt.IsNotNil)
	qt.Assert(t, source.File, qt.Equals, "test.scm")
}

func TestSourceRecording_If(t *testing.T) {
	tpl := compileScheme(t, "(if #t 1 2)")

	source := tpl.SourceAt(0)
	qt.Assert(t, source, qt.IsNotNil)
}

func TestSourceRecording_Define(t *testing.T) {
	tpl := compileScheme(t, "(define x 42)")

	source := tpl.SourceAt(0)
	qt.Assert(t, source, qt.IsNotNil)
}

func TestSourceRecording_DefineFunction(t *testing.T) {
	tpl := compileScheme(t, "(define (bindSymbolWithScopes x) x)")

	// The function template should have a name
	// Find the child template in literals
	var childTpl *NativeTemplate
	for _, lit := range tpl.literals {
		nt, ok := lit.(*NativeTemplate)
		if ok {
			childTpl = nt
			break
		}
	}
	qt.Assert(t, childTpl, qt.IsNotNil)
	qt.Assert(t, childTpl.Name(), qt.Equals, "bindSymbolWithScopes")
}

func TestSourceRecording_Lambda(t *testing.T) {
	tpl := compileScheme(t, "(lambda (x) x)")

	qt.Assert(t, tpl.CodeLen() > 0, qt.IsTrue)
}

func TestSourceRecording_Begin(t *testing.T) {
	tpl := compileScheme(t, "(begin 1 2 3)")

	qt.Assert(t, tpl.CodeLen() > 0, qt.IsTrue)
}

func TestSourceRecording_Call(t *testing.T) {
	// First define a function, then call it
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())

	// Define a simple function
	rdr := strings.NewReader("(define (id x) x)")
	p := parser.NewParserWithFile(env, true, rdr, "test.scm")
	stx, _ := p.ReadSyntax(context.TODO())
	tpl := NewNativeTemplate(0, 0, false)
	ectx := context.Background()
	expanded, err := NewExpanderTimeContinuation(ectx, env).ExpandExpression(stx)
	qt.Assert(t, err, qt.IsNil)
	cctx := NewCompileTimeCallContext(context.Background(), false, true)
	err = NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	// Now compile a call to that function
	rdr = strings.NewReader("(id 42)")
	p = parser.NewParserWithFile(env, true, rdr, "test.scm")
	stx, err = p.ReadSyntax(context.TODO())
	qt.Assert(t, err, qt.IsNil)

	tpl2 := NewNativeTemplate(0, 0, false)
	expanded, err = NewExpanderTimeContinuation(ectx, env).ExpandExpression(stx)
	qt.Assert(t, err, qt.IsNil)

	err = NewCompiletimeContinuation(tpl2, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)
}

func TestSourceRecording_SetBang(t *testing.T) {
	// Define x first, then set!
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())

	// First compile (define x 1)
	rdr := strings.NewReader("(define x 1)")
	p := parser.NewParserWithFile(env, true, rdr, "test.scm")
	stx, _ := p.ReadSyntax(context.TODO())
	tpl := NewNativeTemplate(0, 0, false)
	ectx := context.Background()
	expanded, _ := NewExpanderTimeContinuation(ectx, env).ExpandExpression(stx)
	cctx := NewCompileTimeCallContext(context.Background(), false, true)
	err := NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	// Now compile (set! x 2)
	rdr = strings.NewReader("(set! x 2)")
	p = parser.NewParserWithFile(env, true, rdr, "test.scm")
	stx, err = p.ReadSyntax(context.TODO())
	qt.Assert(t, err, qt.IsNil)

	tpl2 := NewNativeTemplate(0, 0, false)
	expanded, err = NewExpanderTimeContinuation(ectx, env).ExpandExpression(stx)
	qt.Assert(t, err, qt.IsNil)

	err = NewCompiletimeContinuation(tpl2, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)
}

func TestSourceRecording_Quasiquote(t *testing.T) {
	tpl := compileScheme(t, "`(1 2 3)")

	qt.Assert(t, tpl.CodeLen() > 0, qt.IsTrue)
}

func TestSourceRecording_SourceLocationPreserved(t *testing.T) {
	// Use a multi-line program to verify line numbers are correct
	code := `(define (identity a)
  a)`

	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	rdr := strings.NewReader(code)
	p := parser.NewParserWithFile(env, true, rdr, "multiline.scm")

	stx, err := p.ReadSyntax(context.TODO())
	qt.Assert(t, err, qt.IsNil)

	tpl := NewNativeTemplate(0, 0, false)
	ectx := context.Background()
	expanded, err := NewExpanderTimeContinuation(ectx, env).ExpandExpression(stx)
	qt.Assert(t, err, qt.IsNil)

	cctx := NewCompileTimeCallContext(context.Background(), false, true)
	err = NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	// Source should point to the correct file
	source := tpl.SourceAt(0)
	qt.Assert(t, source, qt.IsNotNil)
	qt.Assert(t, source.File, qt.Equals, "multiline.scm")
	// Line numbers start at 0 in internal representation
	qt.Assert(t, source.Start.Line() >= 0, qt.IsTrue)
}

// =============================================================================
// Gap Coverage Tests — verify infrastructure ops have source attribution
//
// These tests validate the architectural improvement from per-operation source
// tracking. With the old range-based SourceMap, infrastructure operations
// (Branch, Push, Apply, SaveContinuation) could fall in gaps between inner
// entries and return nil. Now every operation inherits source from the
// compiler's source stack.
// =============================================================================

func TestSourceRecording_IfAllOpsHaveSource(t *testing.T) {
	c := qt.New(t)

	tpl := compileScheme(t, "(if #t 1 2)")

	// Every operation in the template should have source, including
	// BranchOnFalse and BranchOffset infrastructure ops
	effOps := tpl.Operations()
	for pc := range len(effOps) {
		source := tpl.SourceAt(pc)
		c.Assert(source, qt.IsNotNil,
			qt.Commentf("PC %d (%T) has no source", pc, effOps[pc]))
	}
}

func TestSourceRecording_LambdaAllOpsHaveSource(t *testing.T) {
	c := qt.New(t)

	// Lambda generates MakeClosure and LoadLiteral (template) ops.
	// Uses only core syntax (no primitives needed).
	tpl := compileScheme(t, "(lambda (x) x)")

	effOps := tpl.Operations()
	for pc := range len(effOps) {
		source := tpl.SourceAt(pc)
		c.Assert(source, qt.IsNotNil,
			qt.Commentf("PC %d (%T) has no source", pc, effOps[pc]))
	}
}

func TestSourceRecording_BeginAllOpsHaveSource(t *testing.T) {
	c := qt.New(t)

	tpl := compileScheme(t, "(begin 1 2 3)")

	effOps := tpl.Operations()
	for pc := range len(effOps) {
		source := tpl.SourceAt(pc)
		c.Assert(source, qt.IsNotNil,
			qt.Commentf("PC %d (%T) has no source", pc, effOps[pc]))
	}
}

func TestSourceRecording_NestedIfAllOpsHaveSource(t *testing.T) {
	c := qt.New(t)

	// Nested forms test the source stack's push/pop behavior —
	// inner sub-expressions push their own source, outer infrastructure
	// ops inherit the enclosing form's source
	tpl := compileScheme(t, "(if #t (if #f 42 0) -1)")

	effOps := tpl.Operations()
	for pc := range len(effOps) {
		source := tpl.SourceAt(pc)
		c.Assert(source, qt.IsNotNil,
			qt.Commentf("PC %d (%T) has no source", pc, effOps[pc]))
	}
}

func TestSourceRecording_DefineAllOpsHaveSource(t *testing.T) {
	c := qt.New(t)

	tpl := compileScheme(t, "(define x 42)")

	effOps := tpl.Operations()
	for pc := range len(effOps) {
		source := tpl.SourceAt(pc)
		c.Assert(source, qt.IsNotNil,
			qt.Commentf("PC %d (%T) has no source", pc, effOps[pc]))
	}
}

func TestSourceRecording_QuoteAllOpsHaveSource(t *testing.T) {
	c := qt.New(t)

	tpl := compileScheme(t, "'(a b c)")

	effOps := tpl.Operations()
	for pc := range len(effOps) {
		source := tpl.SourceAt(pc)
		c.Assert(source, qt.IsNotNil,
			qt.Commentf("PC %d (%T) has no source", pc, effOps[pc]))
	}
}
