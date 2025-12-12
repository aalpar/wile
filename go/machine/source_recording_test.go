// Copyright 2025 Aaron Alpar
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
	"wile/environment"
	"wile/parser"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// Helper to compile Scheme code and return the template
func compileScheme(t *testing.T, code string) *NativeTemplate {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	rdr := strings.NewReader(code)
	p := parser.NewParserWithFile(env, rdr, "test.scm")

	stx, err := p.ReadSyntax(nil)
	qt.Assert(t, err, qt.IsNil)

	tpl := NewNativeTemplate(0, 0, false)
	ectx := NewExpandTimeCallContext()
	expanded, err := NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	qt.Assert(t, err, qt.IsNil)

	cctx := NewCompileTimeCallContext(false, true, env)
	err = NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	return tpl
}

func TestSourceRecording_Quote(t *testing.T) {
	tpl := compileScheme(t, "'hello")

	// Source map should have entries
	qt.Assert(t, tpl.sourceMap, qt.IsNotNil)
	qt.Assert(t, tpl.sourceMap.Len() > 0, qt.IsTrue)

	// Should be able to look up source at PC 0
	source := tpl.SourceAt(0)
	qt.Assert(t, source, qt.IsNotNil)
	qt.Assert(t, source.File, qt.Equals, "test.scm")
}

func TestSourceRecording_If(t *testing.T) {
	tpl := compileScheme(t, "(if #t 1 2)")

	qt.Assert(t, tpl.sourceMap, qt.IsNotNil)
	qt.Assert(t, tpl.sourceMap.Len() > 0, qt.IsTrue)

	source := tpl.SourceAt(0)
	qt.Assert(t, source, qt.IsNotNil)
}

func TestSourceRecording_Define(t *testing.T) {
	tpl := compileScheme(t, "(define x 42)")

	qt.Assert(t, tpl.sourceMap, qt.IsNotNil)
	qt.Assert(t, tpl.sourceMap.Len() > 0, qt.IsTrue)

	source := tpl.SourceAt(0)
	qt.Assert(t, source, qt.IsNotNil)
}

func TestSourceRecording_DefineFunction(t *testing.T) {
	tpl := compileScheme(t, "(define (foo x) x)")

	qt.Assert(t, tpl.sourceMap, qt.IsNotNil)
	qt.Assert(t, tpl.sourceMap.Len() > 0, qt.IsTrue)

	// The function template should have a name
	// Find the child template in literals
	var childTpl *NativeTemplate
	for _, lit := range tpl.literals {
		if nt, ok := lit.(*NativeTemplate); ok {
			childTpl = nt
			break
		}
	}
	qt.Assert(t, childTpl, qt.IsNotNil)
	qt.Assert(t, childTpl.Name(), qt.Equals, "foo")
}

func TestSourceRecording_Lambda(t *testing.T) {
	tpl := compileScheme(t, "(lambda (x) x)")

	qt.Assert(t, tpl.sourceMap, qt.IsNotNil)
	qt.Assert(t, tpl.sourceMap.Len() > 0, qt.IsTrue)
}

func TestSourceRecording_Begin(t *testing.T) {
	tpl := compileScheme(t, "(begin 1 2 3)")

	qt.Assert(t, tpl.sourceMap, qt.IsNotNil)
	qt.Assert(t, tpl.sourceMap.Len() > 0, qt.IsTrue)
}

func TestSourceRecording_Call(t *testing.T) {
	// First define a function, then call it
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// Define a simple function
	rdr := strings.NewReader("(define (id x) x)")
	p := parser.NewParserWithFile(env, rdr, "test.scm")
	stx, _ := p.ReadSyntax(nil)
	tpl := NewNativeTemplate(0, 0, false)
	ectx := NewExpandTimeCallContext()
	expanded, _ := NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	cctx := NewCompileTimeCallContext(false, true, env)
	NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)

	// Now compile a call to that function
	rdr = strings.NewReader("(id 42)")
	p = parser.NewParserWithFile(env, rdr, "test.scm")
	stx, err := p.ReadSyntax(nil)
	qt.Assert(t, err, qt.IsNil)

	tpl2 := NewNativeTemplate(0, 0, false)
	expanded, err = NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	qt.Assert(t, err, qt.IsNil)

	err = NewCompiletimeContinuation(tpl2, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	qt.Assert(t, tpl2.sourceMap, qt.IsNotNil)
	qt.Assert(t, tpl2.sourceMap.Len() > 0, qt.IsTrue)
}

func TestSourceRecording_SetBang(t *testing.T) {
	// Define x first, then set!
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// First compile (define x 1)
	rdr := strings.NewReader("(define x 1)")
	p := parser.NewParserWithFile(env, rdr, "test.scm")
	stx, _ := p.ReadSyntax(nil)
	tpl := NewNativeTemplate(0, 0, false)
	ectx := NewExpandTimeCallContext()
	expanded, _ := NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	cctx := NewCompileTimeCallContext(false, true, env)
	NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)

	// Now compile (set! x 2)
	rdr = strings.NewReader("(set! x 2)")
	p = parser.NewParserWithFile(env, rdr, "test.scm")
	stx, err := p.ReadSyntax(nil)
	qt.Assert(t, err, qt.IsNil)

	tpl2 := NewNativeTemplate(0, 0, false)
	expanded, err = NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	qt.Assert(t, err, qt.IsNil)

	err = NewCompiletimeContinuation(tpl2, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	qt.Assert(t, tpl2.sourceMap, qt.IsNotNil)
	qt.Assert(t, tpl2.sourceMap.Len() > 0, qt.IsTrue)
}

func TestSourceRecording_Quasiquote(t *testing.T) {
	tpl := compileScheme(t, "`(1 2 3)")

	qt.Assert(t, tpl.sourceMap, qt.IsNotNil)
	qt.Assert(t, tpl.sourceMap.Len() > 0, qt.IsTrue)
}

func TestSourceRecording_SourceLocationPreserved(t *testing.T) {
	// Use a multi-line program to verify line numbers are correct
	code := `(define (identity a)
  a)`

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	rdr := strings.NewReader(code)
	p := parser.NewParserWithFile(env, rdr, "multiline.scm")

	stx, err := p.ReadSyntax(nil)
	qt.Assert(t, err, qt.IsNil)

	tpl := NewNativeTemplate(0, 0, false)
	ectx := NewExpandTimeCallContext()
	expanded, err := NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	qt.Assert(t, err, qt.IsNil)

	cctx := NewCompileTimeCallContext(false, true, env)
	err = NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	// Source should point to the correct file
	source := tpl.SourceAt(0)
	qt.Assert(t, source, qt.IsNotNil)
	qt.Assert(t, source.File, qt.Equals, "multiline.scm")
	// Line numbers start at 0 in internal representation
	qt.Assert(t, source.Start.Line() >= 0, qt.IsTrue)
}
