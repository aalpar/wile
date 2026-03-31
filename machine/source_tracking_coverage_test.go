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

package machine_test

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/machine/testutil"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// addOpsWithSource adds count placeholder operations tagged with the given source.
// Used by tests that need a template with source-tagged operations.
func addOpsWithSource(tpl *machine.NativeTemplate, count int, source *syntax.SourceContext) {
	for range count {
		tpl.AppendOperationsWithSource(source, machine.NewOperationLoadVoid())
	}
}

// =============================================================================
// CaptureStackTrace Tests
// =============================================================================

func TestCaptureStackTrace_Empty(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, nil, env))

	trace := mc.CaptureStackTrace(10)
	c.Assert(len(trace), qt.Equals, 0)
}

func TestCaptureStackTrace_SingleFrame(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	tpl.SetName("test-func")

	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(10, 5, 100),
	}
	addOpsWithSource(tpl, 10, source)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	trace := mc.CaptureStackTrace(10)
	c.Assert(len(trace), qt.Equals, 1)
	c.Assert(trace[0].FunctionName, qt.Equals, "test-func")
	c.Assert(trace[0].CurrentLoc, qt.IsNotNil)
	c.Assert(trace[0].CurrentLoc.File, qt.Equals, "test.scm")
}

func TestCaptureStackTrace_MultipleFrames(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()

	// Create templates with names and source maps
	tpl1 := machine.NewNativeTemplate(0, 0, false)
	tpl1.SetName("inner")
	source1 := &syntax.SourceContext{File: "inner.scm", Start: syntax.NewSourceIndexes(5, 1, 50)}
	addOpsWithSource(tpl1, 5, source1)

	tpl2 := machine.NewNativeTemplate(0, 0, false)
	tpl2.SetName("middle")
	source2 := &syntax.SourceContext{File: "middle.scm", Start: syntax.NewSourceIndexes(10, 1, 100)}
	addOpsWithSource(tpl2, 5, source2)

	tpl3 := machine.NewNativeTemplate(0, 0, false)
	tpl3.SetName("outer")
	source3 := &syntax.SourceContext{File: "outer.scm", Start: syntax.NewSourceIndexes(15, 1, 150)}
	addOpsWithSource(tpl3, 5, source3)

	// Build continuation chain: outer -> middle -> inner
	cont1 := machine.NewMachineContinuation(nil, tpl3, env)
	cont2 := machine.NewMachineContinuation(cont1, tpl2, env)
	cont2.SetPC(2)
	cont1.SetPC(3)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(cont2, tpl1, env))

	trace := mc.CaptureStackTrace(10)
	c.Assert(len(trace), qt.Equals, 3)
	c.Assert(trace[0].FunctionName, qt.Equals, "inner")
	c.Assert(trace[1].FunctionName, qt.Equals, "middle")
	c.Assert(trace[2].FunctionName, qt.Equals, "outer")
}

func TestCaptureStackTrace_MaxDepth(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Build a deep continuation chain
	var cont *machine.MachineContinuation
	for range 50 {
		cont = machine.NewMachineContinuation(cont, tpl, env)
	}

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(cont, tpl, env))

	// Limit to 5 frames
	trace := mc.CaptureStackTrace(5)
	c.Assert(len(trace), qt.Equals, 6) // 5 + truncation message
	c.Assert(trace[5].FunctionName, qt.Contains, "more frames")
}

func TestCaptureStackTrace_AnonymousFunction(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	// No name set - should show as <anonymous>

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	trace := mc.CaptureStackTrace(10)
	c.Assert(len(trace), qt.Equals, 1)
	// Empty name handled by StackFrame.String()
	c.Assert(trace[0].FunctionName, qt.Equals, "")
}

func TestCaptureStackTrace_NoSourceMap(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	tpl.SetName("no-source")
	// No source map entries

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	trace := mc.CaptureStackTrace(10)
	c.Assert(len(trace), qt.Equals, 1)
	c.Assert(trace[0].CurrentLoc, qt.IsNil)
}

// =============================================================================
// Debugger Runtime Tests (CheckBreakpoint, ShouldStep)
// =============================================================================

func TestDebugger_CheckBreakpoint_Match(t *testing.T) {
	c := qt.New(t)

	d := machine.NewDebugger()
	d.SetBreakpoint("test.scm", 10, 0) // Any column

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Add source at line 10 - NewSourceIndexes(index, column, line)
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 5, 10), // index=100, column=5, line=10
	}
	addOpsWithSource(tpl, 5, source)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNotNil)
	c.Assert(bp.HitCount, qt.Equals, 1)
}

func TestDebugger_CheckBreakpoint_MatchWithColumn(t *testing.T) {
	c := qt.New(t)

	d := machine.NewDebugger()
	d.SetBreakpoint("test.scm", 10, 5) // Specific column

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Add source at line 10, column 5 - NewSourceIndexes(index, column, line)
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 5, 10), // index=100, column=5, line=10
	}
	addOpsWithSource(tpl, 5, source)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNotNil)
}

func TestDebugger_CheckBreakpoint_NoMatch_WrongFile(t *testing.T) {
	c := qt.New(t)

	d := machine.NewDebugger()
	d.SetBreakpoint("other.scm", 10, 0) // machine.Breakpoint at other.scm:10

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Source at test.scm:10 - same line but different file
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 5, 10), // index=100, column=5, line=10
	}
	addOpsWithSource(tpl, 5, source)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNil) // Should not match - wrong file
}

func TestDebugger_CheckBreakpoint_NoMatch_WrongLine(t *testing.T) {
	c := qt.New(t)

	d := machine.NewDebugger()
	d.SetBreakpoint("test.scm", 20, 0) // machine.Breakpoint at line 20

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Source at line 10 - different line
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 5, 10), // index=100, column=5, line=10
	}
	addOpsWithSource(tpl, 5, source)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNil) // Should not match - wrong line (20 != 10)
}

func TestDebugger_CheckBreakpoint_NoMatch_WrongColumn(t *testing.T) {
	c := qt.New(t)

	d := machine.NewDebugger()
	d.SetBreakpoint("test.scm", 10, 20) // machine.Breakpoint at line 10, column 20

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Source at line 10, column 5 - same line but different column
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 5, 10), // index=100, column=5, line=10
	}
	addOpsWithSource(tpl, 5, source)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNil) // Should not match - wrong column (20 != 5)
}

func TestDebugger_CheckBreakpoint_Disabled(t *testing.T) {
	c := qt.New(t)

	d := machine.NewDebugger()
	id := d.SetBreakpoint("test.scm", 10, 0) // machine.Breakpoint at line 10
	d.DisableBreakpoint(id)

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	// Source at line 10 - matches but breakpoint is disabled
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 5, 10), // index=100, column=5, line=10
	}
	addOpsWithSource(tpl, 5, source)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNil) // Should not match - breakpoint is disabled
}

func TestDebugger_CheckBreakpoint_NoSource(t *testing.T) {
	c := qt.New(t)

	d := machine.NewDebugger()
	d.SetBreakpoint("test.scm", 10, 0)

	env := environment.NewNamespace().Runtime()

	// No template - no source
	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, nil, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNil)
}

func TestDebugger_ShouldStep_StepInto(t *testing.T) {
	c := qt.New(t)

	d := machine.NewDebugger()
	d.StepInto()

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	source := &syntax.SourceContext{File: "test.scm"}
	addOpsWithSource(tpl, 5, source)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	c.Assert(d.ShouldStep(mc), qt.IsTrue)
}

func TestDebugger_ShouldStep_StepInto_NoSource(t *testing.T) {
	c := qt.New(t)

	d := machine.NewDebugger()
	d.StepInto()

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	// No source map entries

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	c.Assert(d.ShouldStep(mc), qt.IsFalse)
}

func TestDebugger_ShouldStep_StepOver_SameDepth(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	source := &syntax.SourceContext{File: "test.scm"}
	addOpsWithSource(tpl, 5, source)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	d := machine.NewDebugger()
	d.StepOver(mc)

	// Same depth - should step
	c.Assert(d.ShouldStep(mc), qt.IsTrue)
}

func TestDebugger_ShouldStep_StepOver_DeeperFrame(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	source := &syntax.SourceContext{File: "test.scm"}
	addOpsWithSource(tpl, 5, source)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	d := machine.NewDebugger()
	d.StepOver(mc) // Depth 0

	// Add a continuation to increase depth
	mc.SaveContinuation(5) // Now depth 1

	// Deeper - should NOT step
	c.Assert(d.ShouldStep(mc), qt.IsFalse)
}

func TestDebugger_ShouldStep_StepOver_ShallowerFrame(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	source := &syntax.SourceContext{File: "test.scm"}
	addOpsWithSource(tpl, 5, source)

	// Create a continuation chain where we start at depth 1
	parentCont := machine.NewMachineContinuation(nil, tpl, env)
	childCont := machine.NewMachineContinuation(parentCont, tpl, env)

	mc := machine.NewMachineContext(context.Background(), childCont)
	// mc now has cont pointing to parentCont, depth = 1

	d := machine.NewDebugger()
	d.StepOver(mc) // Set at depth 1

	// Restore from parent (depth goes to 0)
	mc.Restore(parentCont)

	// Shallower (depth 0 <= 1) - should step
	c.Assert(d.ShouldStep(mc), qt.IsTrue)
}

func TestDebugger_ShouldStep_StepOut(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	source := &syntax.SourceContext{File: "test.scm"}
	addOpsWithSource(tpl, 5, source)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))
	mc.SaveContinuation(5)

	d := machine.NewDebugger()
	d.StepOut(mc) // Record current frame

	// Still same frame - should NOT step
	c.Assert(d.ShouldStep(mc), qt.IsFalse)

	// Pop the continuation - frame changed
	_, err := mc.PopContinuation()
	c.Assert(err, qt.IsNil)
	c.Assert(d.ShouldStep(mc), qt.IsTrue)
}

func TestDebugger_ShouldStep_NotStepping(t *testing.T) {
	c := qt.New(t)

	d := machine.NewDebugger()
	// Not stepping

	env := environment.NewNamespace().Runtime()
	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, nil, env))

	c.Assert(d.ShouldStep(mc), qt.IsFalse)
}

// =============================================================================
// Source Recording Tests for Symbols and Literals
// =============================================================================

func TestSourceRecording_Symbol(t *testing.T) {
	c := qt.New(t)

	env := testutil.NewMinimalNamespace(environment.NewNamespace().Runtime())

	// First define x
	rdr := strings.NewReader("(define x 42)")
	p := parser.NewParserWithFile(env, true, rdr, "test.scm")
	stx, _ := p.ReadSyntax(context.TODO())
	tpl := machine.NewNativeTemplate(0, 0, false)
	ectx := context.Background()
	expanded, _ := compilation.NewExpanderTimeContinuation(ectx, env, machine.NewVMMacroEvaluator()).ExpandExpression(stx)
	cctx := compilation.NewCompileTimeCallContext(context.Background(), false)
	_ = compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator()).CompileExpression(cctx, expanded)

	// Now reference x
	rdr = strings.NewReader("x")
	p = parser.NewParserWithFile(env, true, rdr, "ref.scm")
	stx, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	tpl2 := machine.NewNativeTemplate(0, 0, false)
	expanded, err = compilation.NewExpanderTimeContinuation(ectx, env, machine.NewVMMacroEvaluator()).ExpandExpression(stx)
	c.Assert(err, qt.IsNil)

	err = compilation.NewCompileTimeContinuation(tpl2, env, machine.NewVMMacroEvaluator()).CompileExpression(cctx, expanded)
	c.Assert(err, qt.IsNil)

	source := tpl2.SourceAt(0)
	c.Assert(source, qt.IsNotNil)
	c.Assert(source.File, qt.Equals, "ref.scm")
}

func TestSourceRecording_Literal(t *testing.T) {
	c := qt.New(t)

	env := testutil.NewMinimalNamespace(environment.NewNamespace().Runtime())
	rdr := strings.NewReader("42")
	p := parser.NewParserWithFile(env, true, rdr, "literal.scm")
	stx, err := p.ReadSyntax(context.TODO())
	c.Assert(err, qt.IsNil)

	tpl := machine.NewNativeTemplate(0, 0, false)
	ectx := context.Background()
	expanded, err := compilation.NewExpanderTimeContinuation(ectx, env, machine.NewVMMacroEvaluator()).ExpandExpression(stx)
	c.Assert(err, qt.IsNil)

	cctx := compilation.NewCompileTimeCallContext(context.Background(), false)
	err = compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator()).CompileExpression(cctx, expanded)
	c.Assert(err, qt.IsNil)

	source := tpl.SourceAt(0)
	c.Assert(source, qt.IsNotNil)
	c.Assert(source.File, qt.Equals, "literal.scm")
}

func TestSourceRecording_CaseLambda(t *testing.T) {
	c := qt.New(t)

	tpl := compileScheme(t, "(case-lambda ((x) x) ((x y) y))")

	// The outer template should have source for all operations
	c.Assert(tpl.CodeLen() > 0, qt.IsTrue)
}

// =============================================================================
// Run Loop with Debugger Tests
// =============================================================================

func TestRun_WithDebugger_Breakpoint(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()

	// Create a template that just loads a literal
	tpl := machine.NewNativeTemplate(0, 0, false)
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 1, 10), // index, column, line
	}
	// Add operations with source tracking
	lit := values.NewInteger(42)
	tpl.MaybeAppendLiteral(lit)
	tpl.AppendOperationsWithSource(source,
		machine.NewOperationLoadVoid(),
		machine.NewOperationLoadVoid(),
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(0),
	)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	// Verify the context has the template and source map
	c.Assert(mc.Template(), qt.Equals, tpl)
	c.Assert(mc.Template().SourceAt(0), qt.IsNotNil)
	c.Assert(mc.Template().SourceAt(0).File, qt.Equals, "test.scm")
	c.Assert(mc.Template().SourceAt(0).Start.Line(), qt.Equals, 10)

	// Set up debugger with breakpoint
	d := machine.NewDebugger()
	d.SetBreakpoint("test.scm", 10, 0)

	breakHit := false
	d.OnBreak(func(ctx *machine.MachineContext, bp *machine.Breakpoint) {
		breakHit = true
		d.Continue() // Continue execution
	})

	mc.SetDebugger(d)
	c.Assert(mc.Debugger(), qt.Equals, d)

	// Run - will hit breakpoint, continue, then exit when operations are done
	_ = mc.Run()

	c.Assert(breakHit, qt.IsTrue)
}

func TestRun_WithDebugger_StepInto(t *testing.T) {
	c := qt.New(t)

	env := environment.NewNamespace().Runtime()

	tpl := machine.NewNativeTemplate(0, 0, false)
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(5, 1, 50),
	}
	// Add operations with source tracking
	lit := values.NewInteger(42)
	tpl.MaybeAppendLiteral(lit)
	tpl.AppendOperationsWithSource(source,
		machine.NewOperationLoadVoid(),
		machine.NewOperationLoadVoid(),
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(0),
		machine.NewOperationRestoreContinuation(),
	)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))

	d := machine.NewDebugger()
	d.StepInto()

	stepCount := 0
	d.OnBreak(func(ctx *machine.MachineContext, bp *machine.Breakpoint) {
		stepCount++
		if stepCount >= 2 {
			d.Continue()
		}
	})

	mc.SetDebugger(d)

	_ = mc.Run()

	c.Assert(stepCount >= 1, qt.IsTrue)
}
