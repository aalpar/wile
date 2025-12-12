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
	"context"
	"wile/environment"
	"wile/parser"
	"wile/syntax"
	"wile/values"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// =============================================================================
// CaptureStackTrace Tests
// =============================================================================

func TestCaptureStackTrace_Empty(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	mc := NewMachineContext(NewMachineContinuation(nil, nil, env))

	trace := mc.CaptureStackTrace(10)
	c.Assert(len(trace), qt.Equals, 0)
}

func TestCaptureStackTrace_SingleFrame(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.SetName("test-func")

	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(10, 5, 100),
	}
	tpl.sourceMap.Add(0, 10, source)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	trace := mc.CaptureStackTrace(10)
	c.Assert(len(trace), qt.Equals, 1)
	c.Assert(trace[0].FunctionName, qt.Equals, "test-func")
	c.Assert(trace[0].CurrentLoc, qt.IsNotNil)
	c.Assert(trace[0].CurrentLoc.File, qt.Equals, "test.scm")
}

func TestCaptureStackTrace_MultipleFrames(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)

	// Create templates with names and source maps
	tpl1 := NewNativeTemplate(0, 0, false)
	tpl1.SetName("inner")
	source1 := &syntax.SourceContext{File: "inner.scm", Start: syntax.NewSourceIndexes(5, 1, 50)}
	tpl1.sourceMap.Add(0, 5, source1)

	tpl2 := NewNativeTemplate(0, 0, false)
	tpl2.SetName("middle")
	source2 := &syntax.SourceContext{File: "middle.scm", Start: syntax.NewSourceIndexes(10, 1, 100)}
	tpl2.sourceMap.Add(0, 5, source2)

	tpl3 := NewNativeTemplate(0, 0, false)
	tpl3.SetName("outer")
	source3 := &syntax.SourceContext{File: "outer.scm", Start: syntax.NewSourceIndexes(15, 1, 150)}
	tpl3.sourceMap.Add(0, 5, source3)

	// Build continuation chain: outer -> middle -> inner
	cont1 := NewMachineContinuation(nil, tpl3, env)
	cont2 := NewMachineContinuation(cont1, tpl2, env)
	cont2.pc = 2
	cont1.pc = 3

	mc := NewMachineContext(NewMachineContinuation(cont2, tpl1, env))

	trace := mc.CaptureStackTrace(10)
	c.Assert(len(trace), qt.Equals, 3)
	c.Assert(trace[0].FunctionName, qt.Equals, "inner")
	c.Assert(trace[1].FunctionName, qt.Equals, "middle")
	c.Assert(trace[2].FunctionName, qt.Equals, "outer")
}

func TestCaptureStackTrace_MaxDepth(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)

	// Build a deep continuation chain
	var cont *MachineContinuation
	for i := 0; i < 50; i++ {
		cont = NewMachineContinuation(cont, tpl, env)
	}

	mc := NewMachineContext(NewMachineContinuation(cont, tpl, env))

	// Limit to 5 frames
	trace := mc.CaptureStackTrace(5)
	c.Assert(len(trace), qt.Equals, 6) // 5 + truncation message
	c.Assert(trace[5].FunctionName, qt.Contains, "more frames")
}

func TestCaptureStackTrace_AnonymousFunction(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	// No name set - should show as <anonymous>

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	trace := mc.CaptureStackTrace(10)
	c.Assert(len(trace), qt.Equals, 1)
	// Empty name handled by StackFrame.String()
	c.Assert(trace[0].FunctionName, qt.Equals, "")
}

func TestCaptureStackTrace_NoSourceMap(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.SetName("no-source")
	// No source map entries

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	trace := mc.CaptureStackTrace(10)
	c.Assert(len(trace), qt.Equals, 1)
	c.Assert(trace[0].CurrentLoc, qt.IsNil)
}

// =============================================================================
// Debugger Runtime Tests (CheckBreakpoint, ShouldStep)
// =============================================================================

func TestDebugger_CheckBreakpoint_Match(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	d.SetBreakpoint("test.scm", 10, 0) // Any column

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)

	// Add source at line 10 - NewSourceIndexes(index, column, line)
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 5, 10), // index=100, column=5, line=10
	}
	tpl.sourceMap.Add(0, 5, source)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNotNil)
	c.Assert(bp.HitCount, qt.Equals, 1)
}

func TestDebugger_CheckBreakpoint_MatchWithColumn(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	d.SetBreakpoint("test.scm", 10, 5) // Specific column

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)

	// Add source at line 10, column 5 - NewSourceIndexes(index, column, line)
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 5, 10), // index=100, column=5, line=10
	}
	tpl.sourceMap.Add(0, 5, source)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNotNil)
}

func TestDebugger_CheckBreakpoint_NoMatch_WrongFile(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	d.SetBreakpoint("other.scm", 10, 0) // Breakpoint at other.scm:10

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)

	// Source at test.scm:10 - same line but different file
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 5, 10), // index=100, column=5, line=10
	}
	tpl.sourceMap.Add(0, 5, source)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNil) // Should not match - wrong file
}

func TestDebugger_CheckBreakpoint_NoMatch_WrongLine(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	d.SetBreakpoint("test.scm", 20, 0) // Breakpoint at line 20

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)

	// Source at line 10 - different line
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 5, 10), // index=100, column=5, line=10
	}
	tpl.sourceMap.Add(0, 5, source)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNil) // Should not match - wrong line (20 != 10)
}

func TestDebugger_CheckBreakpoint_NoMatch_WrongColumn(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	d.SetBreakpoint("test.scm", 10, 20) // Breakpoint at line 10, column 20

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)

	// Source at line 10, column 5 - same line but different column
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 5, 10), // index=100, column=5, line=10
	}
	tpl.sourceMap.Add(0, 5, source)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNil) // Should not match - wrong column (20 != 5)
}

func TestDebugger_CheckBreakpoint_Disabled(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	id := d.SetBreakpoint("test.scm", 10, 0) // Breakpoint at line 10
	d.DisableBreakpoint(id)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)

	// Source at line 10 - matches but breakpoint is disabled
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 5, 10), // index=100, column=5, line=10
	}
	tpl.sourceMap.Add(0, 5, source)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNil) // Should not match - breakpoint is disabled
}

func TestDebugger_CheckBreakpoint_NoSource(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	d.SetBreakpoint("test.scm", 10, 0)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)

	// No template - no source
	mc := NewMachineContext(NewMachineContinuation(nil, nil, env))

	bp := d.CheckBreakpoint(mc)
	c.Assert(bp, qt.IsNil)
}

func TestDebugger_ShouldStep_StepInto(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	d.StepInto()

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)

	source := &syntax.SourceContext{File: "test.scm"}
	tpl.sourceMap.Add(0, 5, source)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	c.Assert(d.ShouldStep(mc), qt.IsTrue)
}

func TestDebugger_ShouldStep_StepInto_NoSource(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	d.StepInto()

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	// No source map entries

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	c.Assert(d.ShouldStep(mc), qt.IsFalse)
}

func TestDebugger_ShouldStep_StepOver_SameDepth(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	source := &syntax.SourceContext{File: "test.scm"}
	tpl.sourceMap.Add(0, 5, source)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	d := NewDebugger()
	d.StepOver(mc)

	// Same depth - should step
	c.Assert(d.ShouldStep(mc), qt.IsTrue)
}

func TestDebugger_ShouldStep_StepOver_DeeperFrame(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	source := &syntax.SourceContext{File: "test.scm"}
	tpl.sourceMap.Add(0, 5, source)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	d := NewDebugger()
	d.StepOver(mc) // Depth 0

	// Add a continuation to increase depth
	mc.SaveContinuation(5) // Now depth 1

	// Deeper - should NOT step
	c.Assert(d.ShouldStep(mc), qt.IsFalse)
}

func TestDebugger_ShouldStep_StepOver_ShallowerFrame(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	source := &syntax.SourceContext{File: "test.scm"}
	tpl.sourceMap.Add(0, 5, source)

	// Create a continuation chain where we start at depth 1
	parentCont := NewMachineContinuation(nil, tpl, env)
	childCont := NewMachineContinuation(parentCont, tpl, env)

	mc := NewMachineContext(childCont)
	// mc now has cont pointing to parentCont, depth = 1

	d := NewDebugger()
	d.StepOver(mc) // Set at depth 1

	// Restore from parent (depth goes to 0)
	mc.Restore(parentCont)

	// Shallower (depth 0 <= 1) - should step
	c.Assert(d.ShouldStep(mc), qt.IsTrue)
}

func TestDebugger_ShouldStep_StepOut(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	source := &syntax.SourceContext{File: "test.scm"}
	tpl.sourceMap.Add(0, 5, source)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))
	mc.SaveContinuation(5)

	d := NewDebugger()
	d.StepOut(mc) // Record current frame

	// Still same frame - should NOT step
	c.Assert(d.ShouldStep(mc), qt.IsFalse)

	// Pop the continuation - frame changed
	mc.PopContinuation()
	c.Assert(d.ShouldStep(mc), qt.IsTrue)
}

func TestDebugger_ShouldStep_NotStepping(t *testing.T) {
	c := qt.New(t)

	d := NewDebugger()
	// Not stepping

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	mc := NewMachineContext(NewMachineContinuation(nil, nil, env))

	c.Assert(d.ShouldStep(mc), qt.IsFalse)
}

// =============================================================================
// Runtime Error Tests (mc.Error, mc.WrapError in operations)
// =============================================================================

func TestOperationApply_ErrorWithStackTrace(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.SetName("caller")
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(10, 1, 100),
	}
	tpl.sourceMap.Add(0, 5, source)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))
	mc.SetValue(values.NewInteger(42)) // Not a closure!

	op := NewOperationApply()
	_, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNotNil)
	schemeErr, ok := err.(*SchemeError)
	c.Assert(ok, qt.IsTrue)
	c.Assert(schemeErr.Message, qt.Contains, "expected a closure")
	c.Assert(schemeErr.Source, qt.IsNotNil)
	c.Assert(schemeErr.Source.File, qt.Equals, "test.scm")
}

func TestOperationLoadGlobal_ErrorWithStackTrace(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.SetName("test-func")
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(5, 1, 50),
	}
	tpl.sourceMap.Add(0, 5, source)

	// Add a nil literal
	tpl.literals = append(tpl.literals, nil)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	op := NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(0)
	_, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNotNil)
	schemeErr, ok := err.(*SchemeError)
	c.Assert(ok, qt.IsTrue)
	c.Assert(schemeErr.Message, qt.Contains, "does not exist")
	c.Assert(schemeErr.Source, qt.IsNotNil)
}

func TestOperationLoadLocal_ErrorWithStackTrace(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrame(lenv, genv)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.SetName("test-func")
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(5, 1, 50),
	}
	tpl.sourceMap.Add(0, 5, source)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	// Try to load from non-existent local index (depth 10 doesn't exist)
	li := environment.NewLocalIndex(0, 10)
	op := NewOperationLoadLocalByLocalIndexImmediate(li)
	_, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNotNil)
	schemeErr, ok := err.(*SchemeError)
	c.Assert(ok, qt.IsTrue)
	c.Assert(schemeErr.Message, qt.Contains, "no such local binding")
}

// =============================================================================
// Source Recording Tests for Symbols and Literals
// =============================================================================

func TestSourceRecording_Symbol(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// First define x
	rdr := strings.NewReader("(define x 42)")
	p := parser.NewParserWithFile(env, rdr, "test.scm")
	stx, _ := p.ReadSyntax(nil)
	tpl := NewNativeTemplate(0, 0, false)
	ectx := NewExpandTimeCallContext()
	expanded, _ := NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	cctx := NewCompileTimeCallContext(false, true, env)
	_ = NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)

	// Now reference x
	rdr = strings.NewReader("x")
	p = parser.NewParserWithFile(env, rdr, "ref.scm")
	stx, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	tpl2 := NewNativeTemplate(0, 0, false)
	expanded, err = NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	c.Assert(err, qt.IsNil)

	err = NewCompiletimeContinuation(tpl2, env).CompileExpression(cctx, expanded)
	c.Assert(err, qt.IsNil)

	c.Assert(tpl2.sourceMap, qt.IsNotNil)
	c.Assert(tpl2.sourceMap.Len() > 0, qt.IsTrue)

	source := tpl2.SourceAt(0)
	c.Assert(source, qt.IsNotNil)
	c.Assert(source.File, qt.Equals, "ref.scm")
}

func TestSourceRecording_Literal(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	rdr := strings.NewReader("42")
	p := parser.NewParserWithFile(env, rdr, "literal.scm")
	stx, err := p.ReadSyntax(nil)
	c.Assert(err, qt.IsNil)

	tpl := NewNativeTemplate(0, 0, false)
	ectx := NewExpandTimeCallContext()
	expanded, err := NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	c.Assert(err, qt.IsNil)

	cctx := NewCompileTimeCallContext(false, true, env)
	err = NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	c.Assert(err, qt.IsNil)

	c.Assert(tpl.sourceMap, qt.IsNotNil)
	c.Assert(tpl.sourceMap.Len() > 0, qt.IsTrue)

	source := tpl.SourceAt(0)
	c.Assert(source, qt.IsNotNil)
	c.Assert(source.File, qt.Equals, "literal.scm")
}

func TestSourceRecording_CaseLambda(t *testing.T) {
	c := qt.New(t)

	tpl := compileScheme(t, "(case-lambda ((x) x) ((x y) y))")

	c.Assert(tpl.sourceMap, qt.IsNotNil)
	c.Assert(tpl.sourceMap.Len() > 0, qt.IsTrue)
}

// =============================================================================
// Run Loop with Debugger Tests
// =============================================================================

func TestRun_WithDebugger_Breakpoint(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)

	// Create a template that just loads a literal
	tpl := NewNativeTemplate(0, 0, false)
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(100, 1, 10), // index, column, line
	}
	// Map operations 0 and 1 to the source
	tpl.sourceMap.Add(0, 2, source)

	lit := values.NewInteger(42)
	tpl.literals = append(tpl.literals, lit)
	tpl.operations = append(tpl.operations,
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
	)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	// Verify the context has the template and source map
	c.Assert(mc.Template(), qt.Equals, tpl)
	c.Assert(mc.Template().SourceAt(0), qt.IsNotNil)
	c.Assert(mc.Template().SourceAt(0).File, qt.Equals, "test.scm")
	c.Assert(mc.Template().SourceAt(0).Start.Line(), qt.Equals, 10)

	// Set up debugger with breakpoint
	d := NewDebugger()
	d.SetBreakpoint("test.scm", 10, 0)

	breakHit := false
	d.OnBreak(func(ctx *MachineContext, bp *Breakpoint) {
		breakHit = true
		d.Continue() // Continue execution
	})

	mc.SetDebugger(d)
	c.Assert(mc.Debugger(), qt.Equals, d)

	// Run - will hit breakpoint, continue, then exit when operations are done
	_ = mc.Run(context.Background())

	c.Assert(breakHit, qt.IsTrue)
}

func TestRun_WithDebugger_StepInto(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)

	tpl := NewNativeTemplate(0, 0, false)
	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(5, 1, 50),
	}
	tpl.sourceMap.Add(0, 2, source)

	lit := values.NewInteger(42)
	tpl.literals = append(tpl.literals, lit)
	tpl.operations = append(tpl.operations,
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
		NewOperationRestoreContinuation(),
	)

	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	d := NewDebugger()
	d.StepInto()

	stepCount := 0
	d.OnBreak(func(ctx *MachineContext, bp *Breakpoint) {
		stepCount++
		if stepCount >= 2 {
			d.Continue()
		}
	})

	mc.SetDebugger(d)

	_ = mc.Run(context.Background())

	c.Assert(stepCount >= 1, qt.IsTrue)
}
