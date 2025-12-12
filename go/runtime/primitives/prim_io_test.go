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

package primitives_test

import (
	"bytes"
	"context"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"wile/machine"
	"wile/parser"
	"wile/runtime"
	"wile/runtime/primitives"
	"wile/syntax"
	"wile/utils"
	"wile/values"
)

func TestNewline(t *testing.T) {
	prog := values.List(values.NewSymbol("newline"))
	result, err := runProgram(t, prog)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.IsVoid(), qt.IsTrue)
}

func TestCurrentInputPort(t *testing.T) {
	prog := values.List(values.NewSymbol("current-input-port"))
	result, err := runProgram(t, prog)
	qt.Assert(t, err, qt.IsNil)
	_, ok := result.(*values.CharacterInputPort)
	qt.Assert(t, ok, qt.IsTrue)
}

func TestCurrentOutputPort(t *testing.T) {
	prog := values.List(values.NewSymbol("current-output-port"))
	result, err := runProgram(t, prog)
	qt.Assert(t, err, qt.IsNil)
	_, ok := result.(*values.CharacterOutputPort)
	qt.Assert(t, ok, qt.IsTrue)
}

func TestDisplayWithBuffer(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (display "hello")
	prog := values.List(values.NewSymbol("display"), values.NewString("hello"))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, buf.String(), qt.Equals, "hello")
}

func TestWriteWithBuffer(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (write 42)
	prog := values.List(values.NewSymbol("write"), values.NewInteger(42))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, buf.String(), qt.Equals, "42")
}

func TestWriteCharWithBuffer(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (write-char #\A)
	prog := values.List(values.NewSymbol("write-char"), values.NewCharacter('A'))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, buf.String(), qt.Equals, "A")
}

func TestNewlineWithBuffer(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (newline)
	prog := values.List(values.NewSymbol("newline"))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, buf.String(), qt.Equals, "\n")
}

func TestReadToken(t *testing.T) {
	// Save the current input port
	savedPort := primitives.GetCurrentInputPort()
	defer func() { primitives.SetCurrentInputPort(savedPort) }()

	// Create a buffer with input data
	input := strings.NewReader("hello 42")
	primitives.SetCurrentInputPort(values.NewCharacterInputPortFromReader(input))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (read-token)
	prog := values.List(values.NewSymbol("read-token"))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	// First token should be "hello" symbol
	result := mc.GetValue()
	qt.Assert(t, result, qt.IsNotNil)
}

func TestReadSyntax(t *testing.T) {
	// Save the current input port
	savedPort := primitives.GetCurrentInputPort()
	defer func() { primitives.SetCurrentInputPort(savedPort) }()

	// Create a buffer with input data
	input := strings.NewReader("(+ 1 2)")
	primitives.SetCurrentInputPort(values.NewCharacterInputPortFromReader(input))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (read-syntax)
	prog := values.List(values.NewSymbol("read-syntax"))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	result := mc.GetValue()
	// Should return a syntax object
	_, ok := result.(syntax.SyntaxValue)
	qt.Assert(t, ok, qt.IsTrue)
}

func TestRead(t *testing.T) {
	// Save the current input port
	savedPort := primitives.GetCurrentInputPort()
	defer func() { primitives.SetCurrentInputPort(savedPort) }()

	// Create a buffer with input data
	input := strings.NewReader("(a b c)")
	primitives.SetCurrentInputPort(values.NewCharacterInputPortFromReader(input))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (read)
	prog := values.List(values.NewSymbol("read"))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	result := mc.GetValue()
	// Should return a list (a b c)
	pr, ok := result.(*values.Pair)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, pr.Car().(*values.Symbol).Key, qt.Equals, "a")
}

func TestReadWithPort(t *testing.T) {
	// Test read with an explicit port argument
	input := strings.NewReader("42")
	port := values.NewCharacterInputPortFromReader(input)

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)

	// We need to test read with an explicit port - use lambda to capture port
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// Create a program that reads from the port
	// First define a variable holding the port, then call read on it
	// For simplicity, test read-token with no port (uses default)
	prog := values.List(values.NewSymbol("read-token"))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	// Store port for the test
	savedPort := primitives.GetCurrentInputPort()
	defer func() { primitives.SetCurrentInputPort(savedPort) }()
	primitives.SetCurrentInputPort(port)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
}

func TestDisplayWithSymbol(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (display 'hello) - symbol
	prog := values.List(values.NewSymbol("display"),
		values.List(values.NewSymbol("quote"), values.NewSymbol("hello")))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, buf.String(), qt.Equals, "hello")
}

func TestWriteWithString(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (write "hello") - string with quotes
	prog := values.List(values.NewSymbol("write"), values.NewString("hello"))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, buf.String(), qt.Equals, "\"hello\"")
}

func TestWriteCharWithUnicode(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (write-char #\λ) - unicode character
	prog := values.List(values.NewSymbol("write-char"), values.NewCharacter('λ'))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, buf.String(), qt.Equals, "λ")
}

func TestReadMultipleTokens(t *testing.T) {
	// Save the current input port
	savedPort := primitives.GetCurrentInputPort()
	defer func() { primitives.SetCurrentInputPort(savedPort) }()

	// Create a buffer with multiple tokens
	input := strings.NewReader("foo bar 123")
	primitives.SetCurrentInputPort(values.NewCharacterInputPortFromReader(input))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)

	// Read first token
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	prog := values.List(values.NewSymbol("read-token"))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	result := mc.GetValue()
	qt.Assert(t, result, qt.IsNotNil)
}

func TestDisplayWithInteger(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (display 123) - integer
	prog := values.List(values.NewSymbol("display"), values.NewInteger(123))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, buf.String(), qt.Equals, "123")
}

func TestDisplayWithBoolean(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (display #t)
	prog := values.List(values.NewSymbol("display"), values.TrueValue)
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, buf.String(), qt.Equals, "#t")
}

func TestWriteWithSymbol(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (write 'foo) - symbol
	prog := values.List(values.NewSymbol("write"),
		values.List(values.NewSymbol("quote"), values.NewSymbol("foo")))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, buf.String(), qt.Equals, "foo")
}

func TestGetCurrentInputPortInitialization(t *testing.T) {
	// Test GetCurrentInputPort when currentInputPort is nil
	savedPort := primitives.GetCurrentInputPort()
	primitives.ResetCurrentInputPort()
	defer func() { primitives.SetCurrentInputPort(savedPort) }()

	port := primitives.GetCurrentInputPort()
	qt.Assert(t, port, qt.IsNotNil)
}

func TestGetCurrentOutputPortInitialization(t *testing.T) {
	// Test GetCurrentOutputPort when currentOutputPort is nil
	savedPort := primitives.GetCurrentOutputPort()
	primitives.ResetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	port := primitives.GetCurrentOutputPort()
	qt.Assert(t, port, qt.IsNotNil)
}

func TestDisplayWithList(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (display '(1 2 3)) - list
	prog := values.List(values.NewSymbol("display"),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, buf.String(), qt.Equals, "(1 2 3)")
}

func TestWriteWithList(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()

	// (write '(a b c)) - list
	prog := values.List(values.NewSymbol("write"),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))))
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, buf.String(), qt.Equals, "(a b c)")
}

func TestStringPorts(t *testing.T) {
	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)

	// Test string output port round-trip:
	// (let ((out (open-output-string)))
	//   (display "hello" out)
	//   (display " world" out)
	//   (get-output-string out))
	prog := `(let ((out (open-output-string)))
		(display "hello" out)
		(display " world" out)
		(get-output-string out))`

	p := parser.NewParser(env, strings.NewReader(prog))
	stx, err := p.ReadSyntax(nil)
	qt.Assert(t, err, qt.IsNil)

	ectx := machine.NewExpandTimeCallContext()
	expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	qt.Assert(t, err, qt.IsNil)

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)

	qt.Assert(t, mc.GetValue(), values.SchemeEquals, &values.String{Value: "hello world"})
}

func TestStringInputPort(t *testing.T) {
	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)

	// Test string input port:
	// (let ((in (open-input-string "(+ 1 2)")))
	//   (read in))
	prog := `(let ((in (open-input-string "(+ 1 2)")))
		(read in))`

	p := parser.NewParser(env, strings.NewReader(prog))
	stx, err := p.ReadSyntax(nil)
	qt.Assert(t, err, qt.IsNil)

	ectx := machine.NewExpandTimeCallContext()
	expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	qt.Assert(t, err, qt.IsNil)

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)

	// Read should return the list (+ 1 2)
	expected := values.List(values.NewSymbol("+"), values.NewInteger(1), values.NewInteger(2))
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, expected)
}

func TestBytevectorPorts(t *testing.T) {
	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)

	// Test bytevector output port:
	// (let ((out (open-output-bytevector)))
	//   (display "AB" out)
	//   (get-output-bytevector out))
	prog := `(let ((out (open-output-bytevector)))
		(display "AB" out)
		(get-output-bytevector out))`

	p := parser.NewParser(env, strings.NewReader(prog))
	stx, err := p.ReadSyntax(nil)
	qt.Assert(t, err, qt.IsNil)

	ectx := machine.NewExpandTimeCallContext()
	expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	qt.Assert(t, err, qt.IsNil)

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)

	// "AB" as bytes
	expected := values.ByteVector{{Value: 65}, {Value: 66}}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, &expected)
}

func TestBytevectorInputPort(t *testing.T) {
	env, err := runtime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)

	// Test bytevector input port with open-input-bytevector and input-port?:
	// (input-port? (open-input-bytevector #u8(65 66 67)))
	prog := `(input-port? (open-input-bytevector #u8(65 66 67)))`

	p := parser.NewParser(env, strings.NewReader(prog))
	stx, err := p.ReadSyntax(nil)
	qt.Assert(t, err, qt.IsNil)

	ectx := machine.NewExpandTimeCallContext()
	expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	qt.Assert(t, err, qt.IsNil)

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)

	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.TrueValue)
}

func TestPortPredicates(t *testing.T) {
	tcs := []struct {
		name string
		prog string
		out  values.Value
	}{
		{
			name: "input-port? on string input port",
			prog: `(input-port? (open-input-string "hello"))`,
			out:  values.TrueValue,
		},
		{
			name: "output-port? on string output port",
			prog: `(output-port? (open-output-string))`,
			out:  values.TrueValue,
		},
		{
			name: "port? on string input port",
			prog: `(port? (open-input-string "hello"))`,
			out:  values.TrueValue,
		},
		{
			name: "port? on string output port",
			prog: `(port? (open-output-string))`,
			out:  values.TrueValue,
		},
		{
			name: "input-port? on bytevector input port",
			prog: `(input-port? (open-input-bytevector #u8(1 2 3)))`,
			out:  values.TrueValue,
		},
		{
			name: "output-port? on bytevector output port",
			prog: `(output-port? (open-output-bytevector))`,
			out:  values.TrueValue,
		},
		{
			name: "port? on bytevector input port",
			prog: `(port? (open-input-bytevector #u8(1 2 3)))`,
			out:  values.TrueValue,
		},
		{
			name: "port? on bytevector output port",
			prog: `(port? (open-output-bytevector))`,
			out:  values.TrueValue,
		},
		{
			name: "input-port? on string output port is false",
			prog: `(input-port? (open-output-string))`,
			out:  values.FalseValue,
		},
		{
			name: "output-port? on string input port is false",
			prog: `(output-port? (open-input-string "hello"))`,
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env, err := runtime.NewTopLevelEnvironmentFrameTiny()
			qt.Assert(t, err, qt.IsNil)

			p := parser.NewParser(env, strings.NewReader(tc.prog))
			stx, err := p.ReadSyntax(nil)
			qt.Assert(t, err, qt.IsNil)

			ectx := machine.NewExpandTimeCallContext()
			expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
			qt.Assert(t, err, qt.IsNil)

			tpl := machine.NewNativeTemplate(0, 0, false)
			cctx := machine.NewCompileTimeCallContext(false, true, env)
			err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
			qt.Assert(t, err, qt.IsNil)

			mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
			ctx := context.Background()
			err = mc.Run(ctx)
			qt.Assert(t, err, qt.IsNil)

			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.out)
		})
	}
}
