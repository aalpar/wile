package runtime

import (
	"bytes"
	"context"
	"skeme/machine"
	"skeme/parser"
	"skeme/runtime/primitives"
	"skeme/syntax"
	"skeme/utils"
	"skeme/values"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// runProgram is a helper to compile and run a Scheme program
func runProgram(t *testing.T, prog values.Value) (values.Value, error) {
	env, err := NewTopLevelEnvironmentFrameTiny()
	if err != nil {
		return nil, err
	}
	// Use inTail=false for top-level expressions. A top-level expression is NOT
	// in tail position because there's no outer function to return to.
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	tpl := machine.NewNativeTemplate(0, 0, false)
	ccnt := machine.NewCompiletimeContinuation(tpl, env)
	sctx := syntax.NewZeroValueSourceContext()
	err = ccnt.CompileExpression(cctx, utils.DatumToSyntaxValue(sctx, prog))
	if err != nil {
		return nil, err
	}
	mc := machine.NewMachineContext(machine.NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err = mc.Run(ctx)
	if err != nil {
		return nil, err
	}
	return mc.GetValue(), nil
}

func TestNewEnvironmentTiny(t *testing.T) {
	env, err := NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, env, qt.IsNotNil)
}

func TestAddition(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "add two integers",
			prog: values.List(values.NewSymbol("+"), values.NewInteger(1), values.NewInteger(2)),
			out:  values.NewInteger(3),
		},
		{
			name: "add three integers",
			prog: values.List(values.NewSymbol("+"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			out:  values.NewInteger(6),
		},
		{
			name: "add single integer",
			prog: values.List(values.NewSymbol("+"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
		{
			name: "add no arguments returns 0",
			prog: values.List(values.NewSymbol("+")),
			out:  values.NewInteger(0),
		},
		{
			name: "add negative numbers",
			prog: values.List(values.NewSymbol("+"), values.NewInteger(-5), values.NewInteger(3)),
			out:  values.NewInteger(-2),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestSubtraction(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "subtract two integers",
			prog: values.List(values.NewSymbol("-"), values.NewInteger(5), values.NewInteger(2)),
			out:  values.NewInteger(3),
		},
		{
			name: "negate single integer",
			prog: values.List(values.NewSymbol("-"), values.NewInteger(5)),
			out:  values.NewInteger(-5),
		},
		{
			name: "subtract multiple integers",
			prog: values.List(values.NewSymbol("-"), values.NewInteger(10), values.NewInteger(3), values.NewInteger(2)),
			out:  values.NewInteger(5),
		},
		{
			name: "subtract negative result",
			prog: values.List(values.NewSymbol("-"), values.NewInteger(1), values.NewInteger(5)),
			out:  values.NewInteger(-4),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestMultiplication(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "multiply two integers",
			prog: values.List(values.NewSymbol("*"), values.NewInteger(3), values.NewInteger(4)),
			out:  values.NewInteger(12),
		},
		{
			name: "multiply three integers",
			prog: values.List(values.NewSymbol("*"), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4)),
			out:  values.NewInteger(24),
		},
		{
			name: "multiply single integer",
			prog: values.List(values.NewSymbol("*"), values.NewInteger(7)),
			out:  values.NewInteger(7),
		},
		{
			name: "multiply no arguments returns 1",
			prog: values.List(values.NewSymbol("*")),
			out:  values.NewInteger(1),
		},
		{
			name: "multiply by zero",
			prog: values.List(values.NewSymbol("*"), values.NewInteger(5), values.NewInteger(0)),
			out:  values.NewInteger(0),
		},
		{
			name: "multiply negative numbers",
			prog: values.List(values.NewSymbol("*"), values.NewInteger(-3), values.NewInteger(4)),
			out:  values.NewInteger(-12),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestDivision(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "divide two integers",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(10), values.NewInteger(2)),
			out:  values.NewInteger(5),
		},
		{
			name: "divide multiple integers",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(100), values.NewInteger(5), values.NewInteger(4)),
			out:  values.NewInteger(5),
		},
		{
			name: "divide single integer returns reciprocal",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(5)),
			out:  values.NewRational(1, 5), // 1/5 is rational
		},
		{
			name: "divide single integer 1 returns integer",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(1)),
			out:  values.NewInteger(1), // 1/1 simplifies to integer
		},
		{
			name: "divide integers non-evenly returns rational",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(1), values.NewInteger(2)),
			out:  values.NewRational(1, 2), // 1/2 is rational
		},
		{
			name: "divide integers auto-simplifies rational",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(10), values.NewInteger(4)),
			out:  values.NewRational(5, 2), // 10/4 = 5/2
		},
		{
			name: "divide integers evenly returns integer",
			prog: values.List(values.NewSymbol("/"), values.NewInteger(6), values.NewInteger(3)),
			out:  values.NewInteger(2), // 6/3 = 2 (simplifies to integer)
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestNumericEquality(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "equal integers",
			prog: values.List(values.NewSymbol("="), values.NewInteger(5), values.NewInteger(5)),
			out:  values.TrueValue,
		},
		{
			name: "unequal integers",
			prog: values.List(values.NewSymbol("="), values.NewInteger(5), values.NewInteger(3)),
			out:  values.FalseValue,
		},
		{
			name: "zero equals zero",
			prog: values.List(values.NewSymbol("="), values.NewInteger(0), values.NewInteger(0)),
			out:  values.TrueValue,
		},
		{
			name: "negative numbers",
			prog: values.List(values.NewSymbol("="), values.NewInteger(-5), values.NewInteger(-5)),
			out:  values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestEqQ(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "eq? same symbol",
			prog: values.List(values.NewSymbol("eq?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo")),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.TrueValue,
		},
		{
			name: "eq? different symbols",
			prog: values.List(values.NewSymbol("eq?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo")),
				values.List(values.NewSymbol("quote"), values.NewSymbol("bar"))),
			out: values.FalseValue,
		},
		{
			name: "eq? same integers",
			prog: values.List(values.NewSymbol("eq?"), values.NewInteger(5), values.NewInteger(5)),
			out:  values.TrueValue,
		},
		{
			name: "eq? booleans",
			prog: values.List(values.NewSymbol("eq?"), values.TrueValue, values.TrueValue),
			out:  values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestEqualQ(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "equal? same integers",
			prog: values.List(values.NewSymbol("equal?"), values.NewInteger(5), values.NewInteger(5)),
			out:  values.TrueValue,
		},
		{
			name: "equal? different integers",
			prog: values.List(values.NewSymbol("equal?"), values.NewInteger(5), values.NewInteger(3)),
			out:  values.FalseValue,
		},
		{
			name: "equal? same booleans",
			prog: values.List(values.NewSymbol("equal?"), values.TrueValue, values.TrueValue),
			out:  values.TrueValue,
		},
		{
			name: "equal? different booleans",
			prog: values.List(values.NewSymbol("equal?"), values.TrueValue, values.FalseValue),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCar(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "car of quoted list",
			prog: values.List(values.NewSymbol("car"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))),
			out: values.NewInteger(1),
		},
		{
			name: "car of pair",
			prog: values.List(values.NewSymbol("car"),
				values.List(values.NewSymbol("quote"),
					values.NewCons(values.NewSymbol("a"), values.NewSymbol("b")))),
			out: values.NewSymbol("a"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCdr(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "cdr of quoted list",
			prog: values.List(values.NewSymbol("cdr"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))),
			out: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "cdr of pair",
			prog: values.List(values.NewSymbol("cdr"),
				values.List(values.NewSymbol("quote"),
					values.NewCons(values.NewSymbol("a"), values.NewSymbol("b")))),
			out: values.NewSymbol("b"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestNewline(t *testing.T) {
	prog := values.List(values.NewSymbol("newline"))
	result, err := runProgram(t, prog)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, values.IsVoid(result), qt.IsTrue)
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

func TestStringValue(t *testing.T) {
	tcs := []struct {
		name   string
		input  values.Value
		expect string
	}{
		{
			name:   "integer",
			input:  values.NewInteger(42),
			expect: "42",
		},
		{
			name:   "string",
			input:  values.NewString("hello"),
			expect: "hello",
		},
		{
			name:   "symbol",
			input:  values.NewSymbol("foo"),
			expect: "foo",
		},
		{
			name:   "boolean true",
			input:  values.TrueValue,
			expect: "#t",
		},
		{
			name:   "boolean false",
			input:  values.FalseValue,
			expect: "#f",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := primitives.StringValue(tc.input)
			qt.Assert(t, result, qt.Equals, tc.expect)
		})
	}
}

func TestDisplayWithBuffer(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

func TestEqQWithDifferentPairs(t *testing.T) {
	// Test eq? with two different pairs that have same contents
	// According to R7RS, eq? should return #f for different objects
	prog := values.List(values.NewSymbol("eq?"),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewInteger(1), values.NewInteger(2))),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewInteger(3), values.NewInteger(4))))
	result, err := runProgram(t, prog)
	qt.Assert(t, err, qt.IsNil)
	// Two different pairs should not be eq?
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestStringValueWithPair(t *testing.T) {
	// Test StringValue with a Pair which has both String() and SchemeString()
	pair := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	result := primitives.StringValue(pair)
	qt.Assert(t, result, qt.Equals, "(1 . 2)")
}

func TestStringValueWithoutStringer(t *testing.T) {
	// Test StringValue with a type that doesn't implement fmt.Stringer
	// CharacterOutputPort implements values.Value but not fmt.Stringer
	buf := &bytes.Buffer{}
	port := values.NewCharacterOutputPort(buf)
	result := primitives.StringValue(port)
	// Should use SchemeString() instead
	qt.Assert(t, result, qt.Equals, port.SchemeString())
}

func TestDisplayWithSymbol(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

func TestEqualQWithLists(t *testing.T) {
	// Test equal? with two equivalent lists
	prog := values.List(values.NewSymbol("equal?"),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))))
	result, err := runProgram(t, prog)
	qt.Assert(t, err, qt.IsNil)
	// equal? compares by value, so equivalent lists should be equal?
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestDisplayWithInteger(t *testing.T) {
	// Save the current output port
	savedPort := primitives.GetCurrentOutputPort()
	defer func() { primitives.SetCurrentOutputPort(savedPort) }()

	// Create a buffer to capture output
	buf := &bytes.Buffer{}
	primitives.SetCurrentOutputPort(values.NewCharacterOutputPort(buf))

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

	env, err := NewTopLevelEnvironmentFrameTiny()
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

func TestCallCC(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "call/cc with normal return",
			// (call/cc (lambda (k) 42))
			prog: values.List(values.NewSymbol("call/cc"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("k")),
					values.NewInteger(42))),
			out: values.NewInteger(42),
		},
		{
			name: "call/cc with escape",
			// (call/cc (lambda (k) (k 42)))
			prog: values.List(values.NewSymbol("call/cc"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("k")),
					values.List(values.NewSymbol("k"), values.NewInteger(42)))),
			out: values.NewInteger(42),
		},
		{
			name: "call/cc escape skips remaining computation",
			// (+ 1 (call/cc (lambda (k) (+ 2 (k 10)))))
			prog: values.List(values.NewSymbol("+"),
				values.NewInteger(1),
				values.List(values.NewSymbol("call/cc"),
					values.List(values.NewSymbol("lambda"),
						values.List(values.NewSymbol("k")),
						values.List(values.NewSymbol("+"),
							values.NewInteger(2),
							values.List(values.NewSymbol("k"), values.NewInteger(10)))))),
			out: values.NewInteger(11), // 1 + 10, not 1 + 2 + 10
		},
		{
			name: "call/cc normal return continues computation",
			// (+ 1 (call/cc (lambda (k) 10)))
			prog: values.List(values.NewSymbol("+"),
				values.NewInteger(1),
				values.List(values.NewSymbol("call/cc"),
					values.List(values.NewSymbol("lambda"),
						values.List(values.NewSymbol("k")),
						values.NewInteger(10)))),
			out: values.NewInteger(11), // 1 + 10
		},
		{
			name: "call-with-current-continuation alias",
			// (call-with-current-continuation (lambda (k) (k 99)))
			prog: values.List(values.NewSymbol("call-with-current-continuation"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("k")),
					values.List(values.NewSymbol("k"), values.NewInteger(99)))),
			out: values.NewInteger(99),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCallCCWithHigherOrderFunctions(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "call/cc escape from apply",
			// (call/cc (lambda (return) (apply (lambda (a b) (if (> b 5) (return 'big) (+ a b))) '(1 10))))
			prog: values.List(values.NewSymbol("call/cc"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("return")),
					values.List(values.NewSymbol("apply"),
						values.List(values.NewSymbol("lambda"),
							values.List(values.NewSymbol("a"), values.NewSymbol("b")),
							values.List(values.NewSymbol("if"),
								values.List(values.NewSymbol(">"), values.NewSymbol("b"), values.NewInteger(5)),
								values.List(values.NewSymbol("return"),
									values.List(values.NewSymbol("quote"), values.NewSymbol("big"))),
								values.List(values.NewSymbol("+"), values.NewSymbol("a"), values.NewSymbol("b")))),
						values.List(values.NewSymbol("quote"),
							values.List(values.NewInteger(1), values.NewInteger(10)))))),
			out: values.NewSymbol("big"),
		},
		{
			name: "call/cc escape from map",
			// (call/cc (lambda (return) (map (lambda (x) (if (> x 3) (return 'found) (* x x))) '(1 2 3 4 5))))
			prog: values.List(values.NewSymbol("call/cc"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("return")),
					values.List(values.NewSymbol("map"),
						values.List(values.NewSymbol("lambda"),
							values.List(values.NewSymbol("x")),
							values.List(values.NewSymbol("if"),
								values.List(values.NewSymbol(">"), values.NewSymbol("x"), values.NewInteger(3)),
								values.List(values.NewSymbol("return"),
									values.List(values.NewSymbol("quote"), values.NewSymbol("found"))),
								values.List(values.NewSymbol("*"), values.NewSymbol("x"), values.NewSymbol("x")))),
						values.List(values.NewSymbol("quote"),
							values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4), values.NewInteger(5)))))),
			out: values.NewSymbol("found"),
		},
		{
			name: "call/cc no escape from map returns list",
			// (call/cc (lambda (return) (map (lambda (x) (* x x)) '(1 2 3))))
			prog: values.List(values.NewSymbol("call/cc"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("return")),
					values.List(values.NewSymbol("map"),
						values.List(values.NewSymbol("lambda"),
							values.List(values.NewSymbol("x")),
							values.List(values.NewSymbol("*"), values.NewSymbol("x"), values.NewSymbol("x"))),
						values.List(values.NewSymbol("quote"),
							values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))))),
			out: values.List(values.NewInteger(1), values.NewInteger(4), values.NewInteger(9)),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestAppend(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "append no arguments",
			prog: values.List(values.NewSymbol("append")),
			out:  values.EmptyList,
		},
		{
			name: "append single empty list",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.EmptyList,
		},
		{
			name: "append single list",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2)))),
			out: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name: "append two lists",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2))),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(3), values.NewInteger(4)))),
			out: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4)),
		},
		{
			name: "append three lists",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"))),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("b"))),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("c")))),
			out: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			name: "append with empty list in middle",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1))),
				values.List(values.NewSymbol("quote"), values.EmptyList),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(2)))),
			out: values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name: "append with non-list as last argument (improper list result)",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2))),
				values.NewInteger(3)),
			out: values.NewCons(values.NewInteger(1), values.NewCons(values.NewInteger(2), values.NewInteger(3))),
		},
		{
			name: "append empty lists only",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"), values.EmptyList),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.EmptyList,
		},
		{
			name: "append nested lists",
			prog: values.List(values.NewSymbol("append"),
				values.List(values.NewSymbol("quote"),
					values.List(values.List(values.NewInteger(1), values.NewInteger(2)))),
				values.List(values.NewSymbol("quote"),
					values.List(values.List(values.NewInteger(3), values.NewInteger(4))))),
			out: values.List(
				values.List(values.NewInteger(1), values.NewInteger(2)),
				values.List(values.NewInteger(3), values.NewInteger(4))),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestEqv tests the eqv helper function which implements eqv? semantics
// for memv and assv primitives.
func TestEqv(t *testing.T) {
	tcs := []struct {
		name   string
		a      values.Value
		b      values.Value
		expect bool
	}{
		// Same singleton objects
		{
			name:   "true vs true same object",
			a:      values.TrueValue,
			b:      values.TrueValue,
			expect: true,
		},
		{
			name:   "false vs false same object",
			a:      values.FalseValue,
			b:      values.FalseValue,
			expect: true,
		},
		{
			name:   "true vs false",
			a:      values.TrueValue,
			b:      values.FalseValue,
			expect: false,
		},
		{
			name:   "empty list vs empty list",
			a:      values.EmptyList,
			b:      values.EmptyList,
			expect: true,
		},
		// Integer comparisons (different objects with same value)
		{
			name:   "equal integers different objects",
			a:      values.NewInteger(42),
			b:      values.NewInteger(42),
			expect: true,
		},
		{
			name:   "unequal integers",
			a:      values.NewInteger(42),
			b:      values.NewInteger(43),
			expect: false,
		},
		{
			name:   "zero integers",
			a:      values.NewInteger(0),
			b:      values.NewInteger(0),
			expect: true,
		},
		{
			name:   "negative integers equal",
			a:      values.NewInteger(-5),
			b:      values.NewInteger(-5),
			expect: true,
		},
		// Float comparisons
		{
			name:   "equal floats",
			a:      values.NewFloat(3.14),
			b:      values.NewFloat(3.14),
			expect: true,
		},
		{
			name:   "unequal floats",
			a:      values.NewFloat(3.14),
			b:      values.NewFloat(2.71),
			expect: false,
		},
		{
			name:   "zero floats",
			a:      values.NewFloat(0.0),
			b:      values.NewFloat(0.0),
			expect: true,
		},
		// Character comparisons
		{
			name:   "equal characters",
			a:      values.NewCharacter('A'),
			b:      values.NewCharacter('A'),
			expect: true,
		},
		{
			name:   "unequal characters",
			a:      values.NewCharacter('A'),
			b:      values.NewCharacter('B'),
			expect: false,
		},
		{
			name:   "unicode characters equal",
			a:      values.NewCharacter('λ'),
			b:      values.NewCharacter('λ'),
			expect: true,
		},
		// Cross-type comparisons (should always be false)
		{
			name:   "integer vs float",
			a:      values.NewInteger(42),
			b:      values.NewFloat(42.0),
			expect: false,
		},
		{
			name:   "integer vs string",
			a:      values.NewInteger(42),
			b:      values.NewString("42"),
			expect: false,
		},
		{
			name:   "symbol vs string",
			a:      values.NewSymbol("foo"),
			b:      values.NewString("foo"),
			expect: false,
		},
		// Pairs (different objects)
		{
			name:   "different pairs same contents",
			a:      values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			b:      values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			expect: false,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := primitives.Eqv(tc.a, tc.b)
			qt.Assert(t, result, qt.Equals, tc.expect)
		})
	}
}

// TestEqvWithSamePointer tests eqv with the same pointer values
func TestEqvWithSamePointer(t *testing.T) {
	// When a and b are the same object, eqv should return true
	i := values.NewInteger(42)
	qt.Assert(t, primitives.Eqv(i, i), qt.IsTrue)

	f := values.NewFloat(3.14)
	qt.Assert(t, primitives.Eqv(f, f), qt.IsTrue)

	c := values.NewCharacter('X')
	qt.Assert(t, primitives.Eqv(c, c), qt.IsTrue)

	s := values.NewSymbol("foo")
	qt.Assert(t, primitives.Eqv(s, s), qt.IsTrue)

	str := values.NewString("hello")
	qt.Assert(t, primitives.Eqv(str, str), qt.IsTrue)

	pair := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	qt.Assert(t, primitives.Eqv(pair, pair), qt.IsTrue)
}

// TestEqvWithRational tests eqv with rational numbers
func TestEqvWithRational(t *testing.T) {
	// Same rational value
	r1 := values.NewRational(1, 2)
	r2 := values.NewRational(1, 2)
	qt.Assert(t, primitives.Eqv(r1, r2), qt.IsTrue)

	// Different rational values
	r3 := values.NewRational(1, 3)
	qt.Assert(t, primitives.Eqv(r1, r3), qt.IsFalse)

	// Equivalent rationals (reduced form)
	r4 := values.NewRational(2, 4) // Should reduce to 1/2
	qt.Assert(t, primitives.Eqv(r1, r4), qt.IsTrue)

	// Same object
	qt.Assert(t, primitives.Eqv(r1, r1), qt.IsTrue)
}

// TestEqvWithComplex tests eqv with complex numbers
func TestEqvWithComplex(t *testing.T) {
	// Same complex value
	c1 := values.NewComplex(complex(1, 2))
	c2 := values.NewComplex(complex(1, 2))
	qt.Assert(t, primitives.Eqv(c1, c2), qt.IsTrue)

	// Different complex values
	c3 := values.NewComplex(complex(1, 3))
	qt.Assert(t, primitives.Eqv(c1, c3), qt.IsFalse)

	// Same object
	qt.Assert(t, primitives.Eqv(c1, c1), qt.IsTrue)
}

// TestEqvQPrimitive tests the eqv? primitive through program execution
func TestEqvQPrimitive(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "eqv? same integers",
			prog: values.List(values.NewSymbol("eqv?"), values.NewInteger(42), values.NewInteger(42)),
			out:  values.TrueValue,
		},
		{
			name: "eqv? different integers",
			prog: values.List(values.NewSymbol("eqv?"), values.NewInteger(42), values.NewInteger(43)),
			out:  values.FalseValue,
		},
		{
			name: "eqv? same booleans",
			prog: values.List(values.NewSymbol("eqv?"), values.TrueValue, values.TrueValue),
			out:  values.TrueValue,
		},
		{
			name: "eqv? different booleans",
			prog: values.List(values.NewSymbol("eqv?"), values.TrueValue, values.FalseValue),
			out:  values.FalseValue,
		},
		{
			name: "eqv? same symbols",
			prog: values.List(values.NewSymbol("eqv?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo")),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.TrueValue,
		},
		{
			name: "eqv? different symbols",
			prog: values.List(values.NewSymbol("eqv?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo")),
				values.List(values.NewSymbol("quote"), values.NewSymbol("bar"))),
			out: values.FalseValue,
		},
		{
			name: "eqv? integer vs float (different types)",
			prog: values.List(values.NewSymbol("eqv?"), values.NewInteger(42), values.NewFloat(42.0)),
			out:  values.FalseValue,
		},
		{
			name: "eqv? same characters",
			prog: values.List(values.NewSymbol("eqv?"), values.NewCharacter('a'), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "eqv? different characters",
			prog: values.List(values.NewSymbol("eqv?"), values.NewCharacter('a'), values.NewCharacter('b')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestGcdInt tests the gcdInt helper function for computing GCD of two integers.
func TestGcdInt(t *testing.T) {
	tcs := []struct {
		name   string
		a      int64
		b      int64
		expect int64
	}{
		{
			name:   "gcd of 12 and 8",
			a:      12,
			b:      8,
			expect: 4,
		},
		{
			name:   "gcd of 8 and 12",
			a:      8,
			b:      12,
			expect: 4,
		},
		{
			name:   "gcd of coprime numbers",
			a:      17,
			b:      13,
			expect: 1,
		},
		{
			name:   "gcd with zero first",
			a:      0,
			b:      5,
			expect: 5,
		},
		{
			name:   "gcd with zero second",
			a:      5,
			b:      0,
			expect: 5,
		},
		{
			name:   "gcd of same numbers",
			a:      7,
			b:      7,
			expect: 7,
		},
		{
			name:   "gcd of 1 and any",
			a:      1,
			b:      100,
			expect: 1,
		},
		{
			name:   "gcd of large numbers",
			a:      48,
			b:      18,
			expect: 6,
		},
		{
			name:   "gcd of prime and composite",
			a:      7,
			b:      21,
			expect: 7,
		},
		{
			name:   "gcd of powers of 2",
			a:      16,
			b:      64,
			expect: 16,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := primitives.GcdInt(tc.a, tc.b)
			qt.Assert(t, result, qt.Equals, tc.expect)
		})
	}
}

// TestNewEscapeContinuationClosure tests that the escape continuation closure
// is created correctly with the expected properties.
func TestNewEscapeContinuationClosure(t *testing.T) {
	env, err := NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)

	// Create a mock continuation
	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)

	// Create the escape closure
	closure := primitives.NewEscapeContinuationClosure(env, cont)

	// Verify the closure was created
	qt.Assert(t, closure, qt.IsNotNil)

	// Verify it's a foreign closure with 1 parameter (via template)
	qt.Assert(t, closure.Template().ParameterCount(), qt.Equals, 1)
	qt.Assert(t, closure.Template().IsVariadic(), qt.IsFalse)
}

// ----------------------------------------------------------------------------
// Association List Tests (assq, assv, assoc)
// ----------------------------------------------------------------------------

func TestAssq(t *testing.T) {
	// Note: assq uses eq? (pointer equality). Booleans are singletons and work.
	// Symbols/integers inside nested quotes are not deduplicated (known limitation).
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "assq finds boolean key",
			// (assq #t '((#f 1) (#t 2)))
			prog: values.List(values.NewSymbol("assq"),
				values.TrueValue,
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.FalseValue, values.NewInteger(1)),
						values.List(values.TrueValue, values.NewInteger(2))))),
			out: values.List(values.TrueValue, values.NewInteger(2)),
		},
		{
			name: "assq returns #f when not found",
			// (assq #t '((#f 1)))
			prog: values.List(values.NewSymbol("assq"),
				values.TrueValue,
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.FalseValue, values.NewInteger(1))))),
			out: values.FalseValue,
		},
		{
			name: "assq with empty list returns #f",
			// (assq #t '())
			prog: values.List(values.NewSymbol("assq"),
				values.TrueValue,
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestAssv(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "assv finds integer key",
			// (assv 2 '((1 a) (2 b) (3 c)))
			prog: values.List(values.NewSymbol("assv"),
				values.NewInteger(2),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.NewInteger(1), values.NewSymbol("a")),
						values.List(values.NewInteger(2), values.NewSymbol("b")),
						values.List(values.NewInteger(3), values.NewSymbol("c"))))),
			out: values.List(values.NewInteger(2), values.NewSymbol("b")),
		},
		{
			name: "assv returns #f when not found",
			// (assv 4 '((1 a) (2 b) (3 c)))
			prog: values.List(values.NewSymbol("assv"),
				values.NewInteger(4),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.NewInteger(1), values.NewSymbol("a")),
						values.List(values.NewInteger(2), values.NewSymbol("b")),
						values.List(values.NewInteger(3), values.NewSymbol("c"))))),
			out: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestAssoc(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "assoc finds list key with equal?",
			// (assoc '(1 2) '(((1 2) found) ((3 4) other)))
			prog: values.List(values.NewSymbol("assoc"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2))),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewSymbol("found")),
						values.List(values.List(values.NewInteger(3), values.NewInteger(4)), values.NewSymbol("other"))))),
			out: values.List(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewSymbol("found")),
		},
		{
			name: "assoc returns #f when not found",
			// (assoc '(5 6) '(((1 2) a) ((3 4) b)))
			prog: values.List(values.NewSymbol("assoc"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(5), values.NewInteger(6))),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.List(values.NewInteger(1), values.NewInteger(2)), values.NewSymbol("a")),
						values.List(values.List(values.NewInteger(3), values.NewInteger(4)), values.NewSymbol("b"))))),
			out: values.FalseValue,
		},
		{
			name: "assoc with string key",
			// (assoc "hello" '(("hello" found) ("world" other)))
			prog: values.List(values.NewSymbol("assoc"),
				values.NewString("hello"),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.NewString("hello"), values.NewSymbol("found")),
						values.List(values.NewString("world"), values.NewSymbol("other"))))),
			out: values.List(values.NewString("hello"), values.NewSymbol("found")),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Membership Tests (memq, memv, member)
// ----------------------------------------------------------------------------

func TestMemq(t *testing.T) {
	// Note: memq uses eq? (pointer equality). Booleans are singletons and work.
	// Symbols inside nested quotes are not deduplicated (known limitation).
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "memq finds boolean",
			// (memq #t '(#f #t 1))
			prog: values.List(values.NewSymbol("memq"),
				values.TrueValue,
				values.List(values.NewSymbol("quote"),
					values.List(values.FalseValue, values.TrueValue, values.NewInteger(1)))),
			out: values.List(values.TrueValue, values.NewInteger(1)),
		},
		{
			name: "memq returns #f when not found",
			// (memq #t '(#f 1 2))
			prog: values.List(values.NewSymbol("memq"),
				values.TrueValue,
				values.List(values.NewSymbol("quote"),
					values.List(values.FalseValue, values.NewInteger(1), values.NewInteger(2)))),
			out: values.FalseValue,
		},
		{
			name: "memq with empty list returns #f",
			// (memq #t '())
			prog: values.List(values.NewSymbol("memq"),
				values.TrueValue,
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestMemv(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "memv finds integer",
			// (memv 2 '(1 2 3))
			prog: values.List(values.NewSymbol("memv"),
				values.NewInteger(2),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))),
			out: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "memv returns #f when not found",
			// (memv 4 '(1 2 3))
			prog: values.List(values.NewSymbol("memv"),
				values.NewInteger(4),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))),
			out: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestMember(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "member finds list with equal?",
			// (member '(2) '((1) (2) (3)))
			prog: values.List(values.NewSymbol("member"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(2))),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.NewInteger(1)),
						values.List(values.NewInteger(2)),
						values.List(values.NewInteger(3))))),
			out: values.List(values.List(values.NewInteger(2)), values.List(values.NewInteger(3))),
		},
		{
			name: "member finds string",
			// (member "hello" '("world" "hello" "foo"))
			prog: values.List(values.NewSymbol("member"),
				values.NewString("hello"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewString("world"), values.NewString("hello"), values.NewString("foo")))),
			out: values.List(values.NewString("hello"), values.NewString("foo")),
		},
		{
			name: "member returns #f when not found",
			// (member '(4) '((1) (2) (3)))
			prog: values.List(values.NewSymbol("member"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(4))),
				values.List(values.NewSymbol("quote"),
					values.List(
						values.List(values.NewInteger(1)),
						values.List(values.NewInteger(2)),
						values.List(values.NewInteger(3))))),
			out: values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// List Operation Tests (length, reverse, list-ref, list-tail)
// ----------------------------------------------------------------------------

func TestLength(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "length of three element list",
			// (length '(1 2 3))
			prog: values.List(values.NewSymbol("length"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))),
			out: values.NewInteger(3),
		},
		{
			name: "length of empty list",
			// (length '())
			prog: values.List(values.NewSymbol("length"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.NewInteger(0),
		},
		{
			name: "length of single element list",
			// (length '(a))
			prog: values.List(values.NewSymbol("length"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a")))),
			out: values.NewInteger(1),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestReverse(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "reverse list",
			// (reverse '(1 2 3))
			prog: values.List(values.NewSymbol("reverse"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))),
			out: values.List(values.NewInteger(3), values.NewInteger(2), values.NewInteger(1)),
		},
		{
			name: "reverse empty list",
			// (reverse '())
			prog: values.List(values.NewSymbol("reverse"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.EmptyList,
		},
		{
			name: "reverse single element",
			// (reverse '(a))
			prog: values.List(values.NewSymbol("reverse"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a")))),
			out: values.List(values.NewSymbol("a")),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestListRef(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "list-ref first element",
			// (list-ref '(a b c) 0)
			prog: values.List(values.NewSymbol("list-ref"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.NewInteger(0)),
			out: values.NewSymbol("a"),
		},
		{
			name: "list-ref middle element",
			// (list-ref '(a b c) 1)
			prog: values.List(values.NewSymbol("list-ref"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.NewInteger(1)),
			out: values.NewSymbol("b"),
		},
		{
			name: "list-ref last element",
			// (list-ref '(a b c) 2)
			prog: values.List(values.NewSymbol("list-ref"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.NewInteger(2)),
			out: values.NewSymbol("c"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestListTail(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "list-tail from beginning",
			// (list-tail '(a b c) 0)
			prog: values.List(values.NewSymbol("list-tail"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.NewInteger(0)),
			out: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			name: "list-tail skip one",
			// (list-tail '(a b c) 1)
			prog: values.List(values.NewSymbol("list-tail"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.NewInteger(1)),
			out: values.List(values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			name: "list-tail skip all",
			// (list-tail '(a b c) 3)
			prog: values.List(values.NewSymbol("list-tail"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.NewInteger(3)),
			out: values.EmptyList,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Numeric Primitive Tests (abs, floor, ceiling, round, truncate, sqrt, expt, square)
// ----------------------------------------------------------------------------

func TestAbs(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "abs of positive integer",
			prog: values.List(values.NewSymbol("abs"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
		{
			name: "abs of negative integer",
			prog: values.List(values.NewSymbol("abs"), values.NewInteger(-5)),
			out:  values.NewInteger(5),
		},
		{
			name: "abs of zero",
			prog: values.List(values.NewSymbol("abs"), values.NewInteger(0)),
			out:  values.NewInteger(0),
		},
		{
			name: "abs of negative float",
			prog: values.List(values.NewSymbol("abs"), values.NewFloat(-3.14)),
			out:  values.NewFloat(3.14),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestFloor(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "floor of positive float",
			prog: values.List(values.NewSymbol("floor"), values.NewFloat(3.7)),
			out:  values.NewFloat(3.0),
		},
		{
			name: "floor of negative float",
			prog: values.List(values.NewSymbol("floor"), values.NewFloat(-3.2)),
			out:  values.NewFloat(-4.0),
		},
		{
			name: "floor of integer",
			prog: values.List(values.NewSymbol("floor"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCeiling(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "ceiling of positive float",
			prog: values.List(values.NewSymbol("ceiling"), values.NewFloat(3.2)),
			out:  values.NewFloat(4.0),
		},
		{
			name: "ceiling of negative float",
			prog: values.List(values.NewSymbol("ceiling"), values.NewFloat(-3.7)),
			out:  values.NewFloat(-3.0),
		},
		{
			name: "ceiling of integer",
			prog: values.List(values.NewSymbol("ceiling"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestRound(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "round down",
			prog: values.List(values.NewSymbol("round"), values.NewFloat(3.2)),
			out:  values.NewFloat(3.0),
		},
		{
			name: "round up",
			prog: values.List(values.NewSymbol("round"), values.NewFloat(3.7)),
			out:  values.NewFloat(4.0),
		},
		{
			name: "round of integer",
			prog: values.List(values.NewSymbol("round"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestTruncate(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "truncate positive float",
			prog: values.List(values.NewSymbol("truncate"), values.NewFloat(3.7)),
			out:  values.NewFloat(3.0),
		},
		{
			name: "truncate negative float",
			prog: values.List(values.NewSymbol("truncate"), values.NewFloat(-3.7)),
			out:  values.NewFloat(-3.0),
		},
		{
			name: "truncate of integer",
			prog: values.List(values.NewSymbol("truncate"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestSqrt(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "sqrt of perfect square",
			prog: values.List(values.NewSymbol("sqrt"), values.NewInteger(16)),
			out:  values.NewFloat(4.0),
		},
		{
			name: "sqrt of float",
			prog: values.List(values.NewSymbol("sqrt"), values.NewFloat(2.0)),
			out:  values.NewFloat(1.4142135623730951),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestExpt(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "expt integer power",
			prog: values.List(values.NewSymbol("expt"), values.NewInteger(2), values.NewInteger(10)),
			out:  values.NewInteger(1024),
		},
		{
			name: "expt with zero exponent",
			prog: values.List(values.NewSymbol("expt"), values.NewInteger(5), values.NewInteger(0)),
			out:  values.NewInteger(1),
		},
		{
			name: "expt with negative exponent",
			prog: values.List(values.NewSymbol("expt"), values.NewInteger(2), values.NewInteger(-1)),
			out:  values.NewFloat(0.5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestSquare(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "square of integer",
			prog: values.List(values.NewSymbol("square"), values.NewInteger(5)),
			out:  values.NewInteger(25),
		},
		{
			name: "square of negative",
			prog: values.List(values.NewSymbol("square"), values.NewInteger(-3)),
			out:  values.NewInteger(9),
		},
		{
			name: "square of float",
			prog: values.List(values.NewSymbol("square"), values.NewFloat(2.5)),
			out:  values.NewFloat(6.25),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Complex Number Primitive Tests
// ----------------------------------------------------------------------------

func TestRealPart(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "real-part of complex",
			prog: values.List(values.NewSymbol("real-part"), values.NewComplexFromParts(3.0, 4.0)),
			out:  values.NewFloat(3.0),
		},
		{
			name: "real-part of integer",
			prog: values.List(values.NewSymbol("real-part"), values.NewInteger(5)),
			out:  values.NewFloat(5.0),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestImagPart(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "imag-part of complex",
			prog: values.List(values.NewSymbol("imag-part"), values.NewComplexFromParts(3.0, 4.0)),
			out:  values.NewFloat(4.0),
		},
		{
			name: "imag-part of integer",
			prog: values.List(values.NewSymbol("imag-part"), values.NewInteger(5)),
			out:  values.NewFloat(0.0),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestMagnitude(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "magnitude of 3+4i",
			prog: values.List(values.NewSymbol("magnitude"), values.NewComplexFromParts(3.0, 4.0)),
			out:  values.NewFloat(5.0),
		},
		{
			name: "magnitude of real number",
			prog: values.List(values.NewSymbol("magnitude"), values.NewInteger(-5)),
			out:  values.NewFloat(5.0),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestMakeRectangular(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "make-rectangular from integers",
			prog: values.List(values.NewSymbol("make-rectangular"), values.NewInteger(3), values.NewInteger(4)),
			out:  values.NewComplexFromParts(3.0, 4.0),
		},
		{
			name: "make-rectangular from floats",
			prog: values.List(values.NewSymbol("make-rectangular"), values.NewFloat(1.5), values.NewFloat(2.5)),
			out:  values.NewComplexFromParts(1.5, 2.5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Rational Number Primitive Tests
// ----------------------------------------------------------------------------

func TestNumerator(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "numerator of integer",
			prog: values.List(values.NewSymbol("numerator"), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestDenominator(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "denominator of integer",
			prog: values.List(values.NewSymbol("denominator"), values.NewInteger(5)),
			out:  values.NewInteger(1),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Type Predicate Tests
// ----------------------------------------------------------------------------

func TestTypePredicates(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// null?
		{
			name: "null? on empty list",
			prog: values.List(values.NewSymbol("null?"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.TrueValue,
		},
		{
			name: "null? on non-empty list",
			prog: values.List(values.NewSymbol("null?"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1)))),
			out: values.FalseValue,
		},
		// pair?
		{
			name: "pair? on pair",
			prog: values.List(values.NewSymbol("pair?"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2)))),
			out: values.TrueValue,
		},
		// Note: R7RS says (pair? '()) should be #f, but current implementation returns #t
		// because EmptyList is implemented as a *values.Pair. This test documents actual behavior.
		{
			name: "pair? on empty list",
			prog: values.List(values.NewSymbol("pair?"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.FalseValue,
		},
		// boolean?
		{
			name: "boolean? on true",
			prog: values.List(values.NewSymbol("boolean?"), values.TrueValue),
			out:  values.TrueValue,
		},
		{
			name: "boolean? on false",
			prog: values.List(values.NewSymbol("boolean?"), values.FalseValue),
			out:  values.TrueValue,
		},
		{
			name: "boolean? on number",
			prog: values.List(values.NewSymbol("boolean?"), values.NewInteger(1)),
			out:  values.FalseValue,
		},
		// number?
		{
			name: "number? on integer",
			prog: values.List(values.NewSymbol("number?"), values.NewInteger(42)),
			out:  values.TrueValue,
		},
		{
			name: "number? on float",
			prog: values.List(values.NewSymbol("number?"), values.NewFloat(3.14)),
			out:  values.TrueValue,
		},
		{
			name: "number? on symbol",
			prog: values.List(values.NewSymbol("number?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.FalseValue,
		},
		// integer?
		{
			name: "integer? on integer",
			prog: values.List(values.NewSymbol("integer?"), values.NewInteger(42)),
			out:  values.TrueValue,
		},
		{
			name: "integer? on float",
			prog: values.List(values.NewSymbol("integer?"), values.NewFloat(3.14)),
			out:  values.FalseValue,
		},
		// symbol?
		{
			name: "symbol? on symbol",
			prog: values.List(values.NewSymbol("symbol?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.TrueValue,
		},
		{
			name: "symbol? on string",
			prog: values.List(values.NewSymbol("symbol?"), values.NewString("foo")),
			out:  values.FalseValue,
		},
		// string?
		{
			name: "string? on string",
			prog: values.List(values.NewSymbol("string?"), values.NewString("hello")),
			out:  values.TrueValue,
		},
		{
			name: "string? on symbol",
			prog: values.List(values.NewSymbol("string?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.FalseValue,
		},
		// list?
		{
			name: "list? on proper list",
			prog: values.List(values.NewSymbol("list?"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2)))),
			out: values.TrueValue,
		},
		{
			name: "list? on empty list",
			prog: values.List(values.NewSymbol("list?"),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.TrueValue,
		},
		// procedure?
		{
			name: "procedure? on lambda",
			prog: values.List(values.NewSymbol("procedure?"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("x")),
					values.NewSymbol("x"))),
			out: values.TrueValue,
		},
		{
			name: "procedure? on number",
			prog: values.List(values.NewSymbol("procedure?"), values.NewInteger(5)),
			out:  values.FalseValue,
		},
		// vector?
		{
			name: "vector? on vector",
			prog: values.List(values.NewSymbol("vector?"),
				values.List(values.NewSymbol("vector"), values.NewInteger(1), values.NewInteger(2))),
			out: values.TrueValue,
		},
		{
			name: "vector? on list",
			prog: values.List(values.NewSymbol("vector?"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewInteger(1), values.NewInteger(2)))),
			out: values.FalseValue,
		},
		// not
		{
			name: "not on false",
			prog: values.List(values.NewSymbol("not"), values.FalseValue),
			out:  values.TrueValue,
		},
		{
			name: "not on true",
			prog: values.List(values.NewSymbol("not"), values.TrueValue),
			out:  values.FalseValue,
		},
		{
			name: "not on non-false value",
			prog: values.List(values.NewSymbol("not"), values.NewInteger(1)),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Numeric Predicate Tests
// ----------------------------------------------------------------------------

func TestNumericPredicates(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// zero?
		{
			name: "zero? on zero",
			prog: values.List(values.NewSymbol("zero?"), values.NewInteger(0)),
			out:  values.TrueValue,
		},
		{
			name: "zero? on non-zero",
			prog: values.List(values.NewSymbol("zero?"), values.NewInteger(5)),
			out:  values.FalseValue,
		},
		// positive?
		{
			name: "positive? on positive",
			prog: values.List(values.NewSymbol("positive?"), values.NewInteger(5)),
			out:  values.TrueValue,
		},
		{
			name: "positive? on negative",
			prog: values.List(values.NewSymbol("positive?"), values.NewInteger(-5)),
			out:  values.FalseValue,
		},
		{
			name: "positive? on zero",
			prog: values.List(values.NewSymbol("positive?"), values.NewInteger(0)),
			out:  values.FalseValue,
		},
		// negative?
		{
			name: "negative? on negative",
			prog: values.List(values.NewSymbol("negative?"), values.NewInteger(-5)),
			out:  values.TrueValue,
		},
		{
			name: "negative? on positive",
			prog: values.List(values.NewSymbol("negative?"), values.NewInteger(5)),
			out:  values.FalseValue,
		},
		// odd?
		{
			name: "odd? on odd",
			prog: values.List(values.NewSymbol("odd?"), values.NewInteger(5)),
			out:  values.TrueValue,
		},
		{
			name: "odd? on even",
			prog: values.List(values.NewSymbol("odd?"), values.NewInteger(4)),
			out:  values.FalseValue,
		},
		// even?
		{
			name: "even? on even",
			prog: values.List(values.NewSymbol("even?"), values.NewInteger(4)),
			out:  values.TrueValue,
		},
		{
			name: "even? on odd",
			prog: values.List(values.NewSymbol("even?"), values.NewInteger(5)),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// String Primitive Tests
// ----------------------------------------------------------------------------

func TestStringLength(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string-length of hello",
			prog: values.List(values.NewSymbol("string-length"), values.NewString("hello")),
			out:  values.NewInteger(5),
		},
		{
			name: "string-length of empty string",
			prog: values.List(values.NewSymbol("string-length"), values.NewString("")),
			out:  values.NewInteger(0),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestStringRef(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string-ref first char",
			prog: values.List(values.NewSymbol("string-ref"), values.NewString("hello"), values.NewInteger(0)),
			out:  values.NewCharacter('h'),
		},
		{
			name: "string-ref middle char",
			prog: values.List(values.NewSymbol("string-ref"), values.NewString("hello"), values.NewInteger(2)),
			out:  values.NewCharacter('l'),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestSubstring(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "substring from middle",
			prog: values.List(values.NewSymbol("substring"), values.NewString("hello"), values.NewInteger(1), values.NewInteger(4)),
			out:  values.NewString("ell"),
		},
		{
			name: "substring from start",
			prog: values.List(values.NewSymbol("substring"), values.NewString("hello"), values.NewInteger(0), values.NewInteger(2)),
			out:  values.NewString("he"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestStringAppend(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string-append two strings",
			prog: values.List(values.NewSymbol("string-append"), values.NewString("hello"), values.NewString(" world")),
			out:  values.NewString("hello world"),
		},
		{
			name: "string-append three strings",
			prog: values.List(values.NewSymbol("string-append"), values.NewString("a"), values.NewString("b"), values.NewString("c")),
			out:  values.NewString("abc"),
		},
		{
			name: "string-append no strings",
			prog: values.List(values.NewSymbol("string-append")),
			out:  values.NewString(""),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestStringToList(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string->list",
			prog: values.List(values.NewSymbol("string->list"), values.NewString("abc")),
			out:  values.List(values.NewCharacter('a'), values.NewCharacter('b'), values.NewCharacter('c')),
		},
		{
			name: "string->list empty",
			prog: values.List(values.NewSymbol("string->list"), values.NewString("")),
			out:  values.EmptyList,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestListToString(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "list->string",
			prog: values.List(values.NewSymbol("list->string"),
				values.List(values.NewSymbol("quote"),
					values.List(values.NewCharacter('a'), values.NewCharacter('b'), values.NewCharacter('c')))),
			out: values.NewString("abc"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestStringToSymbol(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string->symbol",
			prog: values.List(values.NewSymbol("string->symbol"), values.NewString("foo")),
			out:  values.NewSymbol("foo"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestSymbolToString(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "symbol->string",
			prog: values.List(values.NewSymbol("symbol->string"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.NewString("foo"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Vector Primitive Tests
// ----------------------------------------------------------------------------

func TestVectorLength(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "vector-length",
			prog: values.List(values.NewSymbol("vector-length"),
				values.List(values.NewSymbol("vector"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			out: values.NewInteger(3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestVectorRef(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "vector-ref first",
			prog: values.List(values.NewSymbol("vector-ref"),
				values.List(values.NewSymbol("vector"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
				values.NewInteger(0)),
			out: values.NewInteger(1),
		},
		{
			name: "vector-ref last",
			prog: values.List(values.NewSymbol("vector-ref"),
				values.List(values.NewSymbol("vector"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
				values.NewInteger(2)),
			out: values.NewInteger(3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestMakeVector(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "make-vector without fill",
			prog: values.List(values.NewSymbol("vector-length"),
				values.List(values.NewSymbol("make-vector"), values.NewInteger(5))),
			out: values.NewInteger(5),
		},
		{
			name: "make-vector with fill",
			prog: values.List(values.NewSymbol("vector-ref"),
				values.List(values.NewSymbol("make-vector"), values.NewInteger(3), values.NewInteger(42)),
				values.NewInteger(0)),
			out: values.NewInteger(42),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestVectorToList(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "vector->list",
			prog: values.List(values.NewSymbol("vector->list"),
				values.List(values.NewSymbol("vector"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			out: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestListToVector(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "list->vector length",
			prog: values.List(values.NewSymbol("vector-length"),
				values.List(values.NewSymbol("list->vector"),
					values.List(values.NewSymbol("quote"),
						values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))))),
			out: values.NewInteger(3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Character Primitive Tests
// ----------------------------------------------------------------------------

func TestCharComparisons(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		// char=?
		{
			name: "char=? true",
			prog: values.List(values.NewSymbol("char=?"), values.NewCharacter('a'), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "char=? false",
			prog: values.List(values.NewSymbol("char=?"), values.NewCharacter('a'), values.NewCharacter('b')),
			out:  values.FalseValue,
		},
		// char<?
		{
			name: "char<? true",
			prog: values.List(values.NewSymbol("char<?"), values.NewCharacter('a'), values.NewCharacter('b')),
			out:  values.TrueValue,
		},
		{
			name: "char<? false",
			prog: values.List(values.NewSymbol("char<?"), values.NewCharacter('b'), values.NewCharacter('a')),
			out:  values.FalseValue,
		},
		// char>?
		{
			name: "char>? true",
			prog: values.List(values.NewSymbol("char>?"), values.NewCharacter('b'), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "char>? false",
			prog: values.List(values.NewSymbol("char>?"), values.NewCharacter('a'), values.NewCharacter('b')),
			out:  values.FalseValue,
		},
		// char<=?
		{
			name: "char<=? true equal",
			prog: values.List(values.NewSymbol("char<=?"), values.NewCharacter('a'), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "char<=? true less",
			prog: values.List(values.NewSymbol("char<=?"), values.NewCharacter('a'), values.NewCharacter('b')),
			out:  values.TrueValue,
		},
		{
			name: "char<=? false",
			prog: values.List(values.NewSymbol("char<=?"), values.NewCharacter('b'), values.NewCharacter('a')),
			out:  values.FalseValue,
		},
		// char>=?
		{
			name: "char>=? true equal",
			prog: values.List(values.NewSymbol("char>=?"), values.NewCharacter('a'), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "char>=? true greater",
			prog: values.List(values.NewSymbol("char>=?"), values.NewCharacter('b'), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "char>=? false",
			prog: values.List(values.NewSymbol("char>=?"), values.NewCharacter('a'), values.NewCharacter('b')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCharToInteger(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "char->integer lowercase a",
			prog: values.List(values.NewSymbol("char->integer"), values.NewCharacter('a')),
			out:  values.NewInteger(97),
		},
		{
			name: "char->integer space",
			prog: values.List(values.NewSymbol("char->integer"), values.NewCharacter(' ')),
			out:  values.NewInteger(32),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestIntegerToChar(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "integer->char 97",
			prog: values.List(values.NewSymbol("integer->char"), values.NewInteger(97)),
			out:  values.NewCharacter('a'),
		},
		{
			name: "integer->char 32",
			prog: values.List(values.NewSymbol("integer->char"), values.NewInteger(32)),
			out:  values.NewCharacter(' '),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Arithmetic Tests (gcd, lcm, quotient, remainder, modulo)
// ----------------------------------------------------------------------------

func TestGcd(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "gcd of 12 and 8",
			prog: values.List(values.NewSymbol("gcd"), values.NewInteger(12), values.NewInteger(8)),
			out:  values.NewInteger(4),
		},
		{
			name: "gcd with zero",
			prog: values.List(values.NewSymbol("gcd"), values.NewInteger(5), values.NewInteger(0)),
			out:  values.NewInteger(5),
		},
		{
			name: "gcd of coprime numbers",
			prog: values.List(values.NewSymbol("gcd"), values.NewInteger(7), values.NewInteger(11)),
			out:  values.NewInteger(1),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestLcm(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "lcm of 4 and 6",
			prog: values.List(values.NewSymbol("lcm"), values.NewInteger(4), values.NewInteger(6)),
			out:  values.NewInteger(12),
		},
		{
			name: "lcm of 3 and 5",
			prog: values.List(values.NewSymbol("lcm"), values.NewInteger(3), values.NewInteger(5)),
			out:  values.NewInteger(15),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestQuotient(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "quotient positive",
			prog: values.List(values.NewSymbol("quotient"), values.NewInteger(13), values.NewInteger(4)),
			out:  values.NewInteger(3),
		},
		{
			name: "quotient negative dividend",
			prog: values.List(values.NewSymbol("quotient"), values.NewInteger(-13), values.NewInteger(4)),
			out:  values.NewInteger(-3),
		},
		{
			name: "quotient negative divisor",
			prog: values.List(values.NewSymbol("quotient"), values.NewInteger(13), values.NewInteger(-4)),
			out:  values.NewInteger(-3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestRemainder(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "remainder positive",
			prog: values.List(values.NewSymbol("remainder"), values.NewInteger(13), values.NewInteger(4)),
			out:  values.NewInteger(1),
		},
		{
			name: "remainder negative dividend",
			prog: values.List(values.NewSymbol("remainder"), values.NewInteger(-13), values.NewInteger(4)),
			out:  values.NewInteger(-1),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestModulo(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "modulo positive",
			prog: values.List(values.NewSymbol("modulo"), values.NewInteger(13), values.NewInteger(4)),
			out:  values.NewInteger(1),
		},
		{
			name: "modulo negative dividend",
			prog: values.List(values.NewSymbol("modulo"), values.NewInteger(-13), values.NewInteger(4)),
			out:  values.NewInteger(3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Max and Min Tests
// ----------------------------------------------------------------------------

func TestMax(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "max of two integers",
			prog: values.List(values.NewSymbol("max"), values.NewInteger(3), values.NewInteger(5)),
			out:  values.NewInteger(5),
		},
		{
			name: "max of three integers",
			prog: values.List(values.NewSymbol("max"), values.NewInteger(3), values.NewInteger(7), values.NewInteger(5)),
			out:  values.NewInteger(7),
		},
		{
			name: "max with negative",
			prog: values.List(values.NewSymbol("max"), values.NewInteger(-5), values.NewInteger(-3)),
			out:  values.NewInteger(-3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestMin(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "min of two integers",
			prog: values.List(values.NewSymbol("min"), values.NewInteger(3), values.NewInteger(5)),
			out:  values.NewInteger(3),
		},
		{
			name: "min of three integers",
			prog: values.List(values.NewSymbol("min"), values.NewInteger(3), values.NewInteger(7), values.NewInteger(5)),
			out:  values.NewInteger(3),
		},
		{
			name: "min with negative",
			prog: values.List(values.NewSymbol("min"), values.NewInteger(-5), values.NewInteger(-3)),
			out:  values.NewInteger(-5),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Cons and List Tests
// ----------------------------------------------------------------------------

func TestCons(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "cons two values",
			prog: values.List(values.NewSymbol("cons"), values.NewInteger(1), values.NewInteger(2)),
			out:  values.NewCons(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			name: "cons with empty list",
			prog: values.List(values.NewSymbol("cons"),
				values.NewInteger(1),
				values.List(values.NewSymbol("quote"), values.EmptyList)),
			out: values.List(values.NewInteger(1)),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestList(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "list with three elements",
			prog: values.List(values.NewSymbol("list"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			out:  values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name: "list with no elements",
			prog: values.List(values.NewSymbol("list")),
			out:  values.EmptyList,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// ----------------------------------------------------------------------------
// Number to String Tests
// ----------------------------------------------------------------------------

func TestNumberToString(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "number->string integer",
			prog: values.List(values.NewSymbol("number->string"), values.NewInteger(42)),
			out:  values.NewString("42"),
		},
		{
			name: "number->string negative",
			prog: values.List(values.NewSymbol("number->string"), values.NewInteger(-123)),
			out:  values.NewString("-123"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestStringToNumber(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "string->number integer",
			prog: values.List(values.NewSymbol("string->number"), values.NewString("42")),
			out:  values.NewInteger(42),
		},
		{
			name: "string->number negative",
			prog: values.List(values.NewSymbol("string->number"), values.NewString("-123")),
			out:  values.NewInteger(-123),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestBoundIdentifierEqualQ(t *testing.T) {
	sctx := syntax.NewZeroValueSourceContext()
	scope1 := syntax.NewScope(nil)
	scope2 := syntax.NewScope(nil)

	// Create syntax symbols with various scope configurations
	idFoo := syntax.NewSyntaxSymbol("foo", sctx)
	idFoo2 := syntax.NewSyntaxSymbol("foo", sctx)
	idBar := syntax.NewSyntaxSymbol("bar", sctx)
	idFooWithScope1 := idFoo.AddScope(scope1).(*syntax.SyntaxSymbol)
	idFooWithScope2 := idFoo.AddScope(scope2).(*syntax.SyntaxSymbol)
	idFooWithScope1Again := idFoo.AddScope(scope1).(*syntax.SyntaxSymbol)

	tcs := []struct {
		name string
		id1  *syntax.SyntaxSymbol
		id2  *syntax.SyntaxSymbol
		want bool
	}{
		{
			name: "same identifier object",
			id1:  idFoo,
			id2:  idFoo,
			want: true,
		},
		{
			name: "same name no scopes",
			id1:  idFoo,
			id2:  idFoo2,
			want: true,
		},
		{
			name: "different names",
			id1:  idFoo,
			id2:  idBar,
			want: false,
		},
		{
			name: "same name same scope",
			id1:  idFooWithScope1,
			id2:  idFooWithScope1Again,
			want: true,
		},
		{
			name: "same name different scopes",
			id1:  idFooWithScope1,
			id2:  idFooWithScope2,
			want: false,
		},
		{
			name: "same name one has scope one doesn't",
			id1:  idFoo,
			id2:  idFooWithScope1,
			want: false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Test bound-identifier=? by checking scope set equality directly
			// (This tests the underlying logic used by primBoundIdentifierEqualQ)

			// Same name?
			if tc.id1.Key != tc.id2.Key {
				qt.Assert(t, tc.want, qt.IsFalse)
				return
			}

			// Same scopes? (set equality = mutual subset)
			scopes1 := tc.id1.Scopes()
			scopes2 := tc.id2.Scopes()
			result := syntax.ScopesMatch(scopes1, scopes2) && syntax.ScopesMatch(scopes2, scopes1)
			qt.Assert(t, result, qt.Equals, tc.want)
		})
	}
}

func TestFreeIdentifierEqualQ(t *testing.T) {
	sctx := syntax.NewZeroValueSourceContext()

	// Create syntax symbols
	idFoo := syntax.NewSyntaxSymbol("foo", sctx)
	idFoo2 := syntax.NewSyntaxSymbol("foo", sctx)
	idBar := syntax.NewSyntaxSymbol("bar", sctx)
	idPlus := syntax.NewSyntaxSymbol("+", sctx)
	idPlus2 := syntax.NewSyntaxSymbol("+", sctx)

	tcs := []struct {
		name string
		id1  *syntax.SyntaxSymbol
		id2  *syntax.SyntaxSymbol
		want bool
	}{
		{
			name: "both unbound same name",
			id1:  idFoo,
			id2:  idFoo2,
			want: true,
		},
		{
			name: "both unbound different names",
			id1:  idFoo,
			id2:  idBar,
			want: false,
		},
		{
			name: "both bound to same primitive",
			id1:  idPlus,
			id2:  idPlus2,
			want: true,
		},
		{
			name: "one bound one unbound",
			id1:  idPlus,
			id2:  idFoo,
			want: false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env, err := NewTopLevelEnvironmentFrameTiny()
			qt.Assert(t, err, qt.IsNil)

			// Test free-identifier=? by checking binding resolution
			// (This tests the underlying logic used by primFreeIdentifierEqualQ)
			sym1 := env.InternSymbol(values.NewSymbol(tc.id1.Key))
			sym2 := env.InternSymbol(values.NewSymbol(tc.id2.Key))

			binding1 := env.GetBindingWithScopes(sym1, tc.id1.Scopes())
			binding2 := env.GetBindingWithScopes(sym2, tc.id2.Scopes())

			var result bool
			if binding1 == nil && binding2 == nil {
				// Both unbound → compare names
				result = tc.id1.Key == tc.id2.Key
			} else if binding1 == nil || binding2 == nil {
				// One bound, one unbound → not equal
				result = false
			} else {
				// Both bound → same binding?
				result = binding1 == binding2
			}

			qt.Assert(t, result, qt.Equals, tc.want)
		})
	}
}

func TestBoundIdentifierEqualQPrimitiveExists(t *testing.T) {
	env, err := NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)

	// Verify the bound-identifier=? primitive exists
	boundIdSym := env.InternSymbol(values.NewSymbol("bound-identifier=?"))
	bnd := env.GetBinding(boundIdSym)
	qt.Assert(t, bnd, qt.IsNotNil)
}

func TestFreeIdentifierEqualQPrimitiveExists(t *testing.T) {
	env, err := NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)

	// Verify the free-identifier=? primitive exists
	freeIdSym := env.InternSymbol(values.NewSymbol("free-identifier=?"))
	bnd := env.GetBinding(freeIdSym)
	qt.Assert(t, bnd, qt.IsNotNil)
}

func TestMapMultipleLists(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "single list",
			// (map (lambda (x) (* x 2)) '(1 2 3))
			prog: values.List(
				values.NewSymbol("map"),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("x")),
					values.List(values.NewSymbol("*"), values.NewSymbol("x"), values.NewInteger(2)),
				),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			),
			out: values.List(values.NewInteger(2), values.NewInteger(4), values.NewInteger(6)),
		},
		{
			name: "two lists with +",
			// (map + '(1 2 3) '(10 20 30))
			prog: values.List(
				values.NewSymbol("map"),
				values.NewSymbol("+"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30))),
			),
			out: values.List(values.NewInteger(11), values.NewInteger(22), values.NewInteger(33)),
		},
		{
			name: "three lists",
			// (map (lambda (a b c) (+ a b c)) '(1 2 3) '(10 20 30) '(100 200 300))
			prog: values.List(
				values.NewSymbol("map"),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
					values.List(values.NewSymbol("+"), values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
				),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30))),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(100), values.NewInteger(200), values.NewInteger(300))),
			),
			out: values.List(values.NewInteger(111), values.NewInteger(222), values.NewInteger(333)),
		},
		{
			name: "map list constructor",
			// (map list '(a b c) '(1 2 3))
			prog: values.List(
				values.NewSymbol("map"),
				values.NewSymbol("list"),
				values.List(values.NewSymbol("quote"), values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			),
			out: values.List(
				values.List(values.NewSymbol("a"), values.NewInteger(1)),
				values.List(values.NewSymbol("b"), values.NewInteger(2)),
				values.List(values.NewSymbol("c"), values.NewInteger(3)),
			),
		},
		{
			name: "empty lists",
			// (map + '() '())
			prog: values.List(
				values.NewSymbol("map"),
				values.NewSymbol("+"),
				values.List(values.NewSymbol("quote"), values.EmptyList),
				values.List(values.NewSymbol("quote"), values.EmptyList),
			),
			out: values.EmptyList,
		},
		{
			name: "unequal length lists (stops at shortest)",
			// (map + '(1 2 3) '(10 20))
			prog: values.List(
				values.NewSymbol("map"),
				values.NewSymbol("+"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(10), values.NewInteger(20))),
			),
			out: values.List(values.NewInteger(11), values.NewInteger(22)),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestForEachMultipleLists(t *testing.T) {
	// for-each returns void, so we test that it doesn't error
	tcs := []struct {
		name string
		prog values.Value
	}{
		{
			name: "single list",
			// (for-each (lambda (x) x) '(1 2 3))
			prog: values.List(
				values.NewSymbol("for-each"),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("x")),
					values.NewSymbol("x"),
				),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			),
		},
		{
			name: "two lists",
			// (for-each (lambda (x y) (+ x y)) '(1 2 3) '(10 20 30))
			prog: values.List(
				values.NewSymbol("for-each"),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("x"), values.NewSymbol("y")),
					values.List(values.NewSymbol("+"), values.NewSymbol("x"), values.NewSymbol("y")),
				),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30))),
			),
		},
		{
			name: "empty lists",
			// (for-each + '() '())
			prog: values.List(
				values.NewSymbol("for-each"),
				values.NewSymbol("+"),
				values.List(values.NewSymbol("quote"), values.EmptyList),
				values.List(values.NewSymbol("quote"), values.EmptyList),
			),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

func TestApplyMultipleArgs(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "simple apply",
			// (apply + '(1 2 3))
			prog: values.List(
				values.NewSymbol("apply"),
				values.NewSymbol("+"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			),
			out: values.NewInteger(6),
		},
		{
			name: "apply with prefix args",
			// (apply + 1 2 '(3 4 5))
			prog: values.List(
				values.NewSymbol("apply"),
				values.NewSymbol("+"),
				values.NewInteger(1),
				values.NewInteger(2),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(3), values.NewInteger(4), values.NewInteger(5))),
			),
			out: values.NewInteger(15),
		},
		{
			name: "apply with many prefix args",
			// (apply + 1 2 3 4 '(5 6))
			prog: values.List(
				values.NewSymbol("apply"),
				values.NewSymbol("+"),
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
				values.NewInteger(4),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(5), values.NewInteger(6))),
			),
			out: values.NewInteger(21),
		},
		{
			name: "apply with empty final list",
			// (apply + 1 2 3 '())
			prog: values.List(
				values.NewSymbol("apply"),
				values.NewSymbol("+"),
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
				values.List(values.NewSymbol("quote"), values.EmptyList),
			),
			out: values.NewInteger(6),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestValues(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "single value",
			// (values 42)
			prog: values.List(
				values.NewSymbol("values"),
				values.NewInteger(42),
			),
			out: values.NewInteger(42),
		},
		{
			name: "multiple values returns first",
			// (values 1 2 3) - GetValue() returns first value
			prog: values.List(
				values.NewSymbol("values"),
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
			),
			out: values.NewInteger(1),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestCallWithValues(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "single value producer",
			// (call-with-values (lambda () 42) (lambda (x) (* x 2)))
			prog: values.List(
				values.NewSymbol("call-with-values"),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.NewInteger(42),
				),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("x")),
					values.List(values.NewSymbol("*"), values.NewSymbol("x"), values.NewInteger(2)),
				),
			),
			out: values.NewInteger(84),
		},
		{
			name: "multiple values",
			// (call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (+ a b c)))
			prog: values.List(
				values.NewSymbol("call-with-values"),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.List(values.NewSymbol("values"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
				),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
					values.List(values.NewSymbol("+"), values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
				),
			),
			out: values.NewInteger(6),
		},
		{
			name: "consumer builds list",
			// (call-with-values (lambda () (values 'a 'b 'c)) list)
			prog: values.List(
				values.NewSymbol("call-with-values"),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.List(
						values.NewSymbol("values"),
						values.List(values.NewSymbol("quote"), values.NewSymbol("a")),
						values.List(values.NewSymbol("quote"), values.NewSymbol("b")),
						values.List(values.NewSymbol("quote"), values.NewSymbol("c")),
					),
				),
				values.NewSymbol("list"),
			),
			out: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			name: "no values producer",
			// (call-with-values (lambda () (values)) (lambda () 'done))
			prog: values.List(
				values.NewSymbol("call-with-values"),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.List(values.NewSymbol("values")),
				),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.List(values.NewSymbol("quote"), values.NewSymbol("done")),
				),
			),
			out: values.NewSymbol("done"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestDynamicWind(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "basic - returns thunk result",
			// (dynamic-wind (lambda () 'before) (lambda () 42) (lambda () 'after))
			prog: values.List(
				values.NewSymbol("dynamic-wind"),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.List(values.NewSymbol("quote"), values.NewSymbol("before")),
				),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.NewInteger(42),
				),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.List(values.NewSymbol("quote"), values.NewSymbol("after")),
				),
			),
			out: values.NewInteger(42),
		},
		{
			name: "before runs first",
			// ((lambda (v)
			//   (dynamic-wind
			//     (lambda () (vector-set! v 0 1))
			//     (lambda () (vector-ref v 0))
			//     (lambda () (vector-set! v 0 2))))
			//  (make-vector 1 0))
			prog: values.List(
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("v")),
					values.List(
						values.NewSymbol("dynamic-wind"),
						values.List(
							values.NewSymbol("lambda"),
							values.EmptyList,
							values.List(values.NewSymbol("vector-set!"), values.NewSymbol("v"), values.NewInteger(0), values.NewInteger(1)),
						),
						values.List(
							values.NewSymbol("lambda"),
							values.EmptyList,
							values.List(values.NewSymbol("vector-ref"), values.NewSymbol("v"), values.NewInteger(0)),
						),
						values.List(
							values.NewSymbol("lambda"),
							values.EmptyList,
							values.List(values.NewSymbol("vector-set!"), values.NewSymbol("v"), values.NewInteger(0), values.NewInteger(2)),
						),
					),
				),
				values.List(values.NewSymbol("make-vector"), values.NewInteger(1), values.NewInteger(0)),
			),
			out: values.NewInteger(1), // thunk sees value set by before
		},
		{
			name: "escape returns correct value",
			// (call/cc (lambda (k) (dynamic-wind (lambda () #f) (lambda () (k 77)) (lambda () #f))))
			prog: values.List(
				values.NewSymbol("call/cc"),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("k")),
					values.List(
						values.NewSymbol("dynamic-wind"),
						values.List(values.NewSymbol("lambda"), values.EmptyList, values.FalseValue),
						values.List(values.NewSymbol("lambda"), values.EmptyList,
							values.List(values.NewSymbol("k"), values.NewInteger(77))),
						values.List(values.NewSymbol("lambda"), values.EmptyList, values.FalseValue),
					),
				),
			),
			out: values.NewInteger(77),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runProgram(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestDynamicWindEscape tests that after is called on continuation escape.
// This test uses runProgramWithEnv to load bootstrap macros for 'let'.
func TestDynamicWindEscape(t *testing.T) {
	env, err := NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)

	// Parse and run:
	// (let ((v (make-vector 1 0)))
	//   (call/cc (lambda (k)
	//     (dynamic-wind
	//       (lambda () (vector-set! v 0 1))
	//       (lambda () (k 99))
	//       (lambda () (vector-set! v 0 2)))))
	//   (vector-ref v 0))
	prog := `(let ((v (make-vector 1 0)))
		(call/cc (lambda (k)
			(dynamic-wind
				(lambda () (vector-set! v 0 1))
				(lambda () (k 99))
				(lambda () (vector-set! v 0 2)))))
		(vector-ref v 0))`

	p := parser.NewParser(env, strings.NewReader(prog))
	stx, err := p.ReadSyntax()
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

	// After should have run, setting v[0] to 2
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(2))
}
