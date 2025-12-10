package primitives

import (
	"bufio"
	"context"
	"errors"
	"io"
	"os"

	"wile/machine"
	"wile/parser"
	"wile/values"
)

// PrimLoad implements the (load) primitive.
// Loads and evaluates a Scheme source file.
func PrimLoad(ctx context.Context, mc *machine.MachineContext) error {
	filenameVal := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	filename, ok := filenameVal.(*values.String)
	if !ok {
		return values.WrapForeignErrorf(values.ErrNotAString, "load: expected a string but got %T", filenameVal)
	}

	// Open the file
	f, err := os.Open(filename.Value)
	if err != nil {
		return values.WrapForeignErrorf(err, "load: cannot open file %s", filename.Value)
	}
	defer f.Close()

	// Use the current top-level environment
	env := mc.EnvironmentFrame().TopLevel()

	// Create parser (which creates its own tokenizer internally)
	rdr := bufio.NewReader(f)
	p := parser.NewParser(env, rdr)

	// Read and evaluate each expression
	var lastValue values.Value = values.Void
	for {
		stx, err := p.ReadSyntax()
		if err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			return values.WrapForeignErrorf(err, "load: parse error in %s", filename.Value)
		}

		// Expand the expression
		ectx := machine.NewExpandTimeCallContext()
		expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
		if err != nil {
			return values.WrapForeignErrorf(err, "load: expansion error in %s", filename.Value)
		}

		// Compile the expression
		tpl := machine.NewNativeTemplate(0, 0, false)
		cctx := machine.NewCompileTimeCallContext(false, true, env)
		err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
		if err != nil {
			return values.WrapForeignErrorf(err, "load: compilation error in %s", filename.Value)
		}

		// Run the compiled code
		cont := machine.NewMachineContinuation(nil, tpl, env)
		sub := machine.NewMachineContext(cont)
		if err := sub.Run(ctx); err != nil {
			var escapeErr *machine.ErrContinuationEscape
			if errors.As(err, &escapeErr) {
				return err
			}
			if !errors.Is(err, machine.ErrMachineHalt) {
				return values.WrapForeignErrorf(err, "load: runtime error in %s", filename.Value)
			}
		}

		lastValue = sub.GetValue()
	}

	mc.SetValue(lastValue)
	return nil
}
