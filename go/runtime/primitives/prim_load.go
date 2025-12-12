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
	defer f.Close() //nolint:errcheck

	// Use the current top-level environment
	env := mc.EnvironmentFrame().TopLevel()

	// Create parser (which creates its own tokenizer internally)
	rdr := bufio.NewReader(f)
	p := parser.NewParser(env, rdr)

	// Read and evaluate each expression
	var lastValue values.Value = values.Void
	for {
		stx, err := p.ReadSyntax(nil)
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
