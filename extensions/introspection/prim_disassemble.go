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

package introspection

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimDisassemble implements the (disassemble proc) primitive.
// Returns structured disassembly of a procedure as a list of alists.
func PrimDisassemble(mc *machine.MachineContext) error {
	arg := mc.Arg(0)

	switch c := arg.(type) {
	case *machine.MachineClosure:
		mc.SetValue(disassembleMachineClosure(c))
		return nil
	case *machine.CaseLambdaClosure:
		mc.SetValue(disassembleCaseLambda(c))
		return nil
	case *machine.ForeignClosure:
		mc.SetValue(disassembleForeign(c))
		return nil
	default:
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"disassemble: expected a procedure, got %s", arg.SchemeString())
	}
}

// disassembleMachineClosure disassembles a native closure into Scheme data.
func disassembleMachineClosure(c *machine.MachineClosure) values.Value {
	dis := machine.Disassemble(c.Template())
	return templateToScheme(dis)
}

// disassembleCaseLambda disassembles a case-lambda closure into Scheme data.
// Returns an alist with type and clauses keys.
func disassembleCaseLambda(c *machine.CaseLambdaClosure) values.Value {
	clauses := c.Clauses()
	clauseVals := make([]values.Value, len(clauses))
	for i, clause := range clauses {
		dis := machine.Disassemble(clause.Template())
		clauseVals[i] = templateToScheme(dis)
	}

	return values.List(
		alistEntry("type", values.NewSymbol("case-lambda-closure")),
		alistEntry("clauses", values.List(clauseVals...)),
	)
}

// disassembleForeign returns metadata for a foreign (Go-implemented) closure.
func disassembleForeign(c *machine.ForeignClosure) values.Value {
	return values.List(
		alistEntry("type", values.NewSymbol("foreign-closure")),
		alistEntry("name", values.NewString(c.Name())),
		alistEntry("params", values.NewInteger(int64(c.ParameterCount()))),
		alistEntry("variadic", values.BoolToBoolean(c.IsVariadic())),
		alistEntry("doc", values.NewString(c.Doc())),
	)
}

// templateToScheme converts a DisassembledTemplate to a Scheme list.
// The car is a header alist with metadata; the cdr contains instruction alists.
func templateToScheme(dis machine.DisassembledTemplate) values.Value {
	// Build literals vector.
	litVals := make([]values.Value, len(dis.Literals))
	for i, s := range dis.Literals {
		litVals[i] = values.NewString(s)
	}

	// Build bindings vector.
	bindVals := make([]values.Value, len(dis.Bindings))
	for i, s := range dis.Bindings {
		bindVals[i] = values.NewString(s)
	}

	header := values.List(
		alistEntry("type", values.NewSymbol("native-closure")),
		alistEntry("name", values.NewString(dis.Name)),
		alistEntry("params", values.NewInteger(int64(dis.ParamCount))),
		alistEntry("variadic", values.BoolToBoolean(dis.IsVariadic)),
		alistEntry("doc", values.NewString(dis.Doc)),
		alistEntry("literals", values.NewVector(litVals...)),
		alistEntry("bindings", values.NewVector(bindVals...)),
	)

	elems := make([]values.Value, 0, 1+len(dis.Instructions))
	elems = append(elems, header)
	for _, instr := range dis.Instructions {
		elems = append(elems, instructionToScheme(instr))
	}

	return values.List(elems...)
}

// instructionToScheme converts one DisassembledInstruction to an alist.
// Always includes pc and op; other keys are included only when meaningful.
func instructionToScheme(instr machine.DisassembledInstruction) values.Value {
	entries := make([]values.Value, 0, 10)

	entries = append(entries,
		alistEntry("pc", values.NewInteger(int64(instr.PC))),
		alistEntry("op", values.NewSymbol(instr.Op)))

	if instr.HasArg {
		entries = append(entries, alistEntry("arg", values.NewInteger(int64(instr.Arg))))
	}
	if instr.Slot >= 0 {
		entries = append(entries,
			alistEntry("slot", values.NewInteger(int64(instr.Slot))),
			alistEntry("depth", values.NewInteger(int64(instr.Depth))))
	}
	if instr.Target >= 0 {
		entries = append(entries, alistEntry("target", values.NewInteger(int64(instr.Target))))
	}
	if instr.Literal != "" {
		entries = append(entries, alistEntry("literal", values.NewString(instr.Literal)))
	}
	if instr.Binding != "" {
		entries = append(entries, alistEntry("binding", values.NewString(instr.Binding)))
	}
	if instr.SideOp != "" {
		entries = append(entries, alistEntry("side-op", values.NewString(instr.SideOp)))
	}
	if instr.Source != "" {
		entries = append(entries, alistEntry("source", values.NewString(instr.Source)))
	}

	return values.List(entries...)
}

// alistEntry creates a single (key . value) pair for use in alists.
func alistEntry(key string, val values.Value) values.Value {
	return values.NewCons(values.NewSymbol(key), val)
}
