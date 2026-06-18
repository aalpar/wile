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
	"fmt"
	"strings"

	"github.com/aalpar/wile/pkg/environment"
)

// DisassembledInstruction holds the annotation for a single bytecode instruction.
type DisassembledInstruction struct {
	PC      int
	Op      string
	Arg     int32
	HasArg  bool   // true when Arg is meaningful (distinguishes unused from Arg=0)
	Slot    int    // decoded slot for local ops; -1 when not applicable
	Depth   int    // decoded depth for local ops; -1 when not applicable
	Target  int    // absolute PC for branch/save ops; -1 when not applicable
	Literal string // SchemeString() of resolved literal; "" when not applicable
	Binding string // name of cached binding's value; "" when not applicable
	SideOp  string // String() of InlinedOperation; "" when not applicable
	Source  string // "file:line:col" or ""
}

// DisassembledTemplate holds the annotated disassembly of a NativeTemplate.
type DisassembledTemplate struct {
	Name         string
	ParamCount   int
	IsVariadic   bool
	Doc          string
	Literals     []string // SchemeString() of each literal
	Bindings     []string // name of each cached binding
	Instructions []DisassembledInstruction
}

// named is the interface for values that have a Name() method.
type named interface {
	Name() string
}

// Disassemble walks the template's bytecode and produces a DisassembledTemplate
// with annotations resolved per opcode category.
func Disassemble(tpl *NativeTemplate) DisassembledTemplate {
	code := tpl.Code()
	literals := tpl.Literals()
	bindings := tpl.CachedBindings()
	sideTable := tpl.SideTable()

	q := DisassembledTemplate{
		Name:       tpl.Name(),
		ParamCount: tpl.ParameterCount(),
		IsVariadic: tpl.IsVariadic(),
		Doc:        tpl.Doc(),
	}

	// Populate Literals summary.
	q.Literals = make([]string, len(literals))
	for i, lit := range literals {
		q.Literals[i] = lit.SchemeString()
	}

	// Populate Bindings summary.
	q.Bindings = make([]string, len(bindings))
	for i, bd := range bindings {
		q.Bindings[i] = bindingName(bd)
	}

	// Walk instructions.
	q.Instructions = make([]DisassembledInstruction, len(code))
	for pc, instr := range code {
		di := DisassembledInstruction{
			PC:     pc,
			Op:     instr.Op.String(),
			Arg:    instr.Arg,
			HasArg: opcodeHasArg(instr.Op),
			Slot:   -1,
			Depth:  -1,
			Target: -1,
		}

		// Annotate based on operand kind from opcode metadata.
		switch opcodeTable[instr.Op].operandKind {
		case OperandLiteralIdx:
			idx := int(instr.Arg)
			if idx >= 0 && idx < len(literals) {
				di.Literal = literals[idx].SchemeString()
			} else {
				di.Literal = fmt.Sprintf("<invalid-literal-index:%d>", idx)
			}

		case OperandLocalIdx:
			slot, depth := DecodeLocalIndex(instr.Arg)
			di.Slot = slot
			di.Depth = depth

		case OperandBranchOffset:
			di.Target = pc + int(instr.Arg)

		case OperandCachedBinding:
			idx := int(instr.Arg)
			if idx >= 0 && idx < len(bindings) {
				di.Binding = bindingName(bindings[idx])
			} else {
				di.Binding = fmt.Sprintf("<invalid-binding-index:%d>", idx)
			}

		case OperandSideTable:
			idx := int(instr.Arg)
			if idx >= 0 && idx < len(sideTable) {
				di.SideOp = fmt.Sprint(sideTable[idx])
			} else {
				di.SideOp = fmt.Sprintf("<invalid-sidetable-index:%d>", idx)
			}

		case OperandNone, OperandRaw:
			// No special annotation needed.
		}

		// Special-case: MakeClosure annotates with the preceding literal's template name.
		if instr.Op == OpMakeClosure {
			di.Literal = makeClosureAnnotation(pc, code, literals)
		}

		// Source location.
		src := tpl.SourceAt(pc)
		if src != nil && src.File != "" {
			di.Source = fmt.Sprintf("%s:%d:%d",
				src.File, src.Start.Line(), src.Start.Column())
		}

		q.Instructions[pc] = di
	}

	return q
}

// bindingName extracts a human-readable name from a binding's value.
// It checks for the named interface (Name() string) first, falling back
// to SchemeString() on the value.
func bindingName(bd *environment.Binding) string {
	if bd == nil {
		return "<nil>"
	}
	v := bd.Value()
	if v == nil {
		return "<unset>"
	}
	n, ok := v.(named)
	if ok && n.Name() != "" {
		return n.Name()
	}
	return v.SchemeString()
}

// makeClosureAnnotation checks whether the instruction preceding MakeClosure
// loaded a NativeTemplate literal, and if so returns "<lambda:name>".
func makeClosureAnnotation(pc int, code []Instruction, literals MultipleValues) string {
	if pc == 0 {
		return ""
	}
	prev := code[pc-1]
	var litIdx int
	switch prev.Op {
	case OpLoadLiteral, OpPushLiteral:
		litIdx = int(prev.Arg)
	default:
		return ""
	}
	if litIdx < 0 || litIdx >= len(literals) {
		return ""
	}
	tpl, ok := literals[litIdx].(*NativeTemplate)
	if !ok {
		return ""
	}
	name := tpl.Name()
	if name == "" {
		return "<lambda>"
	}
	return "<lambda:" + name + ">"
}

// opcodeHasArg returns true for opcodes where Arg=0 is still meaningful
// and should be printed (as opposed to zero-operand ops where Arg is unused).
func opcodeHasArg(op OpCode) bool {
	return opcodeTable[op].operandKind != OperandNone
}

// formatDetail builds the DETAIL column string from a DisassembledInstruction.
func formatDetail(di DisassembledInstruction) string {
	var parts []string

	if di.Slot >= 0 {
		parts = append(parts, fmt.Sprintf("slot=%d depth=%d", di.Slot, di.Depth))
	}
	if di.Target >= 0 {
		parts = append(parts, fmt.Sprintf("\u2192%d", di.Target))
	}
	if di.Literal != "" {
		parts = append(parts, di.Literal)
	}
	if di.Binding != "" {
		parts = append(parts, di.Binding)
	}
	if di.SideOp != "" {
		parts = append(parts, di.SideOp)
	}

	return strings.Join(parts, "  ")
}

// DisassembleString produces a columnar human-readable listing from a NativeTemplate.
func DisassembleString(tpl *NativeTemplate) string {
	dt := Disassemble(tpl)

	var b strings.Builder

	// Header.
	name := dt.Name
	if name == "" {
		name = "<anonymous>"
	}
	fmt.Fprintf(&b, "%s  (params: %d, variadic: %v)\n", name, dt.ParamCount, dt.IsVariadic)
	if dt.Doc != "" {
		fmt.Fprintf(&b, "doc: %q\n", dt.Doc)
	}
	if len(dt.Literals) > 0 {
		fmt.Fprintf(&b, "literals: [%s]\n", strings.Join(dt.Literals, ", "))
	}
	if len(dt.Bindings) > 0 {
		fmt.Fprintf(&b, "bindings: [%s]\n", strings.Join(dt.Bindings, ", "))
	}
	b.WriteString("\n")

	// Column header.
	fmt.Fprintf(&b, "  %-4s %-22s %5s   %-24s %s\n",
		"PC", "OP", "ARG", "DETAIL", "SOURCE")

	// Instructions.
	for _, di := range dt.Instructions {
		argStr := ""
		if di.HasArg {
			argStr = fmt.Sprintf("%d", di.Arg)
		}
		detail := formatDetail(di)
		source := di.Source

		fmt.Fprintf(&b, "  %-4d %-22s %5s   %-24s %s\n",
			di.PC, di.Op, argStr, detail, source)
	}

	return b.String()
}
