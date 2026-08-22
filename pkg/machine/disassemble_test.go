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
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestDisassemble_Simple(t *testing.T) {
	// Build a template with LoadLiteral(0) + Push + RestoreContinuation
	// and a literal pool containing 42.
	tpl := NewNativeTemplate(1, 0, false)
	tpl.SetName("test-proc")
	litIdx := tpl.MaybeAppendLiteral(values.NewInteger(42))

	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: int32(litIdx)})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpRestoreContinuation})

	dt := Disassemble(tpl)

	qt.Assert(t, dt.Name, qt.Equals, "test-proc")
	qt.Assert(t, dt.ParamCount, qt.Equals, 1)
	qt.Assert(t, dt.IsVariadic, qt.Equals, false)
	qt.Assert(t, dt.Literals, qt.HasLen, 1)
	qt.Assert(t, dt.Literals[0], qt.Equals, "42")
	qt.Assert(t, dt.Instructions, qt.HasLen, 3)

	// LoadLiteral at PC 0.
	di := dt.Instructions[0]
	qt.Assert(t, di.PC, qt.Equals, 0)
	qt.Assert(t, di.Op, qt.Equals, "LoadLiteral")
	qt.Assert(t, di.Arg, qt.Equals, int32(0))
	qt.Assert(t, di.Literal, qt.Equals, "42")
	qt.Assert(t, di.Target, qt.Equals, -1)
	qt.Assert(t, di.Slot, qt.Equals, -1)

	// Push at PC 1.
	di = dt.Instructions[1]
	qt.Assert(t, di.Op, qt.Equals, "Push")

	// RestoreContinuation at PC 2.
	di = dt.Instructions[2]
	qt.Assert(t, di.Op, qt.Equals, "RestoreContinuation")
}

func TestDisassemble_LocalIndex(t *testing.T) {
	tpl := NewNativeTemplate(2, 0, false)

	// Encode slot=3, depth=1.
	li := environment.NewLocalIndex(3, 1)
	arg := EncodeLocalIndex(li)
	tpl.AppendInstruction(Instruction{Op: OpLoadLocal, Arg: arg})

	dt := Disassemble(tpl)
	qt.Assert(t, dt.Instructions, qt.HasLen, 1)

	di := dt.Instructions[0]
	qt.Assert(t, di.Op, qt.Equals, "LoadLocal")
	qt.Assert(t, di.Slot, qt.Equals, 3)
	qt.Assert(t, di.Depth, qt.Equals, 1)
	qt.Assert(t, di.Literal, qt.Equals, "")
	qt.Assert(t, di.Target, qt.Equals, -1)
}

func TestDisassemble_Branch(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// BranchOnFalseValue at PC 0 with relative offset +3 -> target PC 3.
	tpl.AppendInstruction(Instruction{Op: OpBranchOnFalseValue, Arg: 3})
	// Filler instructions at PC 1, 2.
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	// Branch at PC 3 with relative offset -2 -> target PC 1.
	tpl.AppendInstruction(Instruction{Op: OpBranch, Arg: -2})

	dt := Disassemble(tpl)
	qt.Assert(t, dt.Instructions, qt.HasLen, 4)

	// BranchOnFalseValue -> target 3.
	qt.Assert(t, dt.Instructions[0].Target, qt.Equals, 3)
	qt.Assert(t, dt.Instructions[0].Op, qt.Equals, "BranchOnFalseValue")

	// Branch -> target 1.
	qt.Assert(t, dt.Instructions[3].Target, qt.Equals, 1)
	qt.Assert(t, dt.Instructions[3].Op, qt.Equals, "Branch")

	// Non-branch ops have Target == -1.
	qt.Assert(t, dt.Instructions[1].Target, qt.Equals, -1)
}

func TestDisassemble_CachedBinding(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// Create a ForeignClosure binding with a name.
	env := environment.NewNamespace().Runtime()
	fc := NewForeignClosure(env, 1, false, func(_ CallContext) error {
		return nil
	})
	fc.SetName("my-prim")
	bd := environment.NewBinding(fc, environment.BindingTypeVariable)
	bindIdx := tpl.AppendCachedBinding(bd)

	tpl.AppendInstruction(Instruction{Op: OpLoadCachedBinding, Arg: bindIdx})

	dt := Disassemble(tpl)
	qt.Assert(t, dt.Instructions, qt.HasLen, 1)
	qt.Assert(t, dt.Instructions[0].Binding, qt.Equals, "my-prim")
	qt.Assert(t, dt.Bindings, qt.HasLen, 1)
	qt.Assert(t, dt.Bindings[0], qt.Equals, "my-prim")
}

func TestDisassemble_SideTable(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	scm := NewOperationSetContMark()
	instr := tpl.AppendSideTableOp(scm)
	tpl.code = append(tpl.code, instr)
	tpl.sourceTableRefs = append(tpl.sourceTableRefs, 0)

	dt := Disassemble(tpl)
	qt.Assert(t, dt.Instructions, qt.HasLen, 1)
	qt.Assert(t, dt.Instructions[0].Op, qt.Equals, "Complex")
	qt.Assert(t, dt.Instructions[0].SideOp, qt.Not(qt.Equals), "")
	qt.Assert(t, strings.Contains(dt.Instructions[0].SideOp, "cont-mark"), qt.IsTrue)
}

func TestDisassemble_MakeClosureAnnotation(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	// Create a sub-template to serve as the closure's code.
	subTpl := NewNativeTemplate(1, 0, false)
	subTpl.SetName("inner-fn")
	litIdx := tpl.MaybeAppendLiteral(subTpl)

	// PushLiteral(subTemplate) + MakeClosure.
	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx)})
	tpl.AppendInstruction(Instruction{Op: OpMakeClosure})

	dt := Disassemble(tpl)
	qt.Assert(t, dt.Instructions, qt.HasLen, 2)

	// The PushLiteral should show the template's SchemeString.
	qt.Assert(t, dt.Instructions[0].Op, qt.Equals, "PushLiteral")
	qt.Assert(t, dt.Instructions[0].Literal, qt.Equals, "#<native-template>")

	// MakeClosure should annotate with the lambda name.
	qt.Assert(t, dt.Instructions[1].Op, qt.Equals, "MakeClosure")
	qt.Assert(t, dt.Instructions[1].Literal, qt.Equals, "<lambda:inner-fn>")
}

func TestDisassembleString_Header(t *testing.T) {
	tpl := NewNativeTemplate(2, 0, true)
	tpl.SetName("my-func")
	tpl.SetDoc("A test function")
	tpl.MaybeAppendLiteral(values.NewInteger(99))
	tpl.AppendInstruction(Instruction{Op: OpRestoreContinuation})

	s := DisassembleString(tpl)

	qt.Assert(t, strings.Contains(s, "my-func"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "params: 2"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "variadic: true"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "A test function"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "literals: [99]"), qt.IsTrue)
}

func TestDisassembleString_BranchTarget(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpBranchOnFalseValue, Arg: 5})
	tpl.AppendInstruction(Instruction{Op: OpPush})

	s := DisassembleString(tpl)

	// The arrow character and target PC should appear.
	qt.Assert(t, strings.Contains(s, "\u21925"), qt.IsTrue)
}

func TestDisassemble_SourceLocation(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)

	src := syntax.NewSourceContext("(+ 1 2)", "test.scm",
		syntax.NewSourceIndexes(0, 0, 5),
		syntax.NewSourceIndexes(7, 7, 5))

	tpl.AppendInstructionWithSource(src, Instruction{Op: OpPush})

	dt := Disassemble(tpl)
	qt.Assert(t, dt.Instructions, qt.HasLen, 1)
	qt.Assert(t, dt.Instructions[0].Source, qt.Equals, "test.scm:5:0")
}
