# Disassembler Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Expose bytecode disassembly as structured Scheme alists, a Go debug string, a REPL meta-command, and an MCP tool.

**Architecture:** Core disassembly logic in `machine/disassemble.go` produces `DisassembledTemplate` from a `NativeTemplate`. The Scheme primitive in `extensions/introspection/` converts to alists. The REPL command and MCP tool use the Go string format. See `plans/2026-04-01-disassembler-design.md` for the full design.

**Tech Stack:** Go, quicktest (`qt`), existing `machine` types, `extensions/introspection`, `internal/repl`, `cmd/wile`

---

### Task 1: Add `CachedBindings()` accessor to NativeTemplate

**Files:**
- Modify: `machine/native_template.go`
- Modify: `machine/native_template_test.go`

**Step 1: Write the failing test**

Add to `machine/native_template_test.go`:

```go
func TestNativeTemplate_CachedBindings(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)

	// Empty initially
	c.Assert(tpl.CachedBindings(), qt.HasLen, 0)

	// Add a binding and verify it's accessible
	bd := environment.NewBinding(values.NewInteger(42), environment.BindingTypeVariable)
	idx := tpl.AppendCachedBinding(bd)
	c.Assert(idx, qt.Equals, int32(0))

	bindings := tpl.CachedBindings()
	c.Assert(bindings, qt.HasLen, 1)
	c.Assert(bindings[0], qt.Equals, bd)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestNativeTemplate_CachedBindings ./machine/...`
Expected: FAIL — `CachedBindings` undefined

**Step 3: Write the accessor**

Add to `machine/native_template.go`, after the `SideTable()` method (around line 583):

```go
// CachedBindings returns the compile-time resolved bindings array.
// Used by the disassembler to annotate binding references.
func (p *NativeTemplate) CachedBindings() []*environment.Binding {
	return p.cachedBindings
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestNativeTemplate_CachedBindings ./machine/...`
Expected: PASS

**Step 5: Commit**

```
add CachedBindings accessor to NativeTemplate

Exposes the cached bindings array for read-only use by the
disassembler.
```

---

### Task 2: Core Go disassembly types and Disassemble function

**Files:**
- Create: `machine/disassemble.go`
- Create: `machine/disassemble_test.go`

**Step 1: Write the test**

Create `machine/disassemble_test.go`:

```go
package machine

import (
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestDisassemble_Simple(t *testing.T) {
	c := qt.New(t)

	// Build a template: LoadLiteral 0, Push, RestoreContinuation
	// Literal pool: [42]
	tpl := NewNativeTemplate(1, 0, false)
	tpl.SetName("test-proc")
	tpl.MaybeAppendLiteral(values.NewInteger(42))
	tpl.AppendInstruction(Instruction{Op: OpLoadLiteral, Arg: 0})
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpRestoreContinuation})

	result := Disassemble(tpl)

	c.Assert(result.Name, qt.Equals, "test-proc")
	c.Assert(result.ParamCount, qt.Equals, 1)
	c.Assert(result.IsVariadic, qt.Equals, false)
	c.Assert(result.Literals, qt.HasLen, 1)
	c.Assert(result.Literals[0], qt.Equals, "42")
	c.Assert(result.Instructions, qt.HasLen, 3)

	// First instruction: LoadLiteral
	instr0 := result.Instructions[0]
	c.Assert(instr0.PC, qt.Equals, 0)
	c.Assert(instr0.Op, qt.Equals, "LoadLiteral")
	c.Assert(instr0.Arg, qt.Equals, int32(0))
	c.Assert(instr0.Literal, qt.Equals, "42")

	// Second: Push (no annotations)
	c.Assert(result.Instructions[1].Op, qt.Equals, "Push")

	// Third: RestoreContinuation
	c.Assert(result.Instructions[2].Op, qt.Equals, "RestoreContinuation")
}

func TestDisassemble_LocalIndex(t *testing.T) {
	c := qt.New(t)

	tpl := NewNativeTemplate(2, 0, false)
	// Encode slot=3, depth=1
	arg := int32(1<<16) | int32(3&0xFFFF)
	tpl.AppendInstruction(Instruction{Op: OpLoadLocal, Arg: arg})

	result := Disassemble(tpl)
	c.Assert(result.Instructions, qt.HasLen, 1)
	instr := result.Instructions[0]
	c.Assert(instr.Slot, qt.Equals, 3)
	c.Assert(instr.Depth, qt.Equals, 1)
}

func TestDisassemble_Branch(t *testing.T) {
	c := qt.New(t)

	tpl := NewNativeTemplate(0, 0, false)
	// BranchOnFalseValue at PC 0, offset +3 → target = 3
	tpl.AppendInstruction(Instruction{Op: OpBranchOnFalseValue, Arg: 3})
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})
	tpl.AppendInstruction(Instruction{Op: OpBranch, Arg: 2})
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})
	tpl.AppendInstruction(Instruction{Op: OpRestoreContinuation})

	result := Disassemble(tpl)
	c.Assert(result.Instructions[0].Target, qt.Equals, 3)
	c.Assert(result.Instructions[2].Target, qt.Equals, 4)
}

func TestDisassemble_CachedBinding(t *testing.T) {
	c := qt.New(t)

	tpl := NewNativeTemplate(0, 0, false)

	fc := NewForeignClosure(nil, nil, 2, false, "my-func")
	bd := environment.NewBinding(fc, environment.BindingTypeVariable)
	tpl.AppendCachedBinding(bd)

	tpl.AppendInstruction(Instruction{Op: OpPushCachedBinding, Arg: 0})

	result := Disassemble(tpl)
	c.Assert(result.Bindings, qt.HasLen, 1)
	c.Assert(result.Bindings[0], qt.Equals, "my-func")
	c.Assert(result.Instructions[0].Binding, qt.Equals, "my-func")
}

func TestDisassemble_SideTable(t *testing.T) {
	c := qt.New(t)

	tpl := NewNativeTemplate(0, 0, false)
	op := NewOperationBuildSyntaxList(3)
	sideInstr := tpl.AppendSideTableOp(op)
	tpl.AppendInstruction(sideInstr)

	result := Disassemble(tpl)
	c.Assert(result.Instructions[0].Op, qt.Equals, "Complex")
	c.Assert(result.Instructions[0].SideOp, qt.Not(qt.Equals), "")
}

func TestDisassemble_MakeClosureAnnotation(t *testing.T) {
	c := qt.New(t)

	tpl := NewNativeTemplate(0, 0, false)
	subTpl := NewNativeTemplate(1, 0, false)
	subTpl.SetName("inner")
	litIdx := tpl.MaybeAppendLiteral(subTpl)
	tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx)})
	tpl.AppendInstruction(Instruction{Op: OpMakeClosure})

	result := Disassemble(tpl)
	// MakeClosure should be annotated with the sub-template name
	c.Assert(result.Instructions[1].Literal, qt.Equals, "<lambda:inner>")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestDisassemble ./machine/...`
Expected: FAIL — `Disassemble` undefined

**Step 3: Write the implementation**

Create `machine/disassemble.go`:

```go
package machine

import (
	"fmt"
	"strings"

	"github.com/aalpar/wile/values"
)

// DisassembledInstruction holds the annotated disassembly of a single
// bytecode instruction. Zero-valued fields are not applicable to the
// instruction's opcode.
type DisassembledInstruction struct {
	PC      int
	Op      string
	Arg     int32
	Slot    int
	Depth   int
	Target  int    // absolute PC for branch/save ops; -1 when not applicable
	Literal string
	Binding string
	SideOp  string
	Source  string
}

// DisassembledTemplate holds the annotated disassembly of a NativeTemplate.
type DisassembledTemplate struct {
	Name         string
	ParamCount   int
	IsVariadic   bool
	Doc          string
	Literals     []string
	Bindings     []string
	Instructions []DisassembledInstruction
}

// Disassemble produces structured disassembly of a NativeTemplate.
func Disassemble(tpl *NativeTemplate) DisassembledTemplate {
	code := tpl.Code()
	literals := tpl.Literals()
	sideTable := tpl.SideTable()
	cachedBindings := tpl.CachedBindings()

	// Build literal name table
	litStrs := make([]string, len(literals))
	for i, lit := range literals {
		litStrs[i] = lit.SchemeString()
	}

	// Build cached binding name table
	bindingNames := make([]string, len(cachedBindings))
	for i, bd := range cachedBindings {
		bindingNames[i] = bindingName(bd)
	}

	instructions := make([]DisassembledInstruction, len(code))
	for pc, instr := range code {
		di := DisassembledInstruction{
			PC:     pc,
			Op:     instr.Op.String(),
			Arg:    instr.Arg,
			Target: -1,
		}

		// Source annotation
		src := tpl.SourceAt(pc)
		if src != nil {
			di.Source = fmt.Sprintf("%s:%d:%d", src.File, src.Start.Line(), src.Start.Column())
		}

		// Opcode-specific annotations
		switch instr.Op {
		// Literal ops
		case OpLoadLiteral, OpPushLiteral:
			if int(instr.Arg) < len(litStrs) {
				di.Literal = litStrs[instr.Arg]
			}

		// Global ops (arg is literal index of the symbol name)
		case OpLoadGlobal, OpStoreGlobal, OpPushGlobal:
			if int(instr.Arg) < len(litStrs) {
				di.Literal = litStrs[instr.Arg]
			}

		// Local ops (bit-packed slot|depth)
		case OpLoadLocal, OpStoreLocal, OpPushLocal, OpCallLocal:
			di.Slot, di.Depth = DecodeLocalIndex(instr.Arg)

		// Branch/save ops (relative offset → absolute target)
		case OpBranch, OpBranchOnFalseValue, OpSaveContinuation:
			di.Target = pc + int(instr.Arg)

		// Cached binding ops
		case OpLoadCachedBinding, OpPushCachedBinding,
			OpCallForeignCached, OpCallForeignCachedTail,
			OpCallCachedBinding:
			if int(instr.Arg) < len(bindingNames) {
				di.Binding = bindingNames[instr.Arg]
			}

		// Promoted ops (arg is binding index)
		case OpEqQ, OpEqQTail,
			OpVectorQ, OpVectorQTail,
			OpVectorRef, OpVectorRefTail,
			OpNullQ, OpNullQTail,
			OpPairQ, OpPairQTail,
			OpCar, OpCarTail,
			OpCdr, OpCdrTail,
			OpAdd, OpAddTail,
			OpSub, OpSubTail,
			OpMul, OpMulTail,
			OpDiv, OpDivTail,
			OpNumLt, OpNumLtTail,
			OpNumLe, OpNumLeTail,
			OpNumGt, OpNumGtTail,
			OpNumGe, OpNumGeTail,
			OpNumEq, OpNumEqTail,
			OpCons, OpConsTail:
			if int(instr.Arg) < len(bindingNames) {
				di.Binding = bindingNames[instr.Arg]
			}

		// Side table ops
		case OpComplex:
			if int(instr.Arg) < len(sideTable) {
				di.SideOp = sideTable[instr.Arg].String()
			}

		// MakeClosure: annotate with sub-template name if available
		case OpMakeClosure:
			di.Literal = makeClosureAnnotation(pc, code, literals)

		// PushEnv / PeekK: arg is meaningful as a count
		case OpPushEnv, OpPeekK:
			// Arg is already in di.Arg; no extra annotation needed
		}

		instructions[pc] = di
	}

	return DisassembledTemplate{
		Name:         tpl.Name(),
		ParamCount:   tpl.ParameterCount(),
		IsVariadic:   tpl.IsVariadic(),
		Doc:          tpl.Doc(),
		Literals:     litStrs,
		Bindings:     bindingNames,
		Instructions: instructions,
	}
}

// makeClosureAnnotation looks at the instruction before OpMakeClosure.
// If it loaded a NativeTemplate literal, returns "<lambda:name>" or "<lambda>".
func makeClosureAnnotation(pc int, code []Instruction, literals MultipleValues) string {
	if pc == 0 {
		return ""
	}
	prev := code[pc-1]
	var litIdx int32
	switch prev.Op {
	case OpLoadLiteral, OpPushLiteral:
		litIdx = prev.Arg
	default:
		return ""
	}
	if int(litIdx) >= len(literals) {
		return ""
	}
	subTpl, ok := literals[litIdx].(*NativeTemplate)
	if !ok {
		return ""
	}
	name := subTpl.Name()
	if name != "" {
		return "<lambda:" + name + ">"
	}
	return "<lambda>"
}

// bindingName extracts a human-readable name from a cached binding.
func bindingName(bd *environment.Binding) string {
	if bd == nil {
		return "?"
	}
	v := bd.Value()
	if v == nil {
		return "?"
	}
	type named interface {
		Name() string
	}
	n, ok := v.(named)
	if ok && n.Name() != "" {
		return n.Name()
	}
	return v.SchemeString()
}

// DisassembleString produces a human-readable disassembly listing.
func DisassembleString(tpl *NativeTemplate) string {
	dis := Disassemble(tpl)
	var sb strings.Builder

	// Header
	name := dis.Name
	if name == "" {
		name = "<anonymous>"
	}
	fmt.Fprintf(&sb, "%s  (params: %d, variadic: %v)\n", name, dis.ParamCount, dis.IsVariadic)
	if dis.Doc != "" {
		fmt.Fprintf(&sb, "doc: %s\n", dis.Doc)
	}

	// Literals
	if len(dis.Literals) > 0 {
		fmt.Fprintf(&sb, "literals: [%s]\n", strings.Join(dis.Literals, ", "))
	}

	// Cached bindings
	if len(dis.Bindings) > 0 {
		parts := make([]string, len(dis.Bindings))
		for i, name := range dis.Bindings {
			parts[i] = fmt.Sprintf("%d: %s", i, name)
		}
		fmt.Fprintf(&sb, "cached bindings: [%s]\n", strings.Join(parts, ", "))
	}

	sb.WriteString("\n")

	// Column header
	fmt.Fprintf(&sb, "  %-4s %-22s %5s   %-24s %s\n", "PC", "OP", "ARG", "DETAIL", "SOURCE")

	// Instructions
	for _, instr := range dis.Instructions {
		detail := formatDetail(instr)
		argStr := ""
		if instr.Arg != 0 || opcodeHasArg(instr.Op) {
			argStr = fmt.Sprintf("%d", instr.Arg)
		}

		source := instr.Source
		fmt.Fprintf(&sb, "  %-4d %-22s %5s   %-24s %s\n",
			instr.PC, instr.Op, argStr, detail, source)
	}

	return sb.String()
}

// formatDetail builds the DETAIL column for an instruction.
func formatDetail(instr DisassembledInstruction) string {
	var parts []string
	if instr.Slot != 0 || instr.Depth != 0 {
		parts = append(parts, fmt.Sprintf("slot=%d depth=%d", instr.Slot, instr.Depth))
	}
	if instr.Target >= 0 {
		parts = append(parts, fmt.Sprintf("→%d", instr.Target))
	}
	if instr.Literal != "" {
		parts = append(parts, instr.Literal)
	}
	if instr.Binding != "" {
		parts = append(parts, instr.Binding)
	}
	if instr.SideOp != "" {
		parts = append(parts, instr.SideOp)
	}
	return strings.Join(parts, " ")
}

// opcodeHasArg returns true for opcodes where Arg=0 is still meaningful
// (as opposed to unused).
func opcodeHasArg(op string) bool {
	switch op {
	case "LoadLiteral", "PushLiteral",
		"LoadGlobal", "PushGlobal", "StoreGlobal",
		"LoadLocal", "StoreLocal", "PushLocal",
		"LoadCachedBinding", "PushCachedBinding",
		"CallForeignCached", "CallForeignCachedTail",
		"CallLocal", "CallCachedBinding",
		"Branch", "BranchOnFalseValue", "SaveContinuation",
		"PeekK", "PushEnv", "Complex":
		return true
	}
	return false
}
```

**Step 4: Run tests to verify they pass**

Run: `go test -v -run TestDisassemble ./machine/...`
Expected: PASS

**Step 5: Run lint**

Run: `goimports -w machine/disassemble.go machine/disassemble_test.go && make lint`
Expected: clean

**Step 6: Commit**

```
add core disassembly types and functions

Disassemble() produces structured DisassembledTemplate from a
NativeTemplate. DisassembleString() formats it as a columnar
human-readable listing with annotations.
```

---

### Task 3: Add DisassembleString test

This verifies the string output format is correct and stable.

**Files:**
- Modify: `machine/disassemble_test.go`

**Step 1: Write the test**

Add to `machine/disassemble_test.go`:

```go
func TestDisassembleString_Header(t *testing.T) {
	c := qt.New(t)

	tpl := NewNativeTemplate(2, 0, true)
	tpl.SetName("my-func")
	tpl.SetDoc("A test function.")
	tpl.MaybeAppendLiteral(values.NewSymbol("x"))
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})

	result := DisassembleString(tpl)

	c.Assert(result, qt.Contains, "my-func")
	c.Assert(result, qt.Contains, "params: 2")
	c.Assert(result, qt.Contains, "variadic: true")
	c.Assert(result, qt.Contains, "doc: A test function.")
	c.Assert(result, qt.Contains, "literals: [x]")
	c.Assert(result, qt.Contains, "LoadVoid")
}

func TestDisassembleString_BranchTarget(t *testing.T) {
	c := qt.New(t)

	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpBranchOnFalseValue, Arg: 2})
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})
	tpl.AppendInstruction(Instruction{Op: OpRestoreContinuation})

	result := DisassembleString(tpl)
	// Target should be PC 0 + offset 2 = 2
	c.Assert(result, qt.Contains, "→2")
}
```

**Step 2: Run test**

Run: `go test -v -run TestDisassembleString ./machine/...`
Expected: PASS

**Step 3: Commit**

```
add string format tests for disassembler
```

---

### Task 4: Scheme primitive `(disassemble proc)`

**Files:**
- Create: `extensions/introspection/prim_disassemble.go`
- Modify: `extensions/introspection/register.go`

**Step 1: Write the primitive**

Create `extensions/introspection/prim_disassemble.go`:

```go
package introspection

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimDisassemble implements the (disassemble proc) primitive.
// Returns a structured disassembly as a list of alists.
// The first element is a header alist with metadata; remaining elements
// are instruction alists.
func PrimDisassemble(mc *machine.MachineContext) error {
	proc := mc.Arg(0)

	switch c := proc.(type) {
	case *machine.MachineClosure:
		mc.SetValue(disassembleMachineClosure(c))
	case *machine.CaseLambdaClosure:
		mc.SetValue(disassembleCaseLambda(c))
	case *machine.ForeignClosure:
		mc.SetValue(disassembleForeign(c))
	default:
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"disassemble: expected a procedure, got %s", proc.SchemeString())
	}
	return nil
}

func disassembleMachineClosure(c *machine.MachineClosure) values.Value {
	dis := machine.Disassemble(c.Template())
	return templateToScheme(dis)
}

func disassembleCaseLambda(c *machine.CaseLambdaClosure) values.Value {
	clauses := c.Clauses()
	clauseList := make([]values.Value, len(clauses))
	for i, clause := range clauses {
		dis := machine.Disassemble(clause.Template())
		clauseList[i] = templateToScheme(dis)
	}
	return values.List(
		values.NewCons(values.NewSymbol("type"), values.NewSymbol("case-lambda-closure")),
		values.NewCons(values.NewSymbol("clauses"), values.List(clauseList...)),
	)
}

func disassembleForeign(c *machine.ForeignClosure) values.Value {
	return values.List(
		values.NewCons(values.NewSymbol("type"), values.NewSymbol("foreign-closure")),
		values.NewCons(values.NewSymbol("name"), values.NewString(c.Name())),
		values.NewCons(values.NewSymbol("params"), values.NewInteger(int64(c.ParameterCount()))),
		values.NewCons(values.NewSymbol("variadic"), values.BoolToBoolean(c.IsVariadic())),
		values.NewCons(values.NewSymbol("doc"), values.NewString(c.Doc())),
	)
}

func templateToScheme(dis machine.DisassembledTemplate) values.Value {
	// Build header
	litVals := make([]values.Value, len(dis.Literals))
	for i, s := range dis.Literals {
		litVals[i] = values.NewString(s)
	}
	bindVals := make([]values.Value, len(dis.Bindings))
	for i, s := range dis.Bindings {
		bindVals[i] = values.NewString(s)
	}

	header := values.List(
		values.NewCons(values.NewSymbol("type"), values.NewSymbol("native-closure")),
		values.NewCons(values.NewSymbol("name"), values.NewString(dis.Name)),
		values.NewCons(values.NewSymbol("params"), values.NewInteger(int64(dis.ParamCount))),
		values.NewCons(values.NewSymbol("variadic"), values.BoolToBoolean(dis.IsVariadic)),
		values.NewCons(values.NewSymbol("doc"), values.NewString(dis.Doc)),
		values.NewCons(values.NewSymbol("literals"), values.NewVectorFromSlice(litVals)),
		values.NewCons(values.NewSymbol("bindings"), values.NewVectorFromSlice(bindVals)),
	)

	// Build instruction list
	elems := make([]values.Value, 0, len(dis.Instructions)+1)
	elems = append(elems, header)
	for _, instr := range dis.Instructions {
		elems = append(elems, instructionToScheme(instr))
	}
	return values.List(elems...)
}

func instructionToScheme(instr machine.DisassembledInstruction) values.Value {
	pairs := []values.Value{
		values.NewCons(values.NewSymbol("pc"), values.NewInteger(int64(instr.PC))),
		values.NewCons(values.NewSymbol("op"), values.NewSymbol(instr.Op)),
	}
	if instr.Arg != 0 {
		pairs = append(pairs,
			values.NewCons(values.NewSymbol("arg"), values.NewInteger(int64(instr.Arg))))
	}
	if instr.Slot != 0 || instr.Depth != 0 {
		pairs = append(pairs,
			values.NewCons(values.NewSymbol("slot"), values.NewInteger(int64(instr.Slot))),
			values.NewCons(values.NewSymbol("depth"), values.NewInteger(int64(instr.Depth))))
	}
	if instr.Target >= 0 {
		pairs = append(pairs,
			values.NewCons(values.NewSymbol("target"), values.NewInteger(int64(instr.Target))))
	}
	if instr.Literal != "" {
		pairs = append(pairs,
			values.NewCons(values.NewSymbol("literal"), values.NewString(instr.Literal)))
	}
	if instr.Binding != "" {
		pairs = append(pairs,
			values.NewCons(values.NewSymbol("binding"), values.NewString(instr.Binding)))
	}
	if instr.SideOp != "" {
		pairs = append(pairs,
			values.NewCons(values.NewSymbol("side-op"), values.NewString(instr.SideOp)))
	}
	if instr.Source != "" {
		pairs = append(pairs,
			values.NewCons(values.NewSymbol("source"), values.NewString(instr.Source)))
	}
	return values.List(pairs...)
}
```

**Step 2: Register the primitive**

Modify `extensions/introspection/register.go`, add to the `addPrimitives` function's `AddPrimitives` call:

```go
{Name: "disassemble", ParamCount: 1, Impl: PrimDisassemble,
	Doc: "Returns structured disassembly of a procedure as a list of alists. " +
		"The first element is a header with metadata (type, name, params, " +
		"variadic, doc, literals, bindings). Remaining elements are instruction " +
		"alists with keys: pc, op, arg, slot, depth, target, literal, binding, " +
		"side-op, source. Works with native closures, case-lambda, and " +
		"foreign closures.",
	ParamNames: []string{"proc"}, Category: "introspection"},
```

**Step 3: Verify build**

Run: `go build ./extensions/introspection/...`
Expected: clean

**Step 4: Commit**

```
add (disassemble proc) Scheme primitive

Returns structured bytecode disassembly as a list of alists.
Supports MachineClosure, CaseLambdaClosure, and ForeignClosure.
```

---

### Task 5: Integration test for Scheme primitive

**Files:**
- Create: `extensions/introspection/prim_disassemble_test.go`

**Step 1: Write integration test**

Create `extensions/introspection/prim_disassemble_test.go`:

```go
package introspection_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestPrimDisassemble_NativeClosure(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithAllExtensions())
	c.Assert(err, qt.IsNil)

	// Define a simple function and disassemble it
	_, err = eng.EvalMultiple(ctx, `(define (add1 x) (+ x 1))`)
	c.Assert(err, qt.IsNil)

	val, err := eng.EvalMultiple(ctx, `(disassemble add1)`)
	c.Assert(err, qt.IsNil)

	// Result should be a non-empty list
	pair, ok := val.(*values.Pair)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected pair, got %T", val))
	c.Assert(pair.IsEmptyList(), qt.IsFalse)

	// First element is the header alist
	header := pair.Car()
	headerPair, ok := header.(*values.Pair)
	c.Assert(ok, qt.IsTrue)

	// Header should contain (type . native-closure)
	typePair := headerPair.Car().(*values.Pair)
	c.Assert(typePair.Car().(*values.Symbol).Key, qt.Equals, "type")
	c.Assert(typePair.Cdr().(*values.Symbol).Key, qt.Equals, "native-closure")
}

func TestPrimDisassemble_ForeignClosure(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithAllExtensions())
	c.Assert(err, qt.IsNil)

	val, err := eng.EvalMultiple(ctx, `(disassemble car)`)
	c.Assert(err, qt.IsNil)

	pair, ok := val.(*values.Pair)
	c.Assert(ok, qt.IsTrue)

	// Should be a single alist (header only, no instructions)
	header := pair.Car().(*values.Pair)
	typePair := header.Car().(*values.Pair)
	c.Assert(typePair.Car().(*values.Symbol).Key, qt.Equals, "type")
	c.Assert(typePair.Cdr().(*values.Symbol).Key, qt.Equals, "foreign-closure")
}

func TestPrimDisassemble_CaseLambda(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithAllExtensions())
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(define f (case-lambda ((x) x) ((x y) (+ x y))))`)
	c.Assert(err, qt.IsNil)

	val, err := eng.EvalMultiple(ctx, `(disassemble f)`)
	c.Assert(err, qt.IsNil)

	pair, ok := val.(*values.Pair)
	c.Assert(ok, qt.IsTrue)

	// Header should have type case-lambda-closure
	header := pair.Car().(*values.Pair)
	typePair := header.Car().(*values.Pair)
	c.Assert(typePair.Cdr().(*values.Symbol).Key, qt.Equals, "case-lambda-closure")
}

func TestPrimDisassemble_NotAProcedure(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithAllExtensions())
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(disassemble 42)`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "disassemble")
}

// TestPrimDisassemble_InstructionKeys verifies that instruction alists
// contain the expected keys.
func TestPrimDisassemble_InstructionKeys(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithAllExtensions())
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(define (id x) x)`)
	c.Assert(err, qt.IsNil)

	val, err := eng.EvalMultiple(ctx, `(disassemble id)`)
	c.Assert(err, qt.IsNil)

	// Walk past header to first instruction
	pair := val.(*values.Pair)
	instrList := pair.Cdr().(values.Tuple)

	// First instruction should have at least pc and op keys
	firstInstr := instrList.Car().(*values.Pair)
	pcPair := firstInstr.Car().(*values.Pair)
	c.Assert(pcPair.Car().(*values.Symbol).Key, qt.Equals, "pc")

	// The pc value should be an integer
	_, ok := pcPair.Cdr().(*values.Integer)
	c.Assert(ok, qt.IsTrue)

	// Walk to second pair for op key
	secondPair := firstInstr.Cdr().(values.Tuple).Car().(*values.Pair)
	c.Assert(secondPair.Car().(*values.Symbol).Key, qt.Equals, "op")

	// op value should be a symbol
	_, ok = secondPair.Cdr().(*values.Symbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("op value: %T %v", secondPair.Cdr(), secondPair.Cdr()))
}
```

**Step 2: Run tests**

Run: `go test -v -run TestPrimDisassemble ./extensions/introspection/...`
Expected: PASS

> **Note:** If any assertion fails because `EvalMultiple` wraps the result differently, or `car`/`cdr` need `values.Tuple` instead of `*values.Pair`, adjust the type assertions to match. Read the actual error and fix the test to match the runtime types.

**Step 3: Commit**

```
add integration tests for (disassemble) primitive

Tests cover native closure, foreign closure, case-lambda,
error case, and instruction alist key structure.
```

---

### Task 6: REPL meta-command `,dis`

**Files:**
- Modify: `internal/repl/meta.go`

**Step 1: Add command registration**

In `meta.go`, add to the `metaCommands` slice (after the `libraries` entry):

```go
{"disassemble", []string{"dis"}, "Show bytecode disassembly of a procedure",
	"Usage: ,disassemble <name> or ,dis <name>\n\n" +
		"Looks up the named binding and displays its bytecode disassembly.\n" +
		"For native closures, shows the instruction listing with annotations.\n" +
		"For case-lambda, shows each clause separately.\n" +
		"For foreign closures, shows name, arity, and documentation.\n\n" +
		"For ad-hoc expressions, use (disassemble expr) at the REPL instead.",
	"session"},
```

**Step 2: Add case to Handle switch**

In the `Handle` method's switch statement, add before the `default:` case:

```go
case "disassemble", "dis":
	p.cmdDisassemble(args, out)
```

**Step 3: Write the handler method**

Add the `cmdDisassemble` method after `cmdLibraries`:

```go
func (p *MetaCommandHandler) cmdDisassemble(args []string, out io.Writer) {
	if len(args) == 0 {
		fmt.Fprintln(out, "Usage: ,disassemble <name>")
		return
	}

	name := args[0]
	sym := values.NewSymbol(name)

	// Look up binding across all phases (same path as ,doc)
	var val values.Value
	if p.env != nil {
		topLevel := p.env.Namespace()
		if topLevel != nil {
			phases := topLevel.Phases()
			for _, phase := range phases.Phases() {
				phaseEnv := phases.Get(phase)
				if phaseEnv == nil {
					continue
				}
				bnd := phaseEnv.GetBinding(sym)
				if bnd != nil {
					val = bnd.Value()
					break
				}
			}
		}
	}

	if val == nil {
		fmt.Fprintf(out, "Unbound identifier: %s\n", name)
		return
	}

	var content string
	switch c := val.(type) {
	case *machine.MachineClosure:
		content = machine.DisassembleString(c.Template())
	case *machine.CaseLambdaClosure:
		var sb strings.Builder
		for i, clause := range c.Clauses() {
			if i > 0 {
				sb.WriteString("\n")
			}
			fmt.Fprintf(&sb, "--- clause %d ---\n", i)
			sb.WriteString(machine.DisassembleString(clause.Template()))
		}
		content = sb.String()
	case *machine.ForeignClosure:
		var sb strings.Builder
		fmt.Fprintf(&sb, "%s  (foreign, params: %d, variadic: %v)\n",
			c.Name(), c.ParameterCount(), c.IsVariadic())
		if c.Doc() != "" {
			fmt.Fprintf(&sb, "doc: %s\n", c.Doc())
		}
		content = sb.String()
	default:
		fmt.Fprintf(out, "%s is not a procedure (type: %T)\n", name, val)
		return
	}

	writeWithPager(out, content, p.pager)
}
```

**Step 4: Verify build**

Run: `go build ./internal/repl/...`
Expected: clean

**Step 5: Commit**

```
add ,disassemble / ,dis REPL meta-command

Looks up a binding by name and displays bytecode disassembly
with annotations. Supports native, case-lambda, and foreign
closures.
```

---

### Task 7: REPL meta-command test

**Files:**
- Modify: `internal/repl/meta_test.go`

**Step 1: Read existing meta_test.go to understand test patterns**

Run: `head -80 internal/repl/meta_test.go`

Adapt the test to match existing patterns. The test should create a `MetaCommandHandler` with an environment, bind a closure, and verify `,dis` output.

**Step 2: Write the test**

Add to `internal/repl/meta_test.go`. The exact setup depends on what's already there — follow the existing pattern for `,doc` tests. The core assertion:

```go
func TestCmdDisassemble_NativeClosure(t *testing.T) {
	// Setup: create env with a bound closure (follow existing test setup pattern)
	// ...

	var buf strings.Builder
	handler.Handle(",dis <bound-name>", &buf)
	output := buf.String()

	// Should contain the procedure name and at least one opcode
	c.Assert(output, qt.Contains, "<name>")
	c.Assert(output, qt.Contains, "OP")  // column header
}

func TestCmdDisassemble_Unbound(t *testing.T) {
	var buf strings.Builder
	handler.Handle(",dis nonexistent", &buf)
	c.Assert(buf.String(), qt.Contains, "Unbound identifier")
}

func TestCmdDisassemble_NoArgs(t *testing.T) {
	var buf strings.Builder
	handler.Handle(",dis", &buf)
	c.Assert(buf.String(), qt.Contains, "Usage:")
}
```

> **Note:** Read the existing test file first. Adapt constructors and setup to match the test infrastructure already in place. Do not guess at how environments are built in tests.

**Step 3: Run tests**

Run: `go test -v -run TestCmdDisassemble ./internal/repl/...`
Expected: PASS

**Step 4: Commit**

```
add tests for ,dis REPL meta-command
```

---

### Task 8: MCP tool

**Files:**
- Modify: `cmd/wile/mcp.go`
- Modify: `cmd/wile/mcp_test.go`

**Step 1: Add tool registration**

In `doMCP`, after the `libraries` tool registration, add:

```go
s.AddTool(
	mcp.NewTool("disassemble",
		mcp.WithDescription(
			"Show bytecode disassembly of a named procedure. "+
				"Pass the name of a defined procedure (e.g. \"map\", \"my-function\"). "+
				"Returns an annotated instruction listing with opcodes, literals, "+
				"branch targets, cached binding names, and source locations."),
		mcp.WithString("name",
			mcp.Required(),
			mcp.Description("Name of the procedure to disassemble")),
	),
	srv.handleDisassemble,
)
```

**Step 2: Add handler method**

Add after `handleLibraries`:

```go
func (p *mcpServer) handleDisassemble(ctx context.Context, req mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	name := req.GetString("name", "")
	if name == "" {
		return mcp.NewToolResultError("name parameter is required"), nil
	}
	return p.runMeta(ctx, ",dis "+name)
}
```

**Step 3: Verify build**

Run: `go build ./cmd/wile/...`
Expected: clean

**Step 4: Add test**

Add to `cmd/wile/mcp_test.go`, following the existing test pattern for other tools. The test should call the `disassemble` tool with a known procedure name and verify the result contains expected content.

> **Note:** Read `cmd/wile/mcp_test.go` first to understand the test infrastructure. Follow the existing pattern exactly.

**Step 5: Run tests**

Run: `go test -v -run TestMCP.*Disassemble ./cmd/wile/...`
Expected: PASS

**Step 6: Commit**

```
add disassemble MCP tool

Delegates to ,dis meta-command for consistent formatting
across REPL and MCP interfaces.
```

---

### Task 9: Full verification

**Step 1: Run lint**

Run: `make lint`
Expected: clean

**Step 2: Run full test suite**

Run: `make test`
Expected: PASS

**Step 3: Run coverage check**

Run: `make covercheck`
Expected: PASS

**Step 4: Manual smoke test**

Run: `go build -o dist/wile ./cmd/wile && ./dist/wile`

At the REPL:
```scheme
> (define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
> ,dis fib
> (disassemble fib)
> (disassemble car)
> (disassemble (case-lambda ((x) x) ((x y) (+ x y))))
```

Verify:
- `,dis fib` shows a formatted listing with branch targets and binding names
- `(disassemble fib)` returns an alist structure
- `(disassemble car)` returns a foreign-closure alist
- The case-lambda disassembly shows two clauses

**Step 5: Final commit (if any fixups needed)**

---

## Verification Summary

After all tasks:

```bash
make lint && make covercheck && make test
```

All three must pass.
