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
	"math"
	"slices"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

type LiteralIndex int

type NativeTemplate struct {
	parameterCount int
	valueCount     int
	isVariadic     bool
	literals       MultipleValues
	sourceRefs     []uint16                // parallel to code, index into sourceTable
	sourceTable    []*syntax.SourceContext // index 0 = nil (no source)
	name           string                  // Function name (for stack traces)

	// Integer dispatch: all operations compiled to Instructions.
	// Hot-path ops (Wave 1-3) are direct switch cases; complex ops
	// (closures, macros, FFI) are in sideTable and dispatched via OpComplex.
	code      []Instruction // bytecode instructions
	sideTable []Operation   // complex ops referenced by OpComplex
}

// initialOpsCap is the pre-allocated capacity for the operations and sourceRefs
// slices when a template is created without initial operations (the compilation
// path). Covers simple lambda bodies without re-allocation; larger functions
// grow normally. Kept small to avoid wasting memory on the many short templates
// typical in Scheme programs.
const initialOpsCap = 8

func NewNativeTemplate(pcnt int, vcnt int, vd bool, operations ...Operation) *NativeTemplate {
	q := &NativeTemplate{
		parameterCount: pcnt,
		valueCount:     vcnt,
		isVariadic:     vd,
		sourceTable:    []*syntax.SourceContext{nil}, // index 0 = nil (no source)
		code:           make([]Instruction, 0, initialOpsCap),
		sourceRefs:     make([]uint16, 0, initialOpsCap),
	}
	if len(operations) > 0 {
		// Direct construction with initial operations (e.g., test fixtures).
		// Convert operations to instructions via AppendOperations.
		q.AppendOperations(operations...)
	}
	return q
}

// NewEmptyNativeTemplate creates a new NativeTemplate with default empty parameters.
// This is used when a template is initialized without any known parameters or operations yet.
func NewEmptyNativeTemplate() *NativeTemplate {
	return NewNativeTemplate(0, 0, false)
}

func (p *NativeTemplate) ParameterCount() int {
	return p.parameterCount
}

func (p *NativeTemplate) ValueCount() int {
	return p.valueCount
}

func (p *NativeTemplate) IsVariadic() bool {
	return p.isVariadic
}

// Operations reconstructs the operation sequence from the bytecode.
// Converts Instructions back to Operation values for compatibility with
// existing code that expects Operations (e.g., tests, EqualTo).
func (p *NativeTemplate) Operations() Operations {
	ops := make(Operations, len(p.code))
	for i, instr := range p.code {
		if instr.Op == OpComplex {
			ops[i] = p.sideTable[instr.Arg]
		} else {
			ops[i] = instructionToOperation(instr)
		}
	}
	return ops
}

// EffectiveOperations is an alias for Operations() for backward compatibility.
// Both return the reconstructed operation sequence from bytecode.
func (p *NativeTemplate) EffectiveOperations() Operations {
	return p.Operations()
}

// instructionToOperation converts a direct instruction back to its
// corresponding Operation value. Used by EffectiveOperations for test support.
func instructionToOperation(instr Instruction) Operation {
	switch instr.Op {
	// --- Wave 1: zero-operand operations ---
	case OpPush:
		return NewOperationPush()
	case OpPop:
		return NewOperationPop()
	case OpPull:
		return NewOperationPull()
	case OpLoadVoid:
		return NewOperationLoadVoid()
	case OpDrop:
		return NewOperationDrop()
	case OpPopEnv:
		return NewOperationPopEnv()
	case OpApply:
		return NewOperationApply()
	case OpRestoreContinuation:
		return NewOperationRestoreContinuation()

	// --- Wave 2: single-operand operations ---
	case OpBranchOnFalseValue:
		return NewOperationBranchOnFalseValueOffsetImmediate(int(instr.Arg))
	case OpBranch:
		return NewOperationBranchOffsetImmediate(int(instr.Arg))
	case OpSaveContinuation:
		return NewOperationSaveContinuationOffsetImmediate(int(instr.Arg))
	case OpLoadLiteral:
		return NewOperationLoadLiteralByLiteralIndexImmediate(LiteralIndex(instr.Arg))
	case OpLoadGlobal:
		return NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(instr.Arg))
	case OpStoreGlobal:
		return NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(instr.Arg))
	case OpPeekK:
		return NewOperationPeekK(int(instr.Arg))

	// --- Wave 3: two-operand operations (bit-packed LocalIndex) ---
	case OpLoadLocal:
		slot, depth := DecodeLocalIndex(instr.Arg)
		li := environment.NewLocalIndex(slot, depth)
		return NewOperationLoadLocalByLocalIndexImmediate(li)
	case OpStoreLocal:
		slot, depth := DecodeLocalIndex(instr.Arg)
		li := environment.NewLocalIndex(slot, depth)
		return NewOperationStoreLocalByLocalIndexImmediate(li)

	default:
		return nil
	}
}

// SourceAt returns the source location for the operation at pc.
// Returns nil if pc is out of bounds or no source was recorded.
// O(1) lookup via the parallel sourceRefs array.
func (p *NativeTemplate) SourceAt(pc int) *syntax.SourceContext {
	if pc < 0 || pc >= len(p.sourceRefs) {
		return nil
	}
	return p.sourceTable[p.sourceRefs[pc]]
}

// internSource deduplicates a source context and returns its index in the sourceTable.
// Uses pointer equality first (fast path), then structural equality via sourceEqual.
// Index 0 is reserved for nil (no source).
func (p *NativeTemplate) internSource(src *syntax.SourceContext) uint16 {
	if src == nil {
		return 0
	}
	for i, s := range p.sourceTable {
		if s == src || sourceEqual(s, src) {
			return uint16(i)
		}
	}
	idx := uint16(len(p.sourceTable))
	p.sourceTable = append(p.sourceTable, src)
	return idx
}

// AppendOperationsWithSource converts operations to instructions and tags
// each with the given source. Wave 1-3 operations become direct switch cases;
// complex operations go through the sideTable and are dispatched via OpComplex.
// This is a public method for test use.
func (p *NativeTemplate) AppendOperationsWithSource(src *syntax.SourceContext, ops ...Operation) {
	idx := p.internSource(src)
	for _, op := range ops {
		instr, ok := operationToInstruction(op)
		if !ok {
			instr = p.AppendSideTableOp(op)
		}
		p.code = append(p.code, instr)
		p.sourceRefs = append(p.sourceRefs, idx)
	}
}

// operationToInstruction converts Wave 1 and Wave 2 operations to direct Instructions.
// Returns (instruction, true) if the operation has a dedicated opcode,
// or (Instruction{}, false) if it should go through the sideTable.
func operationToInstruction(op Operation) (Instruction, bool) {
	switch v := op.(type) {
	// --- Wave 1: zero-operand operations ---
	case *OperationPush:
		return Instruction{Op: OpPush}, true
	case *OperationPop:
		return Instruction{Op: OpPop}, true
	case *OperationPull:
		return Instruction{Op: OpPull}, true
	case *OperationLoadVoid:
		return Instruction{Op: OpLoadVoid}, true
	case *OperationDrop:
		return Instruction{Op: OpDrop}, true
	case *OperationPopEnv:
		return Instruction{Op: OpPopEnv}, true
	case *OperationApply:
		return Instruction{Op: OpApply}, true
	case *OperationRestoreContinuation:
		return Instruction{Op: OpRestoreContinuation}, true

	// --- Wave 2: single-operand operations ---
	case *OperationBranchOnFalseValueOffsetImmediate:
		return Instruction{Op: OpBranchOnFalseValue, Arg: int32(v.Offset)}, true
	case *OperationBranchOffsetImmediate:
		return Instruction{Op: OpBranch, Arg: int32(v.Offset)}, true
	case *OperationSaveContinuationOffsetImmediate:
		return Instruction{Op: OpSaveContinuation, Arg: int32(v.Offset)}, true
	case *OperationLoadLiteralByLiteralIndexImmediate:
		return Instruction{Op: OpLoadLiteral, Arg: int32(v.LiteralIndex)}, true
	case *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate:
		return Instruction{Op: OpLoadGlobal, Arg: int32(v.LiteralIndex)}, true
	case *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate:
		return Instruction{Op: OpStoreGlobal, Arg: int32(v.LiteralIndex)}, true
	case *OperationPeekK:
		return Instruction{Op: OpPeekK, Arg: int32(v.Depth)}, true

	// --- Wave 3: two-operand operations (bit-packed LocalIndex) ---
	case *OperationLoadLocalByLocalIndexImmediate:
		return Instruction{Op: OpLoadLocal, Arg: EncodeLocalIndex(v.LocalIndex)}, true
	case *OperationStoreLocalByLocalIndexImmediate:
		return Instruction{Op: OpStoreLocal, Arg: EncodeLocalIndex(v.LocalIndex)}, true

	default:
		return Instruction{}, false
	}
}

// sourceEqual compares two source contexts for equality (by location only).
func sourceEqual(a, b *syntax.SourceContext) bool {
	if a == nil || b == nil {
		return a == b
	}
	return a.File == b.File &&
		a.Start.Line() == b.Start.Line() &&
		a.Start.Column() == b.Start.Column()
}

func (p *NativeTemplate) Name() string {
	return p.name
}

func (p *NativeTemplate) SetName(name string) {
	p.name = name
}

func (p *NativeTemplate) MaybeAppendLiteral(v values.Value) LiteralIndex {
	// Don't deduplicate environments - each closure needs its own instance
	// because environments are mutable and context-dependent. The parent
	// chain is set at runtime by OperationMakeClosure, so structural
	// equality doesn't capture lexical context.
	_, isEnv := v.(*environment.EnvironmentFrame)
	if !isEnv {
		for i, l := range p.literals {
			if literalIdentical(l, v) {
				return LiteralIndex(i)
			}
		}
	}
	l := len(p.literals)
	p.literals = append(p.literals, v)
	return LiteralIndex(l)
}

// literalIdentical returns true if two values are identical for literal
// deduplication purposes. This is stricter than EqualTo for floating point
// values: -0.0 and +0.0 are numerically equal but have different bit
// representations and must be kept as separate literals to preserve IEEE 754
// signed zero semantics in operations like atan2.
func literalIdentical(a, b values.Value) bool {
	// For floats, check both numeric equality and sign bit equality
	af, ok := a.(*values.Float)
	if ok {
		bf, ok := b.(*values.Float)
		if ok {
			return af.Value == bf.Value && math.Signbit(af.Value) == math.Signbit(bf.Value)
		}
		return false
	}
	// For all other types, use standard EqualTo
	return a.EqualTo(b)
}

func (p *NativeTemplate) findLiteral(v values.Value) values.Value {
	for _, l := range p.literals {
		if l.EqualTo(v) {
			return l
		}
	}
	return nil
}

func (p *NativeTemplate) deduplicateLiteral(v values.Value) values.Value {
	existing := p.findLiteral(v)
	if existing != nil {
		return existing
	}
	p.literals = append(p.literals, v)
	return v
}

// deduplicateVector deduplicates all elements in the given vector
// using the template's literal pool.
// Returns a new vector if any elements were changed, or the original vector otherwise.
// TODO: optimize for the common case where no elements change.  consider in place modification?
func (p *NativeTemplate) deduplicateVector(v *values.Vector) *values.Vector {
	if v == nil || len(*v) == 0 {
		return v
	}
	changed := false
	newElements := values.NewVectorWithLength(v.Length())
	for i, elem := range *v {
		deduped := p.DeduplicateLiteral(elem)
		(*newElements)[i] = deduped
		if deduped != elem {
			changed = true
		}
	}
	// No changes, return original vector
	// this avoids unnecessary pointer changes in the caller
	if !changed {
		return v
	}
	return newElements
}

// deduplicatePair deduplicates all elements in the given pair
// using the template's literal pool.
// Returns a new pair if any elements were changed, or the original pair otherwise.
// TODO: optimize for the common case where no elements change.  consider in place modification?
func (p *NativeTemplate) deduplicatePair(v *values.Pair) *values.Pair {
	if v == nil {
		return nil
	}
	car := p.DeduplicateLiteral(v.Car())
	cdr := p.DeduplicateLiteral(v.Cdr())
	// No changes, return original pair
	// this avoids unnecessary pointer changes in the caller
	if car == v.Car() && cdr == v.Cdr() {
		return v
	}
	return values.NewCons(car, cdr)
}

// DeduplicateLiteral deduplicates the given value using the template's literal pool.
// For composite values (pairs and vectors), all elements are deduplicated recursively.
// Returns the deduplicated value.
func (p *NativeTemplate) DeduplicateLiteral(v values.Value) values.Value {
	switch val := v.(type) {
	case *values.Symbol, *values.Integer:
		return p.deduplicateLiteral(val)
	case *values.Pair:
		return p.deduplicatePair(val)
	case *values.Vector:
		return p.deduplicateVector(val)
	default:
		return v
	}
}

// AppendOperations appends operations with no source attribution (index 0 = nil).
// Converts operations to instructions using AppendOperationsWithSource.
func (p *NativeTemplate) AppendOperations(ops ...Operation) {
	p.AppendOperationsWithSource(nil, ops...)
}

// AppendInstructionWithSource appends a single instruction to the integer-dispatch
// bytecode and tags it with the given source context.
func (p *NativeTemplate) AppendInstructionWithSource(src *syntax.SourceContext, instr Instruction) {
	idx := p.internSource(src)
	p.code = append(p.code, instr)
	p.sourceRefs = append(p.sourceRefs, idx)
}

// AppendInstruction appends a single instruction with no source attribution.
func (p *NativeTemplate) AppendInstruction(instr Instruction) {
	p.code = append(p.code, instr)
	p.sourceRefs = append(p.sourceRefs, 0)
}

// AppendSideTableOp adds a complex operation to the side table and returns
// an OpComplex instruction that references it.
func (p *NativeTemplate) AppendSideTableOp(op Operation) Instruction {
	idx := int32(len(p.sideTable))
	p.sideTable = append(p.sideTable, op)
	return Instruction{Op: OpComplex, Arg: idx}
}

// Code returns the integer-dispatch bytecode slice.
func (p *NativeTemplate) Code() []Instruction {
	return p.code
}

// SideTable returns the complex operations referenced by OpComplex instructions.
func (p *NativeTemplate) SideTable() []Operation {
	return p.sideTable
}

// CodeLen returns the current code[] length (number of instructions emitted).
func (p *NativeTemplate) CodeLen() int {
	return len(p.code)
}

// PatchSideTableOp replaces the sideTable operation referenced by code[codeIdx].
// The instruction at code[codeIdx] must be OpComplex.
func (p *NativeTemplate) PatchSideTableOp(codeIdx int, op Operation) {
	p.sideTable[p.code[codeIdx].Arg] = op
}

// SideTableOpAt returns the sideTable operation referenced by code[codeIdx].
// The instruction at code[codeIdx] must be OpComplex.
func (p *NativeTemplate) SideTableOpAt(codeIdx int) Operation {
	return p.sideTable[p.code[codeIdx].Arg]
}

// PatchInstructionArg updates the Arg field of the instruction at code[codeIdx].
// Used for patching branch offsets and continuation save offsets after the
// target PC is known.
func (p *NativeTemplate) PatchInstructionArg(codeIdx int, arg int32) {
	p.code[codeIdx].Arg = arg
}

func (p *NativeTemplate) SchemeString() string {
	return "#<native-template>"
}

func (p *NativeTemplate) IsVoid() bool {
	return p == nil
}

func (p *NativeTemplate) EqualTo(o values.Value) bool {
	v, ok := o.(*NativeTemplate)
	if !ok {
		return false
	}
	if v == nil || p == nil {
		return p == v
	}
	if p.parameterCount != v.parameterCount {
		return false
	}
	if p.valueCount != v.valueCount {
		return false
	}
	if p.isVariadic != v.isVariadic {
		return false
	}
	if len(p.literals) != len(v.literals) {
		return false
	}
	for i, l := range p.literals {
		if !l.EqualTo(v.literals[i]) {
			return false
		}
	}
	// Compare integer-dispatch bytecode if present.
	if len(p.code) != len(v.code) {
		return false
	}
	for i := range p.code {
		if p.code[i] != v.code[i] {
			return false
		}
	}
	if len(p.sideTable) != len(v.sideTable) {
		return false
	}
	for i := range p.sideTable {
		if !p.sideTable[i].EqualTo(v.sideTable[i]) {
			return false
		}
	}
	return true
}

func (p *NativeTemplate) Copy() *NativeTemplate {
	if p == nil {
		return nil
	}
	q := &NativeTemplate{
		parameterCount: p.parameterCount,
		valueCount:     p.valueCount,
		isVariadic:     p.isVariadic,
		name:           p.name,
	}
	q.literals = slices.Clone(p.literals)
	q.code = slices.Clone(p.code)
	q.sideTable = slices.Clone(p.sideTable)
	q.sourceRefs = slices.Clone(p.sourceRefs)
	q.sourceTable = slices.Clone(p.sourceTable)
	return q
}
