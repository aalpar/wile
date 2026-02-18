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
	operations     Operations
	sourceRefs     []uint16                // parallel to operations, index into sourceTable
	sourceTable    []*syntax.SourceContext // index 0 = nil (no source)
	name           string                  // Function name (for stack traces)

	// Integer dispatch fields (Phase 6). During migration, a template has
	// either operations (interface dispatch) or code+sideTable (integer
	// dispatch), never both. The VM's Run() checks which is populated.
	code      []Instruction // integer-dispatched bytecode
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
	}
	if len(operations) > 0 {
		// Direct construction with initial operations (e.g., NewForeignClosure).
		// Uses the interface-dispatch path.
		q.operations = operations
		q.sourceRefs = make([]uint16, len(operations))
	} else {
		// Compilation path: pre-allocate code[] for integer dispatch.
		q.code = make([]Instruction, 0, initialOpsCap)
		q.sourceRefs = make([]uint16, 0, initialOpsCap)
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

func (p *NativeTemplate) Operations() Operations {
	return p.operations
}

// EffectiveOperations returns the logical operation sequence regardless of
// dispatch mode. For interface-dispatch templates, returns operations directly.
// For integer-dispatch templates, reconstructs operations from code[]+sideTable[].
// Used by tests to verify compiled bytecode without depending on dispatch representation.
func (p *NativeTemplate) EffectiveOperations() Operations {
	if len(p.operations) > 0 {
		return p.operations
	}
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

// instructionToOperation converts a direct instruction back to its
// corresponding Operation value. Used by EffectiveOperations for test support.
func instructionToOperation(instr Instruction) Operation {
	switch instr.Op {
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

// appendOperationsWithSource appends operations and tags each with the given source.
// This is the source-aware path used by the compiler's AppendOperations method.
func (p *NativeTemplate) appendOperationsWithSource(src *syntax.SourceContext, ops ...Operation) {
	idx := p.internSource(src)
	p.operations = append(p.operations, ops...)
	for range ops {
		p.sourceRefs = append(p.sourceRefs, idx)
	}
}

// appendInstructionsWithSource routes operations to the integer-dispatch code[]
// path. Wave 1 (zero-operand) operations are converted to direct instructions;
// all other operations are placed in the sideTable and referenced via OpComplex.
func (p *NativeTemplate) appendInstructionsWithSource(src *syntax.SourceContext, ops ...Operation) {
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

// operationToInstruction converts a Wave 1 operation to a direct Instruction.
// Returns (instruction, true) if the operation has a dedicated opcode,
// or (Instruction{}, false) if it should go through the sideTable.
func operationToInstruction(op Operation) (Instruction, bool) {
	switch op.(type) {
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
// Routes to integer dispatch (code[]) or interface dispatch (operations[])
// based on which path this template uses.
func (p *NativeTemplate) AppendOperations(ops ...Operation) {
	if p.code != nil {
		// Integer-dispatch path: route through instruction conversion.
		p.appendInstructionsWithSource(nil, ops...)
		return
	}
	p.operations = append(p.operations, ops...)
	for range ops {
		p.sourceRefs = append(p.sourceRefs, 0)
	}
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
	// Compare interface-dispatch operations if present.
	if len(p.operations) != len(v.operations) {
		return false
	}
	for i := range p.operations {
		op0, ok0 := p.operations[i].(values.Value)
		op1, ok1 := v.operations[i].(values.Value)
		if !ok0 || !ok1 {
			return false
		}
		if !op0.EqualTo(op1) {
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
	q.operations = slices.Clone(p.operations)
	q.code = slices.Clone(p.code)
	q.sideTable = slices.Clone(p.sideTable)
	q.sourceRefs = slices.Clone(p.sourceRefs)
	q.sourceTable = slices.Clone(p.sourceTable)
	return q
}
