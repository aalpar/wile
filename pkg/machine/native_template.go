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
	"reflect"
	"slices"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

var (
	_ values.Value = (*NativeTemplate)(nil)
)

type LiteralIndex int32

// NativeTemplate is the compiled representation of a Scheme procedure.
// It is a trusted-producer surface: the operations are not validated, see the package doc.
type NativeTemplate struct {
	parameterCount int
	valueCount     int
	isVariadic     bool

	// shape is the frame this template's body was compiled against: the local
	// parameter/internal-define structure every closure over this template
	// applies with. It lives here, not on MachineClosure, because it is a
	// property of the compiled body — one template, one shape — while the
	// environment a closure captures is per-activation. Apply reads only its
	// local half (InitApplyFrameWithParent derives everything else from the
	// runtime parent), and reads it at APPLY time rather than snapshotting it,
	// which is sound because compileClosureBody finishes populating the frame
	// before it emits the OpMakeClosure that can first observe it.
	//
	// Kept next to the fields Apply already touches (parameterCount,
	// isVariadic) so the extra load lands on a cache line the apply path has
	// warmed anyway.
	//
	// nil for a template that is not a closure body — a top-level program
	// template, or one still under construction. Both closure constructors
	// refuse a nil shape rather than deferring the fault to Apply.
	shape *environment.EnvironmentFrame

	// literals holds constant values referenced by bytecode instructions in this template.
	literals        MultipleValues
	sourceTableRefs values.SourceTableRefs  // parallel to code, index into sourceTable
	sourceTable     []*syntax.SourceContext // index 0 = nil (no source)
	name            string                  // Function name (for stack traces)
	doc             string                  // Guile-style docstring from leading string literal in body

	// freeNames records the free-variable names in free-vector slot order, as
	// compileClosureBody's Pass 1 computed them.
	//
	// Metadata, not code: EqualTo does not compare it, so it cannot affect
	// literal-pool dedup, and no generated instruction reads it. Reflection
	// (prim_reflection.go) is its consumer — a flat closure has no parent frame
	// to walk, so the names have to be recorded where the body was compiled.
	//
	// nil for a template that closes over nothing, and for every template not
	// compiled as a closure body.
	freeNames []*values.Symbol

	// freeBoxed is parallel to freeNames: slot i holds a shared *values.Box
	// rather than a copied value. nil when NO slot is boxed, which is the
	// overwhelmingly common case — a free variable is boxed only when it is both
	// captured and assigned.
	freeBoxed []bool

	// retainsLexicalEnv marks a closure body that must keep the CREATING frame
	// as its static link rather than the lexical root, because it reads through
	// the frame chain in a way the free-variable pass cannot see: an opaque
	// subtree (a quasiquote template, or a passthrough form — a `syntax`
	// template's pattern variables are the case that occurs).
	//
	// The narrowing is what makes a flat closure stop pinning its creator's
	// frames, so this flag is a per-body opt-out from the arc's whole point.
	// It is set by compileClosureBody from a transitive scan, so an enclosing
	// body inherits it from a nested one.
	retainsLexicalEnv bool

	// cachedBindings stores *Binding pointers resolved at compile time.
	// OpLoadCachedBinding/OpPushCachedBinding index into this array,
	// bypassing the runtime environment lookup path.
	cachedBindings []*environment.Binding

	// literalIndex maps hash codes to literal pool indices for O(1) amortized
	// deduplication of Hashable values. Non-hashable values fall back to
	// linear scan. Lazily initialized on first Hashable literal.
	literalIndex map[uint64][]LiteralIndex

	// sourceIndex maps a source context's identity — sourceEqual's (file, line,
	// column) triple, exactly — to its sourceTable slot, so interning is a map
	// probe rather than a scan of the table.
	//
	// The key IS the equivalence relation, so unlike literalIndex there are no
	// collision buckets to re-check. Rebuilt lazily, because Copy clones
	// sourceTable and not this map.
	sourceIndex map[sourceKey]uint32

	// Integer dispatch: all operations compiled to Instructions.
	// Ops with a dedicated opcode (all of opcode.go's waves) are direct switch
	// cases; the remaining complex ops (case-lambda closures, dynamic-wind,
	// continuation marks, box/unbox, the un-fused FFI call) are in sideTable and
	// dispatched via OpComplex.
	code      []Instruction      // bytecode instructions
	sideTable []InlinedOperation // complex ops referenced by OpComplex

	// executed tracks per-PC execution when coverage is enabled.
	// Nil means coverage is off (the default). When non-nil, its length
	// is kept equal to len(code) via AppendInstruction/AppendInstructionWithSource.
	// Writes in the VM dispatch loop are benign-racy: a bool transitioning
	// false → true does not need synchronization.
	executed []bool
}

// initialOpsCap is the pre-allocated capacity for the operations and sourceTableRefs
// slices when a template is created without initial operations (the compilation
// path). Covers simple lambda bodies without re-allocation; larger functions
// grow normally. Kept small to avoid wasting memory on the many short templates
// typical in Scheme programs.
const initialOpsCap = 8

// NewNativeTemplate assembles a template taking pcnt parameters and vcnt local slots,
// variadic when vd, over an optional initial operation stream. Trusted-producer surface:
// the operations are not validated, see the package doc.
func NewNativeTemplate(pcnt int, vcnt int, vd bool, operations ...Operation) *NativeTemplate {
	q := &NativeTemplate{
		parameterCount:  pcnt,
		valueCount:      vcnt,
		isVariadic:      vd,
		sourceTable:     []*syntax.SourceContext{nil}, // index 0 = nil (no source)
		code:            make([]Instruction, 0, initialOpsCap),
		sourceTableRefs: make([]uint32, 0, initialOpsCap),
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

// Shape returns the frame this template's body was compiled against, whose
// local half every closure over the template applies with. nil for a template
// that is not a closure body; see the field comment.
func (p *NativeTemplate) Shape() *environment.EnvironmentFrame {
	return p.shape
}

// SetShape records the frame this template's body was compiled against. Called
// once, by whoever compiled the body, before any closure over the template can
// exist — compileClosureBody for lambda and case-lambda clauses, and the two
// callers that build a template and its environment together
// (extensions/eval PrimCompile, createTransformerClosure).
func (p *NativeTemplate) SetShape(env *environment.EnvironmentFrame) {
	p.shape = env
}

func (p *NativeTemplate) ValueCount() int {
	return p.valueCount
}

func (p *NativeTemplate) IsVariadic() bool {
	return p.isVariadic
}

// IncrementParameterCount adds one to the parameter count.
func (p *NativeTemplate) IncrementParameterCount() {
	p.parameterCount++
}

// SetVariadic marks this template as accepting a variadic rest argument.
func (p *NativeTemplate) SetVariadic() {
	p.isVariadic = true
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

// instructionToOperation converts a direct instruction back to its
// corresponding Operation value. Used by Operations() for test support.
//
// Fused and promoted opcodes (emitted by peephole only) map back to the FIRST
// operation of the sequence they replaced, so tests can assert against the
// compiler's logical output independent of peephole optimization. The mapping is
// one Operation per instruction and is therefore lossy: OpPullApply drops the
// Apply, OpPushLiteral/OpPushGlobal drop the Push, and every
// OperandCachedBinding opcode collapses to LoadCachedBinding, losing both the
// call and the promoted primitive.
func instructionToOperation(instr Instruction) Operation {
	switch instr.Op {
	// --- Wave 1: zero-operand operations ---
	case OpPush:
		return NewOperationPush()
	case OpPushValues:
		return NewOperationPushValues()
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
	case OpReleaseEnvFrame:
		return NewOperationReleaseEnvFrame()
	case OpApply:
		return NewOperationApply()
	case OpUnpackListToStack:
		return NewOperationUnpackListToStack()
	case OpRestoreContinuation:
		return NewOperationRestoreContinuation()

	// --- Wave 2: single-operand operations ---
	case OpBranchOnFalseValue:
		return NewOperationBranchOnFalseValueOffsetImmediate(int(instr.Arg))
	case OpBranch:
		return NewOperationBranchOffsetImmediate(int(instr.Arg))
	case OpSaveContinuation:
		return NewOperationSaveContinuationOffsetImmediate(int(instr.Arg))
	case OpLoadLiteral, OpPushLiteral:
		return NewOperationLoadLiteralByLiteralIndexImmediate(LiteralIndex(instr.Arg))
	case OpLoadGlobal, OpPushGlobal:
		return NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(instr.Arg))
	case OpStoreGlobal:
		return NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(instr.Arg))
	case OpPeekK:
		return NewOperationPeekK(int(instr.Arg))
	case OpPushEnv:
		return NewOperationPushEnv(int(instr.Arg))
	case OpSelfTailCall:
		return NewOperationSelfTailCall(DecodeSelfTailCall(instr.Arg))

	// --- Wave 3: the LocalIdx ops with a distinct decomposition ---
	case OpStoreLocal:
		slot, depth := DecodeLocalIndex(instr.Arg)
		li := environment.NewLocalIndex(slot, depth)
		return NewOperationStoreLocalByLocalIndexImmediate(li)
	case OpBoxSlot:
		slot, depth := DecodeLocalIndex(instr.Arg)
		li := environment.NewLocalIndex(slot, depth)
		return NewOperationBoxSlot(li)
	case OpStoreThroughBox:
		slot, depth := DecodeLocalIndex(instr.Arg)
		li := environment.NewLocalIndex(slot, depth)
		return NewOperationStoreThroughBox(li)
	case OpUnbox:
		return NewOperationUnbox()
	case OpLoadFree, OpPushFree, OpCallFree:
		return NewOperationLoadFree(int(instr.Arg))
	case OpStoreFree:
		return NewOperationStoreFree(int(instr.Arg))

	// --- Wave 5: fused zero-operand ops ---
	case OpPullApply:
		return NewOperationPull()
	case OpMakeClosure:
		return NewOperationMakeClosure(DecodeMakeClosure(instr.Arg))

	default:
		// Metadata-driven decomposition for peephole-emitted opcodes.
		// All OperandCachedBinding ops decompose to LoadCachedBinding.
		// All OperandLocalIdx ops (except StoreLocal, handled above)
		// decompose to LoadLocal.
		switch opcodeTable[instr.Op].operandKind {
		case OperandCachedBinding:
			return NewOperationLoadCachedBinding(instr.Arg)
		case OperandLocalIdx:
			slot, depth := DecodeLocalIndex(instr.Arg)
			li := environment.NewLocalIndex(slot, depth)
			return NewOperationLoadLocalByLocalIndexImmediate(li)
		default:
			return nil
		}
	}
}

// SourceAt returns the source location for the operation at pc.
// Returns nil if pc is out of bounds or no source was recorded.
// O(1) lookup via the parallel sourceTableRefs array.
func (p *NativeTemplate) SourceAt(pc int) *syntax.SourceContext {
	if pc < 0 || pc >= len(p.sourceTableRefs) {
		return nil
	}
	return p.sourceTable[p.sourceTableRefs[pc]]
}

// internSource deduplicates a source context and returns its index in the sourceTable.
// Index 0 is reserved for nil (no source).
//
// One probe of sourceIndex, not a scan of sourceTable. The scan this replaces
// ran once per EMITTED INSTRUCTION against a table that grows with the compiled
// form, which made emission quadratic in program size — 61% cumulative on the
// 40,000-internal-define profile, and the older of the two compile-time
// quadratics that profile named.
//
// Interning still returns the FIRST slot holding an equal context, because
// sourceIndex only ever records the slot that created a key.
func (p *NativeTemplate) internSource(src *syntax.SourceContext) uint32 {
	if src == nil {
		return 0
	}
	p.ensureSourceIndex()
	k := newSourceKey(src)
	idx, ok := p.sourceIndex[k]
	if ok {
		return idx
	}
	if len(p.sourceTable) > math.MaxUint32 {
		panic(werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"internSource: source table overflow (%d entries)", len(p.sourceTable)))
	}
	idx = uint32(len(p.sourceTable))
	p.sourceTable = append(p.sourceTable, src)
	p.sourceIndex[k] = idx
	return idx
}

// ensureSourceIndex builds sourceIndex from sourceTable when it is absent —
// on a freshly constructed template, and after a Copy, which clones the table
// but not the index.
//
// The nil sentinel at slot 0 is skipped: sourceEqual reports nil equal to
// nothing but nil, and internSource answers a nil source before probing.
func (p *NativeTemplate) ensureSourceIndex() {
	if p.sourceIndex != nil {
		return
	}
	p.sourceIndex = make(map[sourceKey]uint32, len(p.sourceTable)+1)
	for i, s := range p.sourceTable {
		if s == nil {
			continue
		}
		k := newSourceKey(s)
		_, seen := p.sourceIndex[k]
		if seen {
			continue
		}
		p.sourceIndex[k] = uint32(i)
	}
}

// AppendOperationsWithSource converts operations to instructions and tags
// each with the given source. Operations with a dedicated opcode (all of
// opcode.go's waves) become direct switch cases; the remaining complex
// operations (case-lambda closures, dynamic-wind, continuation marks,
// box/unbox, the un-fused FFI call) go through the sideTable and are dispatched
// via OpComplex.
// This is a public method for test use.
func (p *NativeTemplate) AppendOperationsWithSource(src *syntax.SourceContext, ops ...Operation) {
	idx := p.internSource(src)
	for _, op := range ops {
		instr, ok := operationToInstruction(op)
		if !ok {
			iop, iok := op.(InlinedOperation)
			if !iok {
				panic(werr.WrapForeignErrorf(
					werr.ErrInvalidArgument,
					"AppendOperationsWithSource: operation %T does not implement InlinedOperation", op))
			}
			instr = p.AppendSideTableOp(iop)
		}
		p.code = append(p.code, instr)
		p.sourceTableRefs = append(p.sourceTableRefs, idx)
		if p.executed != nil {
			p.executed = append(p.executed, false)
		}
	}
}

// operationToInstruction converts Wave 1, Wave 2, and Wave 3 operations to direct Instructions.
// Returns (instruction, true) if the operation has a dedicated opcode,
// or (Instruction{}, false) if it should go through the sideTable.
//
// Dispatch identity (which opcode to emit) comes from op.OpKind(); the type
// switch below only handles operand extraction for the ~11 operand-bearing
// ops. Zero-operand ops fall through to the default branch.
func operationToInstruction(op Operation) (Instruction, bool) {
	kind := op.OpKind()
	if kind == OpComplex {
		return Instruction{}, false
	}
	switch v := op.(type) {
	// --- Wave 2: single-operand operations ---
	case *OperationBranchOnFalseValueOffsetImmediate:
		return Instruction{Op: kind, Arg: int32(v.Offset)}, true
	case *OperationBranchOffsetImmediate:
		return Instruction{Op: kind, Arg: int32(v.Offset)}, true
	case *OperationSaveContinuationOffsetImmediate:
		return Instruction{Op: kind, Arg: int32(v.Offset)}, true
	case *OperationLoadLiteralByLiteralIndexImmediate:
		return Instruction{Op: kind, Arg: int32(v.LiteralIndex)}, true
	case *OperationLoadGlobalByGlobalIndexLiteralIndexImmediate:
		return Instruction{Op: kind, Arg: int32(v.LiteralIndex)}, true
	case *OperationStoreGlobalByGlobalIndexLiteralIndexImmediate:
		return Instruction{Op: kind, Arg: int32(v.LiteralIndex)}, true
	case *OperationPeekK:
		return Instruction{Op: kind, Arg: int32(v.Depth)}, true
	case *OperationPushEnv:
		return Instruction{Op: kind, Arg: int32(v.SlotCount)}, true
	case *OperationSelfTailCall:
		return Instruction{Op: kind, Arg: EncodeSelfTailCall(v.ArgCount, v.PopCount)}, true

	// --- Wave 3: two-operand operations (bit-packed LocalIndex) ---
	case *OperationLoadLocalByLocalIndexImmediate:
		return Instruction{Op: kind, Arg: EncodeLocalIndex(v.LocalIndex)}, true
	case *OperationStoreLocalByLocalIndexImmediate:
		return Instruction{Op: kind, Arg: EncodeLocalIndex(v.LocalIndex)}, true
	case *OperationBoxSlot:
		return Instruction{Op: kind, Arg: EncodeLocalIndex(v.LocalIndex)}, true
	case *OperationStoreThroughBox:
		return Instruction{Op: kind, Arg: EncodeLocalIndex(v.LocalIndex)}, true

	// --- Wave 6: cached binding operations ---
	case *OperationLoadCachedBinding:
		return Instruction{Op: kind, Arg: v.BindingIndex}, true

	// --- Flat closures: packed and single-dimension operands ---
	case *OperationMakeClosure:
		return Instruction{Op: kind, Arg: EncodeMakeClosure(v.freeCount, v.selfSlot)}, true
	case *OperationLoadFree:
		return Instruction{Op: kind, Arg: int32(v.Index)}, true
	case *OperationStoreFree:
		return Instruction{Op: kind, Arg: int32(v.Index)}, true

	default:
		// Zero-operand operations (Wave 1, Wave 5 OperationMakeClosure):
		// the opcode alone fully specifies the instruction.
		//
		// Guard against silent miscompile: if a future operand-bearing op
		// is added with an OpKind() but no operand-extraction case above,
		// it would fall through here and lose its operand. Cross-check
		// against opcodeTable's operandKind classification.
		if opcodeTable[kind].operandKind != OperandNone {
			panic(werr.WrapForeignErrorf(
				werr.ErrInvalidArgument,
				"operationToInstruction: %T has OpKind=%s with operand but no extraction case", op, kind))
		}
		return Instruction{Op: kind}, true
	}
}

// sourceKey is a source context reduced to what sourceEqual compares, so that
// map equality on the key and sourceEqual agree by construction.
//
// Keep the two in step: a field added to one is a field the other needs.
type sourceKey struct {
	file string
	line int
	col  int
}

// newSourceKey projects a non-nil source context onto its identity.
func newSourceKey(src *syntax.SourceContext) sourceKey {
	return sourceKey{
		file: src.File,
		line: src.Start.Line(),
		col:  src.Start.Column(),
	}
}

// sourceEqual compares two source contexts for equality (by location only),
// extending sourceKey's relation to nil: nil is equal to nothing but nil, which
// is why slot 0's sentinel never interns.
func sourceEqual(a, b *syntax.SourceContext) bool {
	if a == nil || b == nil {
		return a == b
	}
	return newSourceKey(a) == newSourceKey(b)
}

func (p *NativeTemplate) Name() string {
	return p.name
}

func (p *NativeTemplate) SetName(name string) {
	p.name = name
}

func (p *NativeTemplate) Doc() string {
	return p.doc
}

func (p *NativeTemplate) SetDoc(doc string) {
	p.doc = doc
}

// SetFreeLayout records the free-variable names in free-vector slot order and,
// parallel to them, which slots are boxed. Written once, by compileClosureBody,
// before the body is compiled. boxed may be nil when no slot is boxed; when it
// is not nil it must have the same length as names.
func (p *NativeTemplate) SetFreeLayout(names []*values.Symbol, boxed []bool) {
	if boxed != nil && len(boxed) != len(names) {
		panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"SetFreeLayout: %d boxed flags against %d free names", len(boxed), len(names)))
	}
	p.freeNames = names
	p.freeBoxed = boxed
}

// FreeNames returns the free-variable names in free-vector slot order. nil for
// a template that closes over nothing, and for every template not compiled as a
// closure body. The slice is the template's own — treat it as read-only.
func (p *NativeTemplate) FreeNames() []*values.Symbol {
	return p.freeNames
}

// FreeBoxed reports, per free-vector slot, whether that slot holds a shared box
// rather than a copied value. nil when no slot is boxed. Read-only.
func (p *NativeTemplate) FreeBoxed() []bool {
	return p.freeBoxed
}

// SetRetainsLexicalEnv records that closures over this template must keep the
// creating frame as their static link. See the field comment.
func (p *NativeTemplate) SetRetainsLexicalEnv() {
	p.retainsLexicalEnv = true
}

// RetainsLexicalEnv reports whether closures over this template must keep the
// creating frame as their static link rather than the lexical root.
func (p *NativeTemplate) RetainsLexicalEnv() bool {
	return p.retainsLexicalEnv
}

func (p *NativeTemplate) MaybeAppendLiteral(v values.Value) LiteralIndex {
	// Don't deduplicate environments - each closure needs its own instance
	// because environments are mutable and context-dependent. The parent
	// chain is set at runtime by OperationMakeClosure, so structural
	// equality doesn't capture lexical context.
	_, isEnv := v.(*environment.EnvironmentFrame)
	if isEnv {
		l := len(p.literals)
		p.literals = append(p.literals, v)
		return LiteralIndex(l)
	}

	// Hash-indexed path for Hashable values (Symbol, Integer, String, etc.)
	h, ok := v.(values.Hashable)
	if ok {
		hash := h.HashCode()
		// Lazily build literalIndex from existing literals if it hasn't
		// been initialized yet (e.g., after Copy() which clones literals
		// but not the index).
		if p.literalIndex == nil {
			p.literalIndex = make(map[uint64][]LiteralIndex, len(p.literals)/2+1)
			for i, lit := range p.literals {
				hLit, okLit := lit.(values.Hashable)
				if okLit {
					litHash := hLit.HashCode()
					p.literalIndex[litHash] = append(p.literalIndex[litHash], LiteralIndex(i))
				}
			}
		}
		for _, idx := range p.literalIndex[hash] {
			if literalIdentical(p.literals[idx], v) {
				return idx
			}
		}
		l := len(p.literals)
		p.literals = append(p.literals, v)
		p.literalIndex[hash] = append(p.literalIndex[hash], LiteralIndex(l))
		return LiteralIndex(l)
	}

	// Linear fallback for non-hashable values
	for i, lit := range p.literals {
		if literalIdentical(lit, v) {
			return LiteralIndex(i)
		}
	}
	l := len(p.literals)
	p.literals = append(p.literals, v)
	return LiteralIndex(l)
}

// literalIdentical reports whether two values are interchangeable for literal-pool
// deduplication. It is THE pool's only equality predicate, and MaybeAppendLiteral
// is its only caller — both of its arms, the hash-bucketed one and the linear
// fallback for non-hashable values.
//
// It is STRICTER than EqualTo in exactly one way: KIND. Two values of different
// concrete types never dedup, even when they are eqv?. An Integer 1 and a BigInteger 1
// ARE the same number under R7RS §6.1, but they dispatch to different arithmetic, and
// merging them RE-TYPES the literal. Same for Rational 1/1. reflect.TypeOf is preferred
// over a type switch enumerating the numeric kinds precisely because it cannot rot when
// a new values.Value type is added. It runs once per literal at COMPILE time, never in
// the VM loop.
//
// Everything else is delegated to EqualTo, which routes to values.EqvNumber — the single
// authority on numeric equivalence (see values/eqv.go). This used to hand-roll a Float
// arm, `af.Value == bf.Value && Signbit == Signbit`, and eqv.go's own doc named it as
// one of the three drifted copies of the numeric rule. It was never unified, and by the
// end it DISAGREED with EqualTo: `NaN == NaN` is false in IEEE, so two +nan.0 literals
// were kept apart, while EqualTo now (correctly, per Chez) calls them eqv?. Both of its
// stated jobs are already done, and done better, by EqvNumber:
//
//   - Signed zero: EqvNumber consults SignBit precisely because IEEE says 0.0 == -0.0.
//     (eqv? 0.0 -0.0) is #f, so the two stay separate literals, and atan2 keeps its sign.
//   - Float vs BigFloat: EqvNumber separates inexact numbers by Kind, so they are never
//     eqv? and can never merge — the reflect gate above is belt-and-braces for them.
//
// Merging two NaN literals is SOUND, and is what now happens: every NaN is eqv? to every
// other, so a program cannot tell the merged pool from the unmerged one.
func literalIdentical(a, b values.Value) bool {
	if reflect.TypeOf(a) != reflect.TypeOf(b) {
		return false
	}
	return a.EqualTo(b)
}

// AppendCachedBinding adds a *Binding to the cached bindings array,
// deduplicating by pointer identity. Returns the index for use as
// an OpLoadCachedBinding/OpPushCachedBinding operand.
func (p *NativeTemplate) AppendCachedBinding(bd *environment.Binding) int32 {
	for i, existing := range p.cachedBindings {
		if existing == bd {
			return int32(i)
		}
	}
	idx := int32(len(p.cachedBindings))
	p.cachedBindings = append(p.cachedBindings, bd)
	return idx
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
	p.sourceTableRefs = append(p.sourceTableRefs, idx)
	if p.executed != nil {
		p.executed = append(p.executed, false)
	}
}

// AppendInstruction appends a single instruction with no source attribution.
func (p *NativeTemplate) AppendInstruction(instr Instruction) {
	p.code = append(p.code, instr)
	p.sourceTableRefs = append(p.sourceTableRefs, 0)
	if p.executed != nil {
		p.executed = append(p.executed, false)
	}
}

// AppendSideTableOp adds a complex operation to the side table and returns
// an OpComplex instruction that references it.
func (p *NativeTemplate) AppendSideTableOp(op InlinedOperation) Instruction {
	idx := int32(len(p.sideTable))
	p.sideTable = append(p.sideTable, op)
	return Instruction{Op: OpComplex, Arg: idx}
}

// Literals returns the literals pool.
func (p *NativeTemplate) Literals() MultipleValues {
	return p.literals
}

// Code returns the integer-dispatch bytecode slice.
func (p *NativeTemplate) Code() []Instruction {
	return p.code
}

// SideTable returns the complex operations referenced by OpComplex instructions.
func (p *NativeTemplate) SideTable() []InlinedOperation {
	return p.sideTable
}

// CachedBindings returns the compile-time resolved bindings array.
// Used by the disassembler to annotate binding references.
func (p *NativeTemplate) CachedBindings() []*environment.Binding {
	return p.cachedBindings
}

// CodeLen returns the current code[] length (number of instructions emitted).
func (p *NativeTemplate) CodeLen() int {
	return len(p.code)
}

// EnableCoverage allocates the per-PC executed array (if not already allocated)
// so the VM dispatch loop will record executions. Length is kept parallel to
// code via AppendInstruction. Idempotent: safe to call multiple times; an
// existing array is preserved.
func (p *NativeTemplate) EnableCoverage() {
	if p.executed != nil {
		return
	}
	p.executed = make([]bool, len(p.code))
}

// Executed returns the per-PC executed array, or nil if coverage is disabled.
// Returned slice aliases internal state; callers must not resize it.
func (p *NativeTemplate) Executed() []bool {
	return p.executed
}

// IsCoverageEnabled reports whether coverage tracking is active on this template.
func (p *NativeTemplate) IsCoverageEnabled() bool {
	return p.executed != nil
}

// PatchInstructionArg updates the Arg field of the instruction at code[codeIdx].
// Used for patching branch offsets and continuation save offsets after the
// target PC is known.
//
// An out-of-range codeIdx panics rather than returning an error, and arg is not
// checked against the opcode's operand domain: a well-formed-looking patch can
// defer the panic to the dispatch loop. Trusted-producer surface, see the package doc.
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
	if len(p.code) != len(p.sourceTableRefs) {
		panic(werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"native_template: code/sourceTableRefs length invariant violated (len(code)=%d, len(sourceTableRefs)=%d)",
			len(p.code), len(p.sourceTableRefs),
		))
	}
	if p.executed != nil && len(p.executed) != len(p.code) {
		panic(werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"native_template: code/executed length invariant violated (len(code)=%d, len(executed)=%d)",
			len(p.code), len(p.executed),
		))
	}
	q := &NativeTemplate{
		parameterCount: p.parameterCount,
		valueCount:     p.valueCount,
		isVariadic:     p.isVariadic,
		name:           p.name,
		doc:            p.doc,
	}
	q.literals = slices.Clone(p.literals)
	q.cachedBindings = slices.Clone(p.cachedBindings)
	q.code = slices.Clone(p.code)
	q.sideTable = slices.Clone(p.sideTable)
	q.sourceTableRefs = slices.Clone(p.sourceTableRefs)
	q.sourceTable = slices.Clone(p.sourceTable)
	q.executed = slices.Clone(p.executed)
	// freeNames describes the compiled body, like code and literals, so it is
	// cloned with them. (shape is deliberately NOT copied — see its field
	// comment; a copy is not yet any closure's body.)
	q.freeNames = slices.Clone(p.freeNames)
	q.freeBoxed = slices.Clone(p.freeBoxed)
	q.retainsLexicalEnv = p.retainsLexicalEnv
	return q
}
