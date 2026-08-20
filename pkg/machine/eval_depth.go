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

// eval_depth.go answers ONE question for the call-fusion passes: is the eval
// stack provably EMPTY on entry to this instruction?
//
// WHY A FUSION PASS NEEDS IT. OpPullApply pulls its callee from the BOTTOM of
// the eval stack (Stack.PullDrain), so the callee is whatever was pushed first
// since the group began. A fusion that rewrites the apply into OpCallLocal /
// OpCallCachedBinding / OpCallForeignCached names the callee by operand
// instead, and is therefore correct only if the push it deleted really was that
// bottom element.
//
// The guard that used to stand in for this — "the previous instruction is
// neither SaveContinuation nor a push" — is not the same question, and the gap
// is reachable from ordinary Scheme. In
//
//	(lambda (a n) (+ (begin (set! a 1) a) n))
//
// the group opens with the `+` push, the set! then POPS (OpStoreLocal), and the
// push that follows looked like a fresh group start. The apply was rewritten to
// call `a`, and evaluating it reported "expected a procedure, got 1".
//
// FAIL-SAFE BY CONSTRUCTION. Every unmodelled opcode, every merge of two
// disagreeing depths and every unreachable index answers depthUnknown, which
// refuses the fusion. Only the deltas actually written below have to be right;
// nothing here can turn a missing fact into a fusion.

const (
	// depthUnreached marks an index the forward walk has not reached yet.
	depthUnreached = -2
	// depthUnknown marks an index whose depth the analysis will not pin: an
	// unmodelled opcode reached it, or two paths reached it disagreeing.
	depthUnknown = -1
)

// promotedStackDelta holds -arity for every promoted opcode and 0 elsewhere.
//
// Built from the promotedOps descriptors rather than written out, so promoting
// a new primitive cannot leave this table stale — which would be a miscompile
// rather than a missed fusion, the one direction this file must not fail in.
var promotedStackDelta = buildPromotedStackDelta()

func buildPromotedStackDelta() [opCount]int8 {
	var q [opCount]int8
	for _, op := range promotedOps {
		q[op.nonTail] = int8(-op.arity)
		q[op.tail] = int8(-op.arity)
	}
	return q
}

// stackDelta reports how instr changes the eval stack's height, and whether
// that is known at all.
//
// Only ops that fall through to the next instruction need an answer here.
// Terminators are handled by evalStackDepths, which gives them no successor:
// an apply drains the group and either returns through its continuation — whose
// return point the enclosing OpSaveContinuation supplies at the caller's own
// depth — or does not return to this template at all.
func stackDelta(instr Instruction) (int, bool) {
	if promotedStackDelta[instr.Op] != 0 {
		return int(promotedStackDelta[instr.Op]), true
	}
	switch instr.Op {
	case OpPush, OpPushLiteral, OpPushGlobal, OpPushLocal, OpPushCachedBinding, OpPushFree:
		return 1, true
	case OpPop, OpPull, OpDrop, OpStoreLocal, OpStoreGlobal, OpStoreThroughBox, OpStoreFree:
		return -1, true
	case OpMakeClosure:
		// popMakeClosureArgs takes the template plus one value per free slot.
		freeCount, _ := DecodeMakeClosure(instr.Arg)
		return -(freeCount + 1), true
	case OpLoadVoid, OpLoadLiteral, OpLoadGlobal, OpLoadLocal, OpLoadFree, OpLoadCachedBinding,
		OpPeekK, OpBoxSlot, OpUnbox, OpPushEnv, OpPopEnv, OpReleaseEnvFrame,
		OpBranchOnFalseValue:
		// Value register or environment only.
		return 0, true
	default:
		// OpComplex, OpUnpackListToStack, OpPushValues and anything added later.
		return 0, false
	}
}

// isEvalStackTerminator reports whether instr hands control somewhere this
// walk must not follow with a fallthrough edge.
//
// The applies are terminators even in the inline-foreign case that continues at
// pc+1: applyForeign restores the continuation first, so the depth at that
// index is the caller's, which the enclosing OpSaveContinuation already
// contributes. Following BOTH edges would merge the same fact twice and, when
// the fusion has already rewritten the apply, once with the wrong height.
func isEvalStackTerminator(op OpCode) bool {
	if promotedStackDelta[op] != 0 && opIsTailPromoted(op) {
		return true
	}
	switch op {
	case OpApply, OpPullApply, OpCallLocal, OpCallCachedBinding, OpCallFree,
		OpCallForeignCached, OpCallForeignCachedTail,
		OpRestoreContinuation, OpSelfTailCall:
		return true
	default:
		return false
	}
}

// opIsTailPromoted reports whether op is the TAIL half of a promoted primitive,
// which returns rather than falling through.
func opIsTailPromoted(op OpCode) bool {
	for _, p := range promotedOps {
		if p.tail == op {
			return true
		}
	}
	return false
}

// evalStackDepths returns the eval-stack depth on ENTRY to each instruction of
// code, or depthUnknown where it cannot be pinned.
//
// A worklist rather than a linear scan, because a branch target is reached from
// two places and the two must agree before either is trusted. Templates are
// small and every index is re-queued only when its answer weakens (known ->
// unknown), so the walk terminates after at most two visits per edge.
func evalStackDepths(code []Instruction) []int {
	q := make([]int, len(code))
	for i := range q {
		q[i] = depthUnreached
	}
	if len(code) == 0 {
		return q
	}

	var work []int
	visit := func(idx, d int) {
		if idx < 0 || idx >= len(code) {
			return
		}
		cur := q[idx]
		if cur == depthUnknown {
			return
		}
		if cur == depthUnreached {
			q[idx] = d
			work = append(work, idx)
			return
		}
		if cur != d {
			// Two paths disagree, or one of them was already unknown.
			q[idx] = depthUnknown
			work = append(work, idx)
		}
	}

	visit(0, 0)
	for len(work) > 0 {
		i := work[len(work)-1]
		work = work[:len(work)-1]
		d := q[i]
		instr := code[i]

		if instr.Op == OpSaveContinuation {
			// Two successors, and they are on DIFFERENT stacks. The callee
			// starts at pc+1 with a cleared stack; the return point at pc+off
			// resumes on the caller's, restored to exactly this depth.
			visit(i+1, 0)
			visit(i+int(instr.Arg), d)
			continue
		}
		if isEvalStackTerminator(instr.Op) {
			continue
		}
		if instr.Op == OpBranch {
			visit(i+int(instr.Arg), d)
			continue
		}

		delta, known := stackDelta(instr)
		next := depthUnknown
		if known && d != depthUnknown {
			next = d + delta
			if next < 0 {
				// A negative height means the model and the bytecode disagree;
				// refuse to answer rather than to guess low.
				next = depthUnknown
			}
		}
		visit(i+1, next)
		if instr.Op == OpBranchOnFalseValue {
			visit(i+int(instr.Arg), next)
		}
	}
	return q
}

// evalStackEmptyAt reports whether the eval stack is PROVABLY empty on entry to
// code[i] — the precondition for treating a push there as a call group's
// callee. Unreached and unknown both answer false.
func evalStackEmptyAt(depths []int, i int) bool {
	if i < 0 || i >= len(depths) {
		return false
	}
	return depths[i] == 0
}
