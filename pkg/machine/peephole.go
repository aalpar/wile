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

import "github.com/aalpar/wile/pkg/values"

// Optimize performs a peephole optimization pass on the template's bytecode.
// It removes dead instructions identified by pattern-matching rules, fixes
// branch offsets to account for removed instructions, and compacts the code
// and source reference arrays in parallel.
//
// Superinstruction formation (Ertl & Gregg 2003). Fusing adjacent
// instructions reduces dispatch overhead in the Run() switch loop.
//
//	Four EditPlan passes, each applied before the next matches on its output:
//	  1. markDeadLoadVoidEdits, fuseLoadPush, fusePullApply
//	       Load* + Push  → Push*       (eliminates 1 dispatch)
//	       Pull + Apply  → PullApply   (eliminates 1 dispatch)
//	  2. fuseCallForeignCached
//	       PushCachedBinding ... PullApply → CallForeignCached
//	         (SaveCont retained; eliminates ~5 dispatches)
//	       or, for a promoted primitive, one of the 34 promoted opcodes
//	  3. fuseCallGeneric → CallLocal / CallCachedBinding
//	  4. fusePromotedCompoundArgs (gated on a preceding OpReleaseEnvFrame)
//
//	Cost model: each fusion saves one (fetch opcode + switch branch).
//	  In switch-dispatch interpreters, dispatch dominates execution time.
//
//	Invariant: fusions must not change observable semantics. Branch
//	  offsets are recomputed after compaction.
//	Constrains: Run() must implement fused opcodes with identical
//	  semantics to the original sequence.
//	Constrained by: must preserve semantic equivalence of fused
//	  instruction sequences.
//
// See BIBLIOGRAPHY.md "Superinstruction Formation".
//
// After compacting its own code, Optimize recurses into any *NativeTemplate
// values in the literals pool (lambda closures compiled as sub-templates).
//
// Idempotent: a second call finds nothing to remove.
func (p *NativeTemplate) Optimize() {
	n := len(p.code)
	if n == 0 {
		p.optimizeSubTemplates()
		return
	}

	plan := NewEditPlan(p)
	markDeadLoadVoidEdits(p.code, plan)
	fuseLoadPush(p.code, p.sourceTableRefs, plan)
	fusePullApply(p.code, p.sourceTableRefs, plan)
	plan.Apply()

	plan2 := NewEditPlan(p)
	fuseCallForeignCached(p, plan2)
	plan2.Apply()

	plan3 := NewEditPlan(p)
	fuseCallGeneric(p, plan3)
	plan3.Apply()

	plan4 := NewEditPlan(p)
	fusePromotedCompoundArgs(p, plan4)
	plan4.Apply()

	p.optimizeSubTemplates()
}

// optimizeSubTemplates recurses into any *NativeTemplate values stored
// in the literals pool. These are sub-templates for lambda closures.
func (p *NativeTemplate) optimizeSubTemplates() {
	for _, lit := range p.literals {
		sub, ok := lit.(*NativeTemplate)
		if ok {
			sub.Optimize()
		}
	}
}

// writesValueRegister returns true for opcodes that unconditionally write
// to the value register without reading it first, making a preceding
// LoadVoid dead. Derived from opcodeTable metadata.
func writesValueRegister(op OpCode) bool {
	return opcodeTable[op].writesValue
}

// markDeadLoadVoidEdits scans code[0..len-2] and adds Delete edits for LoadVoid
// instructions immediately followed by an opcode that unconditionally writes the
// value register (opcodeTable.writesValue).
func markDeadLoadVoidEdits(code []Instruction, plan *EditPlan) {
	for i := 0; i < len(code)-1; i++ {
		if code[i].Op == OpLoadVoid && writesValueRegister(code[i+1].Op) {
			plan.Delete(i, i+1)
		}
	}
}

// loadToFusedPush maps Load opcodes to their fused Push equivalents.
var loadToFusedPush = [opCount]OpCode{
	OpLoadLiteral:       OpPushLiteral,
	OpLoadGlobal:        OpPushGlobal,
	OpLoadLocal:         OpPushLocal,
	OpLoadCachedBinding: OpPushCachedBinding,
	OpLoadFree:          OpPushFree,
}

// fuseLoadPush scans for LoadLiteral+Push, LoadGlobal+Push, LoadLocal+Push, and
// LoadCachedBinding+Push pairs and adds Replace edits that fuse them into single
// PushLiteral, PushGlobal, PushLocal, or PushCachedBinding instructions. The
// fused instruction inherits the source attribution from the Load instruction.
// PushCachedBinding is the input every later call-fusion pass matches on.
//
// A Load+Push pair is NOT fused if the Push is a branch target, because
// the Push may be a convergence point for multiple control flow paths
// (e.g., both branches of an `if` expression that share a Push to push
// the result). Fusing would bind the Push to only the Load's value.
func fuseLoadPush(code []Instruction, sourceTableRefs []uint32, plan *EditPlan) {
	targets := branchTargets(code)
	for i := 0; i < len(code)-1; i++ {
		fused := loadToFusedPush[code[i].Op]
		if fused != OpInvalid && code[i+1].Op == OpPush && !targets[i+1] {
			plan.Replace(i, i+2,
				[]Instruction{{Op: fused, Arg: code[i].Arg}},
				sourceTableRefs[i],
			)
			i++ // skip the Push
		}
	}
}

// fusePullApply fuses Pull + Apply into a single OpPullApply instruction.
// Every function call emits this pair; fusing saves one dispatch per call.
func fusePullApply(code []Instruction, sourceTableRefs []uint32, plan *EditPlan) {
	targets := branchTargets(code)
	for i := 0; i < len(code)-1; i++ {
		if code[i].Op == OpPull && code[i+1].Op == OpApply && !targets[i+1] {
			plan.Replace(i, i+2,
				[]Instruction{{Op: OpPullApply}},
				sourceTableRefs[i],
			)
			i++ // skip the Apply
		}
	}
}

// branchTargets returns a set of instruction positions that are targeted
// by branch instructions. Used to prevent fusing Load+Push when the Push
// is a convergence point for multiple control flow paths.
//
// Branch targets beyond [0, len(code)) are silently discarded by the
// bounds check. This is safe because fuseLoadPush only iterates
// [0, len(code)-1], so out-of-bounds positions (e.g., a branch
// targeting len(code) for fall-through) are never consulted.
func branchTargets(code []Instruction) []bool {
	targets := make([]bool, len(code))
	for i, instr := range code {
		if isBranchOp(instr.Op) {
			target := i + int(instr.Arg)
			if target >= 0 && target < len(code) {
				targets[target] = true
			}
		}
	}
	return targets
}

// isPushOp returns true for opcodes that push a value onto the eval stack
// without side effects relevant to call fusion. Used by every call-fusion pass
// (fuseCallForeignCached, fuseCallGeneric, fusePromotedCompoundArgs, and
// walkCallArgs) to identify argument-pushing instructions between the callee
// load and apply.
func isPushOp(op OpCode) bool {
	switch op {
	case OpPush, OpPushLiteral, OpPushGlobal, OpPushLocal, OpPushCachedBinding, OpPushFree:
		return true
	default:
		return false
	}
}

// releaseSafeCallee reports whether a callee pushed by op can still be resolved
// AFTER an OpReleaseEnvFrame has run.
//
// This is the one question a tail fusion has to answer once frame reclaim is
// armed inside a merged `let` body. The fusion deletes the callee push and
// resolves the callee at the apply instead, which MOVES that resolution across
// anything in between. OpReleaseEnvFrame hands mc.env to the pool and
// ResetForPool zeroes the frame while mc.env still points at it, so the move is
// sound only for a callee whose resolution never reads mc.env:
//
//   - OpPushCachedBinding reads tpl.cachedBindings[i], a *Binding resolved at
//     compile time and owned by the template.
//   - OpPushFree reads mc.free[i], the running closure's own free vector, which
//     a flat closure copied by value at creation.
//   - OpPushLocal resolves through mc.env (resolveLocalBinding) and is NOT safe.
//     Its call keeps the unfused Push…/PullApply shape and pays one extra
//     dispatch, which is the cheaper half of the trade against a pooled frame.
//
// Emitting the release is codegen's decision and its proof (no capture, no
// escaping closure, only capture-safe callees); this predicate only decides
// whether a fusion may step over one.
func releaseSafeCallee(op OpCode) bool {
	switch op {
	case OpPushCachedBinding, OpPushFree:
		return true
	default:
		return false
	}
}

// scanTailCallArgs finds the OpPullApply closing a tail call whose callee push
// sits at start-1, and reports how many ARGUMENT pushes precede it.
//
// allowRelease admits the OpReleaseEnvFrame that frame reclaim emits after the
// last argument push and before the apply; the caller decides that from
// releaseSafeCallee. The release is not an argument, which is why the count is
// returned rather than recomputed as pullIdx-start by the caller — a promoted
// primitive's arity check reads it, and an off-by-one there would fuse a
// 2-argument op onto 3 stack entries.
//
// Returns -1 when the run hits anything else first.
func scanTailCallArgs(code []Instruction, start int, allowRelease bool) (pullIdx, argCount int) {
	for k := start; k < len(code); k++ {
		switch {
		case code[k].Op == OpPullApply:
			return k, argCount
		case isPushOp(code[k].Op):
			argCount++
		case allowRelease && code[k].Op == OpReleaseEnvFrame:
		default:
			return -1, 0
		}
	}
	return -1, 0
}

// fuseCallForeignCached rewrites PushCachedBinding...PullApply sequences
// into direct OpCallForeignCached / OpCallForeignCachedTail instructions
// when the cached binding holds a *ForeignClosure. This eliminates the
// Pull+Apply dispatch overhead for known-primitive calls.
//
// This runs as a second pass after fuseLoadPush and fusePullApply have
// already been applied, so it matches post-fusion opcodes.
//
// Non-tail pattern (preceded by SaveContinuation):
//
//	code[i]       = SaveContinuation(off)
//	code[i+1]     = PushCachedBinding(idx)   -- binding holds *ForeignClosure
//	... 0+ Push-family ops ...
//	code[i+off-1] = PullApply                -- SaveCont offset targets return point (one past here)
//
// Rewrite: delete PushCachedBinding (keep SaveContinuation for stack
// isolation), replace PullApply with OpCallForeignCached(idx). The runtime
// restores the continuation after the call to recover the caller's state.
//
// Promoted primitives (the promotedOps descriptors, matched by
// PrimitiveIdentity: predicates, car/cdr, vector-ref, cons, set-cdr!, and 2-arg
// arithmetic/comparison) delete both SaveCont and PushCachedBinding because they
// use fixed Pop() counts instead of Drain().
//
// Tail pattern (no preceding SaveContinuation):
//
//	code[i]   = PushCachedBinding(idx)     -- binding holds *ForeignClosure
//	... 0+ Push-family ops ...
//	code[j]   = PullApply
//
// Rewrite: delete PushCachedBinding, replace PullApply with
// OpCallForeignCachedTail(idx).
func fuseCallForeignCached(tpl *NativeTemplate, plan *EditPlan) {
	code := tpl.code
	if len(code) < 2 {
		return
	}
	targets := branchTargets(code)
	depths := evalStackDepths(code)

	// Track PullApply indices already claimed by a fusion to prevent
	// the same PullApply from being matched by both non-tail and tail
	// patterns (e.g., when argument-pushing PushCachedBinding follows
	// the callee PushCachedBinding that was already fused).
	claimed := values.NewIntSet(0)

	for i := range len(code) {
		// Non-tail pattern: SaveContinuation + PushCachedBinding ... PullApply
		if code[i].Op == OpSaveContinuation &&
			i+1 < len(code) &&
			code[i+1].Op == OpPushCachedBinding {
			off := int(code[i].Arg)
			pullIdx := i + off - 1
			bindingIdx := code[i+1].Arg

			// SaveCont offset targets the return point (one past PullApply).
			if pullIdx < 0 || pullIdx >= len(code) || code[pullIdx].Op != OpPullApply {
				continue
			}

			// Skip if this PullApply was already claimed.
			done := claimed.Get(pullIdx)
			if done {
				continue
			}

			// Verify the binding holds a *ForeignClosure.
			if int(bindingIdx) >= len(tpl.cachedBindings) {
				continue
			}
			fcls, ok := tpl.cachedBindings[bindingIdx].Value().(*ForeignClosure)
			if !ok {
				continue
			}

			// No branch target in the interior (i+1, pullIdx), exclusive of
			// pullIdx itself.
			//
			// The justification for excluding pullIdx is NOT that SaveCont cannot
			// target it arithmetically — it can. It is that the region [i+2, pullIdx)
			// is required to be entirely push-family (see the allPush guard below),
			// and none of the three isBranch opcodes (OpBranch, OpBranchOnFalseValue,
			// OpSaveContinuation) is push-family. A branch landing on pullIdx from
			// outside would therefore be jumping onto an Apply having bypassed the
			// callee push that PullApply exists to consume — already broken before
			// any fusion. Verified empirically: instrumenting Optimize() across all
			// packages plus the Scheme corpus yields zero occurrences.
			//
			// Widening the scan to <= pullIdx would be free, harmless hardening if a
			// future change relaxes the all-push guard. It is not a bug fix.
			branchInInterior := false
			for k := i + 1; k < pullIdx; k++ {
				if targets[k] {
					branchInInterior = true
					break
				}
			}
			if branchInInterior {
				continue
			}

			// All instructions between PushCachedBinding and PullApply
			// must be Push-family.
			allPush := true
			for k := i + 2; k < pullIdx; k++ {
				if !isPushOp(code[k].Op) {
					allPush = false
					break
				}
			}
			if !allPush {
				continue
			}

			// Check for promoted primitive: inline the hot primitive logic
			// directly, bypassing arity check and indirect function call.
			argCount := pullIdx - (i + 2)
			promotedOp, _, promotedArity := promotedOpForIdentity(fcls.identity)
			if promotedOp != OpInvalid && argCount == promotedArity {
				plan.Delete(i, i+2)
				plan.Replace(pullIdx, pullIdx+1,
					[]Instruction{{Op: promotedOp, Arg: bindingIdx}},
					tpl.sourceTableRefs[pullIdx],
				)
				claimed.Set(pullIdx)
				continue
			}

			// Delete only PushCachedBinding [i+1, i+2). Keep SaveContinuation
			// for stack isolation — CallForeignCached uses Drain() which
			// needs the saved evals boundary, and RestoreAndRelease to
			// recover the caller's env/template/pc after the call.
			plan.Delete(i+1, i+2)
			// Replace PullApply with OpCallForeignCached
			plan.Replace(pullIdx, pullIdx+1,
				[]Instruction{{Op: OpCallForeignCached, Arg: bindingIdx}},
				tpl.sourceTableRefs[pullIdx],
			)
			claimed.Set(pullIdx)
			continue
		}

		// Tail pattern: PushCachedBinding opening an empty group ... PullApply
		if code[i].Op == OpPushCachedBinding {
			// Preceded by SaveContinuation: the non-tail pattern above owns
			// this sequence. Skipping it here is DEDUPLICATION, not a
			// soundness condition — the depth test below passes there too,
			// since SaveContinuation hands the callee an empty stack, and both
			// passes would then delete the same push.
			if i > 0 && code[i-1].Op == OpSaveContinuation {
				continue
			}
			// The PushCachedBinding must be the CALLEE, which OpPullApply reads
			// from the BOTTOM of the eval stack — so the stack must be empty
			// here. This subsumes the old "predecessor is not a push" test and
			// closes the gap that test left; see eval_depth.go.
			if !evalStackEmptyAt(depths, i) {
				continue
			}

			bindingIdx := code[i].Arg

			// Verify the binding holds a *ForeignClosure.
			if int(bindingIdx) >= len(tpl.cachedBindings) {
				continue
			}
			fcls, ok := tpl.cachedBindings[bindingIdx].Value().(*ForeignClosure)
			if !ok {
				continue
			}

			// Scan forward for PullApply, allowing Push-family ops and the
			// frame-reclaim release — a cached binding is resolved off the
			// template, so releasing mc.env first cannot invalidate it
			// (releaseSafeCallee).
			pullIdx, argCount := scanTailCallArgs(code, i+1, true)
			if pullIdx < 0 {
				continue
			}

			// Skip if this PullApply was already claimed.
			done := claimed.Get(pullIdx)
			if done {
				continue
			}

			// No branch target in range [i, pullIdx].
			branchInInterior := false
			for k := i; k <= pullIdx; k++ {
				if targets[k] {
					branchInInterior = true
					break
				}
			}
			if branchInInterior {
				continue
			}

			// Check for promoted primitive (tail variant).
			_, promotedTailOp, promotedArity := promotedOpForIdentity(fcls.identity)
			if promotedTailOp != OpInvalid && argCount == promotedArity {
				plan.Delete(i, i+1)
				plan.Replace(pullIdx, pullIdx+1,
					[]Instruction{{Op: promotedTailOp, Arg: bindingIdx}},
					tpl.sourceTableRefs[pullIdx],
				)
				claimed.Set(pullIdx)
				continue
			}

			// Delete PushCachedBinding [i, i+1)
			plan.Delete(i, i+1)
			// Replace PullApply with OpCallForeignCachedTail
			plan.Replace(pullIdx, pullIdx+1,
				[]Instruction{{Op: OpCallForeignCachedTail, Arg: bindingIdx}},
				tpl.sourceTableRefs[pullIdx],
			)
			claimed.Set(pullIdx)
			continue
		}
	}
}

// calleeToCallOp maps the opcode that pushes the callee to the fused
// call opcode that replaces PullApply. Only push-family opcodes that
// can appear as the first push in a call sequence are included.
var calleeToCallOp = [opCount]OpCode{
	OpPushLocal:         OpCallLocal,
	OpPushCachedBinding: OpCallCachedBinding,
	// Calling a CAPTURED procedure is the common case in higher-order code, so
	// leaving this row out would silently deoptimize exactly the workloads flat
	// closures target — and nothing but the arc's site-count ratchet would say so.
	OpPushFree: OpCallFree,
}

// fuseCallGeneric rewrites PushLocal/PushCachedBinding...PullApply sequences
// into OpCallLocal / OpCallCachedBinding instructions. This handles the
// general case (any callable) after fuseCallForeignCached has already claimed
// the foreign-closure-specific patterns.
//
// This pass runs third, on post-pass-2 bytecode where some PullApply have
// already been replaced by CallForeignCached/CallForeignCachedTail/promoted ops.
//
// Non-tail pattern:
//
//	code[i]       = SaveContinuation(off)
//	code[i+1]     = PushLocal(arg) or PushCachedBinding(arg)   -- callee
//	... 0+ Push-family ops ...
//	code[i+off-1] = PullApply                                  -- SaveCont targets return point (one past here)
//
// Rewrite: delete callee push, replace PullApply with CallLocal/CallCachedBinding(arg).
// SaveContinuation is preserved (needed for non-foreign callables).
//
// Tail pattern:
//
//	code[i] = PushLocal(arg) or PushCachedBinding(arg)     -- callee, not preceded by SaveCont/push
//	... 0+ Push-family ops ...
//	code[j] = PullApply
//
// Rewrite: delete callee push, replace PullApply with CallLocal/CallCachedBinding(arg).
func fuseCallGeneric(tpl *NativeTemplate, plan *EditPlan) {
	code := tpl.code
	if len(code) < 2 {
		return
	}
	targets := branchTargets(code)
	depths := evalStackDepths(code)

	for i := range len(code) {
		// Non-tail pattern: SaveContinuation + PushLocal/PushCachedBinding ... PullApply
		if code[i].Op == OpSaveContinuation &&
			i+1 < len(code) {
			calleeOp := calleeToCallOp[code[i+1].Op]
			if calleeOp == OpInvalid {
				continue
			}

			off := int(code[i].Arg)
			pullIdx := i + off - 1
			calleeArg := code[i+1].Arg

			if pullIdx < 0 || pullIdx >= len(code) || code[pullIdx].Op != OpPullApply {
				continue
			}

			// No branch target in (i+1, pullIdx).
			branchInInterior := false
			for k := i + 1; k < pullIdx; k++ {
				if targets[k] {
					branchInInterior = true
					break
				}
			}
			if branchInInterior {
				continue
			}

			// All instructions between the callee push and PullApply must be push-family.
			allPush := true
			for k := i + 2; k < pullIdx; k++ {
				if !isPushOp(code[k].Op) {
					allPush = false
					break
				}
			}
			if !allPush {
				continue
			}

			// Delete only the callee push [i+1, i+2). Keep SaveContinuation.
			plan.Delete(i+1, i+2)
			plan.Replace(pullIdx, pullIdx+1,
				[]Instruction{{Op: calleeOp, Arg: calleeArg}},
				tpl.sourceTableRefs[pullIdx],
			)
			continue
		}

		// Tail pattern: PushLocal/PushCachedBinding opening an empty group ... PullApply
		calleeOp := calleeToCallOp[code[i].Op]
		if calleeOp == OpInvalid {
			continue
		}
		// Preceded by SaveContinuation: the non-tail pattern above owns it.
		// Deduplication, not soundness — see the same guard in
		// fuseCallForeignCached.
		if i > 0 && code[i-1].Op == OpSaveContinuation {
			continue
		}
		// The callee is what OpPullApply pulls from the BOTTOM of the eval
		// stack, so this push is it only when the stack is empty here. This
		// subsumes the old "predecessor is not a push" test and closes the gap
		// that test left; see eval_depth.go.
		if !evalStackEmptyAt(depths, i) {
			continue
		}

		calleeArg := code[i].Arg

		// Scan forward for PullApply. A frame-reclaim release may sit between
		// the last argument push and the apply; stepping over it is sound only
		// for a callee that does not resolve through mc.env, so a PushLocal
		// callee stops the scan there and keeps its unfused shape.
		pullIdx, _ := scanTailCallArgs(code, i+1, releaseSafeCallee(code[i].Op))
		if pullIdx < 0 {
			continue
		}

		// No branch target in [i, pullIdx].
		branchInInterior := false
		for k := i; k <= pullIdx; k++ {
			if targets[k] {
				branchInInterior = true
				break
			}
		}
		if branchInInterior {
			continue
		}

		// Delete callee push [i, i+1).
		plan.Delete(i, i+1)
		plan.Replace(pullIdx, pullIdx+1,
			[]Instruction{{Op: calleeOp, Arg: calleeArg}},
			tpl.sourceTableRefs[pullIdx],
		)
	}
}

// fusePromotedCompoundArgs promotes a TAIL call to a promoted primitive whose
// arguments are compound subexpressions — the case the all-push fusions in
// fuseCallForeignCached reject. The dominant example is fib's tail addition:
//
//	(+ (fib (- n 1)) (fib (- n 2)))
//
// whose operands are nested calls, so SaveContinuation/PullApply (non-push)
// ops sit between the `+` callee push and its PullApply. fuseCallForeignCached
// only counts push-family args and bails the moment it sees a non-push op, so
// `+` stays on the generic apply path while `-` and `<=` (leaf operands) are
// promoted to inline Sub/NumLe.
//
// Pattern (tail position — callee push not preceded by SaveContinuation/push):
//
//	code[i]    = PushCachedBinding(idx)   -- promoted *ForeignClosure
//	<arg_1> .. <arg_k>                    -- each arg: one push, OR a
//	                                         SaveContinuation(off)..PullApply..Push block
//	code[t]    = ReleaseEnvFrame          -- REQUIRED (the frame-reclaim proof)
//	code[t+1]  = PullApply                -- k == promoted arity
//
// Rewrite: delete the callee push, replace the trailing PullApply with the
// tail promoted op. The arguments and the ReleaseEnvFrame are left intact, so
// interior continuation resume points stay valid; only the callee push is
// removed (the inline op uses fixed Pop(arity) and never reads a callee off
// the stack) and the final dispatch is replaced. Offsets are recomputed by
// EditPlan.Apply.
//
// The REQUIRED OpReleaseEnvFrame is the safety gate. Codegen emits it only
// after proving this env frame is dead at the tail call — no capture, no
// escaping closure, only capture-safe callees. That same proof is what makes
// promotion sound: if no argument can capture a continuation that re-enters
// this frame, the inline tail op (which pops the eval stack and returns,
// abandoning the frame) cannot corrupt a live continuation. fib's `+` carries
// the proof; a tail `cons` inside `map` does NOT (its callback `f` is an
// unknown callee that may call/cc), so it is correctly left on the generic path
// — without this gate, re-entering a continuation captured in `f` fails to find
// the loop's list local.
//
// Runs after fuseCallForeignCached, so all-push promoted calls are already
// fused and never reach here. Soundness against `set!`-redefinition of the
// primitive is preserved by execPromoted's runtime fallback (callPromotedFallback,
// which PopN(arity)s — matching the k values the compound args leave on the stack).
func fusePromotedCompoundArgs(tpl *NativeTemplate, plan *EditPlan) {
	code := tpl.code
	if len(code) < 2 {
		return
	}
	targets := branchTargets(code)

	for i := 0; i < len(code); i++ {
		if code[i].Op != OpPushCachedBinding {
			continue
		}

		// Tail position only: a callee push preceded by SaveContinuation or
		// another push is a non-tail call or an argument, not a tail callee.
		// The non-tail case would also need its outer SaveContinuation removed
		// (the inline op has no continuation to restore); that is left for a
		// follow-up. Tail covers the fib hot path.
		if i > 0 && (code[i-1].Op == OpSaveContinuation || isPushOp(code[i-1].Op)) {
			continue
		}

		bindingIdx := code[i].Arg
		if int(bindingIdx) >= len(tpl.cachedBindings) {
			continue
		}
		fcls, ok := tpl.cachedBindings[bindingIdx].Value().(*ForeignClosure)
		if !ok {
			continue
		}
		_, promotedTailOp, promotedArity := promotedOpForIdentity(fcls.identity)
		if promotedTailOp == OpInvalid {
			continue
		}

		termIdx, argCount, walked := walkCallArgs(code, i+1)
		if !walked || argCount != promotedArity {
			continue
		}

		// Safety gate: only promote when the tail apply is immediately preceded
		// by OpReleaseEnvFrame — the codegen's proof that this env frame is dead
		// (no capture, no escaping closure, only capture-safe callees). Without
		// the proof an argument may capture a continuation that re-enters needing
		// this frame's locals, which the inline tail op (it pops the eval stack
		// and returns, abandoning the frame) cannot preserve. This is exactly
		// what distinguishes fib's `+` (proof present) from a tail `cons` inside
		// `map`, whose unknown callback may call/cc (no proof) — the latter must
		// stay on the generic apply path.
		if code[termIdx].Op != OpReleaseEnvFrame {
			continue
		}
		pullIdx := termIdx + 1
		if pullIdx >= len(code) || code[pullIdx].Op != OpPullApply {
			continue
		}

		// The inline op pops exactly `arity` values, so the apply cannot be a
		// control-flow join (a branch into it would arrive with a wrong stack).
		if targets[pullIdx] {
			continue
		}

		plan.Delete(i, i+1)
		plan.Replace(pullIdx, pullIdx+1,
			[]Instruction{{Op: promotedTailOp, Arg: bindingIdx}},
			tpl.sourceTableRefs[pullIdx],
		)
		i = pullIdx // skip past the claimed call
	}
}

// walkCallArgs walks the argument sequence of a call beginning at position
// start (the instruction just after the callee push). Each argument is either
// a single push-family instruction or a SaveContinuation(off)..PullApply..Push
// block (a compound subexpression, skipped by following the continuation offset
// to its result push). It returns termIdx — the index of the first instruction
// that is not an argument, i.e. the start of the call's terminator region
// (ReleaseEnvFrame and/or PullApply) — the argument count, and whether the
// argument structure was recognized.
//
// The first opcode that is neither a push nor a SaveContinuation block ends the
// argument region: it is returned as termIdx with ok=true. walkCallArgs does NOT
// verify that termIdx points at a legitimate terminator — a bare branch lands
// here just as ReleaseEnvFrame/PullApply would, and confirming the terminator
// shape is the caller's job. ok is false only when a SaveContinuation offset is
// malformed (non-forward, out of range, or not landing on a result push) or the
// code ends before any terminator opcode is reached.
func walkCallArgs(code []Instruction, start int) (termIdx, argCount int, ok bool) {
	j := start
	for j < len(code) {
		op := code[j].Op
		if isPushOp(op) {
			argCount++
			j++
			continue
		}
		if op == OpSaveContinuation {
			// Compound arg: the offset targets the result push, one past the
			// inner PullApply. Skip the whole subexpression by jumping to it.
			// The offset must point strictly forward: resume <= j catches a zero
			// or negative offset, the latter of which would otherwise index
			// code[resume] out of bounds below (the >= len(code) guard alone does
			// not cover a negative resume).
			resume := j + int(code[j].Arg)
			if resume <= j || resume >= len(code) || !isPushOp(code[resume].Op) {
				return 0, 0, false
			}
			argCount++
			j = resume + 1
			continue
		}
		// First non-argument opcode: the terminator region begins here.
		return j, argCount, true
	}
	return 0, 0, false
}
