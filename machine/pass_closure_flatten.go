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
	"github.com/aalpar/wile/werr"
)

// FlattenClosures rewrites free-variable references in closure bodies
// from OpLoadLocal(slot, depth>0) to OpLoadFreeVar(closureSlot), and
// rewrites OpMakeClosure to OpMakeFlatClosure for templates with captures.
//
// This is Pass 3 of the flat closure pipeline. It modifies bytecodes
// in the CAPTURING template (the closure body).
//
// enclosingInfo is the FreeVarInfo of the enclosing template, used to
// resolve FromFreeVars for captures with SourceDepth > 1. Pass nil for
// top-level templates.
//
// Execution order: top-down (outer templates before inner), so that
// enclosingInfo is available when processing children.
func FlattenClosures(tpl *NativeTemplate, enclosingInfo *FreeVarInfo) {
	info := tpl.FreeVarInfo()

	// Step 1: Resolve FromFreeVars on captures.
	if info != nil && len(info.Captures) > 0 {
		resolveFromFreeVars(info, enclosingInfo)
	}

	// Step 2: Build lookup map and rewrite bytecodes.
	if info != nil && len(info.Captures) > 0 {
		lookup := buildFreeVarLookup(info)
		rewriteFreeVarReferences(tpl, lookup)
	}

	// Step 3: Rewrite OpMakeClosure → OpMakeFlatClosure for sub-templates
	// that have captures.
	rewriteMakeClosureToFlat(tpl)

	// Step 4: Recurse into sub-templates (top-down).
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*NativeTemplate)
		if !ok {
			continue
		}
		FlattenClosures(sub, info)
	}
}

// resolveFromFreeVars resolves the FromFreeVars flag and adjusts SourceSlot
// for captures that pass through the enclosing scope's freeVars array.
//
// For captures at SourceDepth == 1: the value comes from the enclosing
// scope's local bindings, so FromFreeVars = false.
//
// For captures at SourceDepth > 1: the value is itself a free variable of
// the enclosing scope. FromFreeVars is set to true, and SourceSlot is
// rewritten to the enclosing scope's ClosureSlot for that variable.
func resolveFromFreeVars(info *FreeVarInfo, enclosingInfo *FreeVarInfo) {
	for i := range info.Captures {
		entry := &info.Captures[i]
		if entry.SourceDepth == 1 {
			entry.FromFreeVars = false
			continue
		}
		// SourceDepth > 1: the variable passes through the enclosing scope.
		if enclosingInfo == nil {
			continue
		}
		entry.FromFreeVars = true
		// Find the enclosing template's capture entry for this variable.
		// From the current template's perspective, the variable is at
		// (SourceSlot, SourceDepth). From the enclosing template's
		// perspective, it is at (SourceSlot, SourceDepth-1).
		encSlot := findEnclosingClosureSlot(enclosingInfo, entry.SourceSlot, entry.SourceDepth-1)
		if encSlot < 0 {
			panic(werr.WrapForeignErrorf(
				werr.ErrNoSuchBinding,
				"FlattenClosures: no enclosing capture for (slot=%d, depth=%d)",
				entry.SourceSlot, entry.SourceDepth-1,
			))
		}
		entry.SourceSlot = encSlot
	}
}

// findEnclosingClosureSlot searches the enclosing template's FreeVarInfo
// for a capture matching (slot, depth) and returns its ClosureSlot.
// Returns -1 if not found.
func findEnclosingClosureSlot(enclosingInfo *FreeVarInfo, slot, depth int) int {
	for _, c := range enclosingInfo.Captures {
		if c.SourceSlot == slot && c.SourceDepth == depth {
			return c.ClosureSlot
		}
	}
	return -1
}

// buildFreeVarLookup builds a map from (slot, depth) to closureSlot for
// the template's FreeVarInfo captures.
func buildFreeVarLookup(info *FreeVarInfo) map[[2]int]int {
	q := make(map[[2]int]int, len(info.Captures))
	for _, c := range info.Captures {
		q[[2]int{c.SourceSlot, c.SourceDepth}] = c.ClosureSlot
	}
	return q
}

// rewriteFreeVarReferences rewrites OpLoadLocal(slot, depth>0) and
// OpPushLocal(slot, depth>0) to OpLoadFreeVar(closureSlot) using an EditPlan.
func rewriteFreeVarReferences(tpl *NativeTemplate, lookup map[[2]int]int) {
	plan := NewEditPlan(tpl)

	for pc, instr := range tpl.Code() {
		switch instr.Op {
		case OpLoadLocal:
			slot, depth := DecodeLocalIndex(instr.Arg)
			if depth == 0 {
				continue
			}
			closureSlot, ok := lookup[[2]int{slot, depth}]
			if !ok {
				continue
			}
			plan.Replace(pc, pc+1, []Instruction{
				{Op: OpLoadFreeVar, Arg: int32(closureSlot)},
			}, tpl.sourceRefs[pc])

		case OpPushLocal:
			slot, depth := DecodeLocalIndex(instr.Arg)
			if depth == 0 {
				continue
			}
			closureSlot, ok := lookup[[2]int{slot, depth}]
			if !ok {
				continue
			}
			plan.Replace(pc, pc+1, []Instruction{
				{Op: OpLoadFreeVar, Arg: int32(closureSlot)},
				{Op: OpPush},
			}, tpl.sourceRefs[pc])
		}
	}

	plan.Apply()
}

// rewriteMakeClosureToFlat scans for OpMakeClosure instructions and rewrites
// the closure creation sequence to use OpMakeFlatClosure when the child
// template has captures (non-empty FreeVarInfo).
//
// The compiler emits the following 5-instruction sequence:
//
//	OpLoadLiteral(tplIdx)   ; load template
//	OpPush                  ; push template
//	OpLoadLiteral(envIdx)   ; load env
//	OpPush                  ; push env
//	OpMakeClosure           ; pop env, pop template, create closure
//
// After peephole optimization, Load+Push pairs may be fused:
//
//	OpPushLiteral(tplIdx)   ; fused load+push template
//	OpPushLiteral(envIdx)   ; fused load+push env
//	OpMakeClosure           ; pop env, pop template, create closure
//
// OpMakeFlatClosure pops only the template (not the env). The rewrite
// removes the env-pushing instructions and replaces MakeClosure:
//
//	unfused: keep [tplLoad, tplPush], replace [envLoad, envPush, MakeClosure] → [MakeFlatClosure]
//	fused:   keep [PushLiteral(tplIdx)], replace [PushLiteral(envIdx), MakeClosure] → [MakeFlatClosure]
func rewriteMakeClosureToFlat(tpl *NativeTemplate) {
	plan := NewEditPlan(tpl)
	code := tpl.Code()

	for pc := range code {
		if code[pc].Op != OpMakeClosure {
			continue
		}

		// Try to match the closure creation pattern by scanning backward.
		// We need to find: (1) the env push, and (2) the template literal index.
		match := matchMakeClosurePattern(code, pc)
		if !match.valid {
			continue
		}

		// Look up the child template from the literal pool.
		lits := tpl.Literals()
		if int(match.tplLitIdx) >= len(lits) {
			continue
		}
		childTpl, ok := lits[match.tplLitIdx].(*NativeTemplate)
		if !ok {
			continue
		}

		// Only rewrite if the child template has captures.
		childInfo := childTpl.FreeVarInfo()
		if childInfo == nil || len(childInfo.Captures) == 0 {
			continue
		}

		// Replace the env-push instructions and MakeClosure with MakeFlatClosure.
		// Everything from match.envStart to pc (inclusive) is replaced.
		plan.Replace(match.envStart, pc+1, []Instruction{
			{Op: OpMakeFlatClosure},
		}, tpl.sourceRefs[pc])
	}

	plan.Apply()
}

// makeClosureMatch holds the result of backward pattern matching for
// a MakeClosure instruction sequence.
type makeClosureMatch struct {
	valid     bool  // true if pattern was recognized
	tplLitIdx int32 // literal index of the child template
	envStart  int   // first instruction of the env-push sequence to replace
}

// matchMakeClosurePattern scans backward from an OpMakeClosure at code[pc]
// to identify the closure creation pattern. Handles both unfused and
// peephole-fused variants.
func matchMakeClosurePattern(code []Instruction, pc int) makeClosureMatch {
	// Pattern A (unfused, 5 instructions):
	//   pc-4: LoadLiteral(tplIdx)
	//   pc-3: Push
	//   pc-2: LoadLiteral(envIdx)
	//   pc-1: Push
	//   pc:   MakeClosure
	if pc >= 4 &&
		code[pc-4].Op == OpLoadLiteral &&
		code[pc-3].Op == OpPush &&
		code[pc-2].Op == OpLoadLiteral &&
		code[pc-1].Op == OpPush {
		return makeClosureMatch{
			valid:     true,
			tplLitIdx: code[pc-4].Arg,
			envStart:  pc - 2,
		}
	}

	// Pattern B (fully fused, 3 instructions):
	//   pc-2: PushLiteral(tplIdx)
	//   pc-1: PushLiteral(envIdx)
	//   pc:   MakeClosure
	if pc >= 2 &&
		code[pc-2].Op == OpPushLiteral &&
		code[pc-1].Op == OpPushLiteral {
		return makeClosureMatch{
			valid:     true,
			tplLitIdx: code[pc-2].Arg,
			envStart:  pc - 1,
		}
	}

	// Pattern C (tpl unfused, env fused, 4 instructions):
	//   pc-3: LoadLiteral(tplIdx)
	//   pc-2: Push
	//   pc-1: PushLiteral(envIdx)
	//   pc:   MakeClosure
	if pc >= 3 &&
		code[pc-3].Op == OpLoadLiteral &&
		code[pc-2].Op == OpPush &&
		code[pc-1].Op == OpPushLiteral {
		return makeClosureMatch{
			valid:     true,
			tplLitIdx: code[pc-3].Arg,
			envStart:  pc - 1,
		}
	}

	// Pattern D (tpl fused, env unfused, 4 instructions):
	//   pc-3: PushLiteral(tplIdx)
	//   pc-2: LoadLiteral(envIdx)
	//   pc-1: Push
	//   pc:   MakeClosure
	if pc >= 3 &&
		code[pc-3].Op == OpPushLiteral &&
		code[pc-2].Op == OpLoadLiteral &&
		code[pc-1].Op == OpPush {
		return makeClosureMatch{
			valid:     true,
			tplLitIdx: code[pc-3].Arg,
			envStart:  pc - 2,
		}
	}

	return makeClosureMatch{}
}
