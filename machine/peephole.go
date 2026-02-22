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

// Optimize performs a peephole optimization pass on the template's bytecode.
// It removes dead instructions identified by pattern-matching rules, fixes
// branch offsets to account for removed instructions, and compacts the code
// and source reference arrays in parallel.
//
// After compacting its own code, Optimize recurses into any *NativeTemplate
// values in the literals pool (lambda closures compiled as sub-templates).
//
// Idempotent: a second call finds nothing to remove.
//
// Must be called BEFORE computeNoCopyApply, since removing instructions
// (e.g. OpSaveContinuation in future rules) could change escape analysis.
func (p *NativeTemplate) Optimize() {
	n := len(p.code)
	if n == 0 {
		p.optimizeSubTemplates()
		return
	}

	plan := NewEditPlan(p)
	markDeadLoadVoidEdits(p.code, plan)
	fuseLoadPush(p.code, p.sourceRefs, plan)
	plan.Apply()

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
// LoadVoid dead.
func writesValueRegister(op OpCode) bool {
	return op == OpLoadVoid ||
		op == OpLoadLiteral ||
		op == OpLoadGlobal ||
		op == OpLoadLocal ||
		op == OpPop ||
		op == OpPull ||
		op == OpPeekK
}

// markDeadLoadVoidEdits scans code[0..len-2] and adds Delete edits for
// LoadVoid instructions immediately followed by a load-family opcode.
func markDeadLoadVoidEdits(code []Instruction, plan *EditPlan) {
	for i := 0; i < len(code)-1; i++ {
		if code[i].Op == OpLoadVoid && writesValueRegister(code[i+1].Op) {
			plan.Delete(i, i+1)
		}
	}
}

// loadToFusedPush maps Load opcodes to their fused Push equivalents.
var loadToFusedPush = [opCount]OpCode{
	OpLoadLiteral: OpPushLiteral,
	OpLoadGlobal:  OpPushGlobal,
	OpLoadLocal:   OpPushLocal,
}

// fuseLoadPush scans for LoadLiteral+Push, LoadGlobal+Push, LoadLocal+Push
// pairs and adds Replace edits that fuse them into single PushLiteral,
// PushGlobal, or PushLocal instructions. The fused instruction inherits
// the source attribution from the Load instruction.
//
// A Load+Push pair is NOT fused if the Push is a branch target, because
// the Push may be a convergence point for multiple control flow paths
// (e.g., both branches of an `if` expression that share a Push to push
// the result). Fusing would bind the Push to only the Load's value.
func fuseLoadPush(code []Instruction, sourceRefs []uint16, plan *EditPlan) {
	targets := branchTargets(code)
	for i := 0; i < len(code)-1; i++ {
		fused := loadToFusedPush[code[i].Op]
		if fused != OpInvalid && code[i+1].Op == OpPush && !targets[i+1] {
			plan.Replace(i, i+2,
				[]Instruction{{Op: fused, Arg: code[i].Arg}},
				sourceRefs[i],
			)
			i++ // skip the Push
		}
	}
}

// branchTargets returns a set of instruction positions that are targeted
// by branch instructions. Used to prevent fusing Load+Push when the Push
// is a convergence point for multiple control flow paths.
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
