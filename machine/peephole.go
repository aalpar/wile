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
		return
	}

	dead := make([]bool, n)
	count := markDeadLoadVoid(p.code, dead)
	if count == 0 {
		p.optimizeSubTemplates()
		return
	}

	pcRemap := buildPCRemap(dead)
	fixBranchOffsets(p.code, dead, pcRemap)
	p.code, p.sourceRefs = compact(p.code, p.sourceRefs, dead)

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

// isLoadOp returns true for opcodes that write to the value register,
// making a preceding LoadVoid dead.
func isLoadOp(op OpCode) bool {
	return op == OpLoadVoid ||
		op == OpLoadLiteral ||
		op == OpLoadGlobal ||
		op == OpLoadLocal
}

// isBranchOp returns true for opcodes whose Arg is a relative PC offset
// that must be adjusted when instructions are removed.
func isBranchOp(op OpCode) bool {
	return op == OpBranch ||
		op == OpBranchOnFalseValue ||
		op == OpSaveContinuation
}

// markDeadLoadVoid scans code[0..len-2] and marks LoadVoid instructions
// that are immediately followed by a load-family opcode. Returns the
// number of dead instructions found.
func markDeadLoadVoid(code []Instruction, dead []bool) int {
	count := 0
	for i := 0; i < len(code)-1; i++ {
		if code[i].Op == OpLoadVoid && isLoadOp(code[i+1].Op) {
			dead[i] = true
			count++
		}
	}
	return count
}

// buildPCRemap constructs a mapping from old PC positions to new PC
// positions after dead instructions are removed. The returned slice
// has length len(dead)+1 so that pcRemap[len(dead)] gives the total
// count of surviving instructions (used for targets at code end).
//
// Dead positions map to the same new index as the next surviving
// instruction, so a branch targeting a dead position correctly lands
// on the first survivor after it.
func buildPCRemap(dead []bool) []int {
	remap := make([]int, len(dead)+1)
	removed := 0
	for i, d := range dead {
		remap[i] = i - removed
		if d {
			removed++
		}
	}
	remap[len(dead)] = len(dead) - removed
	return remap
}

// fixBranchOffsets adjusts the relative offset (Arg) of surviving branch
// instructions so they target the same logical instruction after dead
// entries are removed.
func fixBranchOffsets(code []Instruction, dead []bool, pcRemap []int) {
	for i := range code {
		if dead[i] {
			continue
		}
		if !isBranchOp(code[i].Op) {
			continue
		}
		absTarget := i + int(code[i].Arg)
		code[i].Arg = int32(pcRemap[absTarget] - pcRemap[i])
	}
}

// compact removes dead entries from code and sourceRefs using an in-place
// write cursor. Both slices are compacted in lockstep to maintain the
// 1:1 correspondence between instructions and source references.
func compact(code []Instruction, sourceRefs []uint16, dead []bool) ([]Instruction, []uint16) {
	w := 0
	for i := range code {
		if dead[i] {
			continue
		}
		code[w] = code[i]
		sourceRefs[w] = sourceRefs[i]
		w++
	}
	return code[:w], sourceRefs[:w]
}
