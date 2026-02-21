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

// isLoadOp returns true for opcodes that write to the value register,
// making a preceding LoadVoid dead.
func isLoadOp(op OpCode) bool {
	return op == OpLoadVoid ||
		op == OpLoadLiteral ||
		op == OpLoadGlobal ||
		op == OpLoadLocal
}

// markDeadLoadVoidEdits scans code[0..len-2] and adds Delete edits for
// LoadVoid instructions immediately followed by a load-family opcode.
func markDeadLoadVoidEdits(code []Instruction, plan *EditPlan) {
	for i := 0; i < len(code)-1; i++ {
		if code[i].Op == OpLoadVoid && isLoadOp(code[i+1].Op) {
			plan.Delete(i, i+1)
		}
	}
}
