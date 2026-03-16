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
	"sort"
)

// AnalyzeFreeVars scans a compiled template's bytecodes for free variable
// references (OpLoadLocal/OpStoreLocal/OpPushLocal with depth > 0) and
// produces a FreeVarInfo describing which variables the template captures
// from enclosing scopes.
//
// This is Pass 1 of the flat closure pipeline. It is a read-only scan —
// no bytecodes are modified. The result is stored on the template via
// SetFreeVarInfo.
//
// Execution order: bottom-up. Inner templates (found in tpl.literals)
// are analyzed before the outer template.
func AnalyzeFreeVars(tpl *NativeTemplate) *FreeVarInfo {
	// Step 1: Recurse into sub-templates (bottom-up).
	// Skip sub-templates already processed by a nested compileClosureBody call.
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*NativeTemplate)
		if !ok {
			continue
		}
		if sub.flatClosuresDone {
			continue
		}
		info := AnalyzeFreeVars(sub)
		sub.SetFreeVarInfo(info)
	}

	// freeVarSet maps (slot, depth) to presence for deduplication.
	freeVarSet := make(map[[2]int]bool)
	// mutatedSet tracks which (slot, depth) pairs are targeted by set!.
	mutatedSet := make(map[[2]int]bool)

	// Step 2: Scan own bytecodes.
	for _, instr := range tpl.Code() {
		switch instr.Op {
		case OpLoadLocal, OpPushLocal:
			slot, depth := DecodeLocalIndex(instr.Arg)
			if depth > 0 {
				freeVarSet[[2]int{slot, depth}] = true
			}
		case OpStoreLocal:
			slot, depth := DecodeLocalIndex(instr.Arg)
			if depth > 0 {
				key := [2]int{slot, depth}
				freeVarSet[key] = true
				mutatedSet[key] = true
			}
		}
	}

	// Step 3: Propagate from inner templates.
	for _, lit := range tpl.Literals() {
		sub, ok := lit.(*NativeTemplate)
		if !ok {
			continue
		}
		subInfo := sub.FreeVarInfo()
		if subInfo == nil {
			continue
		}
		for _, cap := range subInfo.Captures {
			if cap.SourceDepth > 1 {
				// Variable passes through the current template.
				// Adjust depth by -1: the inner template's depth=2 is our depth=1.
				adjusted := [2]int{cap.SourceSlot, cap.SourceDepth - 1}
				freeVarSet[adjusted] = true
			}
			// If SourceDepth == 1, the variable comes from our locals.
			// We don't add it to our own free vars — it's ours.
		}
		// Propagate mutation status.
		for mutKey := range subInfo.Mutated {
			// mutKey is (slot, depth) in the sub-template's coordinate space.
			// Only propagate if depth > 1 (passes through us).
			if mutKey[1] > 1 {
				adjusted := [2]int{mutKey[0], mutKey[1] - 1}
				if freeVarSet[adjusted] {
					mutatedSet[adjusted] = true
				}
			}
		}
	}

	// Step 4: Build capture list in deterministic order.
	keys := make([][2]int, 0, len(freeVarSet))
	for k := range freeVarSet {
		keys = append(keys, k)
	}
	sort.Slice(keys, func(i, j int) bool {
		if keys[i][1] != keys[j][1] {
			return keys[i][1] < keys[j][1]
		}
		return keys[i][0] < keys[j][0]
	})

	captures := make([]CaptureEntry, len(keys))
	for i, k := range keys {
		captures[i] = CaptureEntry{
			SourceSlot:  k[0],
			SourceDepth: k[1],
			ClosureSlot: i,
		}
	}

	// Step 5: Return FreeVarInfo (always non-nil, empty Captures if no free vars).
	q := &FreeVarInfo{
		Captures: captures,
		Mutated:  mutatedSet,
	}
	return q
}
