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

// CaptureEntry describes one free variable in a closure's capture list.
// Each entry maps a binding in some enclosing scope to a slot in the
// closure's flat freeVars array.
type CaptureEntry struct {
	SourceSlot   int  // binding slot in the source scope
	SourceDepth  int  // de Bruijn depth from closure (1 = immediate parent)
	ClosureSlot  int  // index in the flat freeVars array
	Boxed        bool // needs *values.Box wrapping
	FromFreeVars bool // true: source is enclosing closure's freeVars[SourceSlot]
}

// FreeVarInfo is the analysis result for one NativeTemplate.
// Produced by Pass 1 (FreeVarAnalysis), consumed by Pass 2 (BoxInsertion)
// and Pass 3 (ClosureFlatten).
type FreeVarInfo struct {
	Captures []CaptureEntry
	Mutated  map[[2]int]bool // (slot, depth) pairs targeted by set!
}
