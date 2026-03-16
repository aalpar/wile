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

// RunFlatClosurePipeline runs the three-pass flat closure transformation
// on a compiled template: free variable analysis, box insertion, and
// closure flattening. Each pass recurses internally into sub-templates
// (bottom-up for Passes 1-2, top-down for Pass 3).
//
// The pipeline is idempotent: sub-templates already processed by a
// nested compileClosureBody call are skipped via the flatClosuresDone
// flag on NativeTemplate.
//
// Call site: compileClosureBody, after compileBody returns and before
// peephole optimization.
func RunFlatClosurePipeline(tpl *NativeTemplate) {
	if tpl.flatClosuresDone {
		return
	}
	AnalyzeFreeVars(tpl)
	InsertBoxes(tpl)
	FlattenClosures(tpl, nil)
	tpl.flatClosuresDone = true
}
