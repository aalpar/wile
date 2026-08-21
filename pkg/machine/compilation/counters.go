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

package compilation

import "sync/atomic"

// templateLocalEmits counts template occurrences that compileSyntaxTemplateToOps
// resolved to a local binding and emitted a load for — the compile-time half of
// the pattern-variable resolution ratchet, whose runtime half is
// match.PatternVarSubstitutions.
//
// Both halves are needed and neither substitutes for the other: a template
// without ellipsis is emitted here, one with ellipsis is expanded at run time,
// and the two paths have failed independently. The 2026-08-20 capture defect was
// live on this path and fixed on the other for as long as it took to notice that
// `AllScopes` made this one blind to scopes entirely.
//
// Counted before the pattern-variable gate narrows it, so an ENCLOSING clause's
// pattern variable — which the gate deliberately does not intercept — is
// included. A ratchet wants the broader event: the number that must not fall.
//
// See match.PatternVarSubstitutions for why this is process-global and ungated.
var templateLocalEmits atomic.Uint64

// TemplateLocalEmits reports how many syntax-template occurrences have been
// emitted as local loads in this process. Ratchet on a DIFFERENCE across a known
// unit of work, never on the absolute value.
func TemplateLocalEmits() uint64 {
	return templateLocalEmits.Load()
}
