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

// goSyntaxFormCompiles counts entries into the Go compilers of the forms the
// Scheme layer replaces (syntax, syntax-case, with-syntax, quasisyntax; from P2
// syntax-rules and er-macro-transformer). Under WithSchemeSyntaxForms it must
// not move: TestP1_SwitchSelectsTheLayer is the unreachability pin design §7
// P1 asks for, and it is a COUNTER rather than a value because the interim
// failure mode is silent — an unexcluded Scheme define-syntax lands in the
// Primitive-typed slot, the Go compiler runs anyway, and every value assertion
// stays green.
//
// Deleted in P3 with the compilers.
var goSyntaxFormCompiles atomic.Uint64

// GoSyntaxFormCompiles reports the process-global count; ratchet on a
// difference across a known unit of work.
func GoSyntaxFormCompiles() uint64 {
	return goSyntaxFormCompiles.Load()
}
