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

package match

import "sync/atomic"

// patternVarSubstitutions counts template occurrences that expandSymbol replaced
// with a capture — the runtime half of the pattern-variable resolution ratchet.
//
// It exists because both failure directions of the substitution gate are
// silent under value assertions when the escaping name collides with a global.
// An identifier that stops substituting falls through to free-identifier
// hygiene, and `(list x)` in a template whose `x` deopted still evaluates, to
// the global `x`. That is the shape of the 2026-08-20 capture defect read
// backwards, and no assertion over return values distinguishes it from the
// correct expansion. A count does.
//
// Process-global and ungated: macro expansion is not a hot path (the Gabriel
// suite contains no define-syntax at all), and a gate would add a branch plus
// the risk of a ratchet that silently measures nothing when the gate is off.
// Atomic because SRFI-18 threads can expand concurrently.
var patternVarSubstitutions atomic.Uint64

// PatternVarSubstitutions reports how many template occurrences have been
// replaced by a capture in this process. Callers ratchet on a DIFFERENCE across
// a known unit of work, never on the absolute value, which depends on whatever
// else ran first in the same binary.
func PatternVarSubstitutions() uint64 {
	return patternVarSubstitutions.Load()
}
