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

package helpers

import "github.com/aalpar/wile/pkg/values"

// Eqv implements R7RS §6.1 eqv?, and backs memv, assv, and case.
//
// eqv? is eq? widened by exactly two families: numbers and characters. Everything
// else it settles by identity, which is what eq? already does (symbols by name,
// per §6.5, since Wile de-interns them). So this function is three clauses, and
// each defers to the authority for its family rather than restating it:
//
//	identity  -> values.EqIdentity
//	numbers   -> values.EqvNumber   (also what equal? uses, via each numeric EqualTo)
//	chars     -> char= on the code point
//
// It used to restate the numeric rules as a fourteen-arm type switch. That copy
// disagreed with the one in the numeric EqualTo methods — it merged 0.0 with -0.0
// (Go's == on float64), and it rejected cross-representation inexacts that
// EqualTo accepted — so eqv? and equal? gave different answers on the same two
// numbers, which §6.1 forbids outright. Routing both through EqvNumber makes them
// agree by construction rather than by two implementations happening to concur.
//
// The composition itself moved to values.Eqv so Hashtable can dispatch an
// eqv-kind table through it; this is a thin re-export retained for API stability,
// matching EqIdentity below.
func Eqv(a, b values.Value) bool {
	return values.Eqv(a, b)
}

// EqIdentity implements eq? semantics: pointer identity for all types except
// symbols, which compare by name (R7RS §6.1, §6.5). Thin re-export of
// values.EqIdentity — the single source of truth — retained for API stability
// (registry/helpers is a public package; memq/assq pass it as a comparator value).
func EqIdentity(a, b values.Value) bool {
	return values.EqIdentity(a, b)
}
