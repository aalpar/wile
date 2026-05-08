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

package registry

import (
	"fmt"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/werr"
)

// PhaseSet is a bitset over non-negative environment.Phase values, used to
// declare which phases a primitive is registered for. The companion type
// environment.Phase is a typed enum identifying a single phase; PhaseSet
// is the registration vocabulary that says "this primitive is available
// at runtime", "at runtime and expand", etc.
//
// PhaseSet does not represent environment.PhaseTemplate (-1). Bit
// positions index into the set via 1 << uint(phase) for non-negative
// phases. With(PhaseTemplate) panics; Has(PhaseTemplate) returns false.
type PhaseSet uint8

// PhaseSet bit constants. Each bit position equals 1 << int(environment.Phase).
// init() asserts the values stay in sync with environment.Phase.
const (
	PhaseSetRuntime PhaseSet = 1 << iota // matches environment.PhaseRuntime (=0)
	PhaseSetExpand                       // matches environment.PhaseExpand  (=1)
	PhaseSetCompile                      // matches environment.PhaseCompile (=2)
)

func init() {
	expected := PhaseSet(1<<uint(environment.PhaseRuntime)) |
		PhaseSet(1<<uint(environment.PhaseExpand)) |
		PhaseSet(1<<uint(environment.PhaseCompile))
	have := PhaseSetRuntime | PhaseSetExpand | PhaseSetCompile
	if expected != have {
		panic(fmt.Sprintf(
			"registry: PhaseSet bit values out of sync with environment.Phase "+
				"(expected union %d, have %d) — did environment.Phase constants change?",
			expected, have))
	}
}

// Has reports whether p is in the set. Returns false for negative phases
// (PhaseTemplate cannot appear in a PhaseSet).
func (s PhaseSet) Has(p environment.Phase) bool {
	if p < 0 {
		return false
	}
	return s&(1<<uint(p)) != 0
}

// With returns a new PhaseSet with p added. Panics if p is negative —
// PhaseTemplate cannot be set; if it ever needs to be, this method
// (and the underlying uint8 bitset) needs to be redesigned.
func (s PhaseSet) With(p environment.Phase) PhaseSet {
	if p < 0 {
		panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"PhaseSet.With: phase %d not representable in PhaseSet", p))
	}
	return s | (1 << uint(p))
}

// String returns a pipe-separated list of phase names in the set,
// or "none" if the set is empty.
func (s PhaseSet) String() string {
	var parts []string
	for _, p := range []environment.Phase{environment.PhaseRuntime, environment.PhaseExpand, environment.PhaseCompile} {
		if s.Has(p) {
			parts = append(parts, p.String())
		}
	}
	if len(parts) == 0 {
		return "none"
	}
	return strings.Join(parts, "|")
}
