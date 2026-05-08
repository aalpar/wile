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
// Representable phases are 0 ≤ phase < phaseSetBits. environment.PhaseTemplate
// (-1) and any phase ≥ phaseSetBits are unrepresentable. With(unrepresentable)
// panics; Has(unrepresentable) returns false. Both ends of the illegal-value
// space are checked — silent shift overflow on a uint8 bitset would otherwise
// hide the upper bound.
//
// PhaseSet is intended as a registration-time API: Add* helpers and the
// extension authoring path build PhaseSet values from compile-time constants.
// If a runtime caller appears (constructing a PhaseSet from a Scheme integer,
// for example), introduce an error-returning sibling such as TryWith rather
// than relying on the panic semantics here.
//
// ADDING A NEW PHASE requires updates in these locations:
//
//  1. environment/phase_registry.go — add a Phase constant; ensure 0 ≤ value
//     < phaseSetBits below if the new phase should be representable in a
//     PhaseSet.
//  2. registry/phase.go (this file) — add PhaseSet<Name> bit constant and
//     extend phaseSetBits if the bitset width must grow.
//  3. registry/apply.go — extend phaseTargets with the new phase if
//     primitives may register at it.
//  4. wile/options.go — add re-export so embedders can name the constant.
//
// The init() assertion below verifies bit positions match Phase indices,
// catching drift from steps (1)+(2).
type PhaseSet uint8

// phaseSetBits is the number of bit positions PhaseSet supports — the
// upper bound on representable phase indices. Currently bounded by the
// uint8 storage of PhaseSet itself.
const phaseSetBits = 8

// PhaseSet bit constants. Each bit position equals 1 << int(environment.Phase).
// init() asserts the values stay in sync with environment.Phase.
const (
	PhaseSetRuntime PhaseSet = 1 << iota // matches environment.PhaseRuntime (=0)
	PhaseSetExpand                       // matches environment.PhaseExpand  (=1)
	PhaseSetCompile                      // matches environment.PhaseCompile (=2)
)

func init() {
	// Per-bit correspondence: each PhaseSet constant must be exactly
	// 1 << int(its corresponding environment.Phase). Catches both reorders
	// (swapping two Phase values) and silent truncation (Phase out of
	// representable range).
	checks := []struct {
		bit  PhaseSet
		p    environment.Phase
		name string
	}{
		{PhaseSetRuntime, environment.PhaseRuntime, "Runtime"},
		{PhaseSetExpand, environment.PhaseExpand, "Expand"},
		{PhaseSetCompile, environment.PhaseCompile, "Compile"},
	}
	for _, c := range checks {
		if c.p < 0 || int(c.p) >= phaseSetBits {
			panic(fmt.Sprintf(
				"registry: environment.Phase%s = %d is not representable in PhaseSet (bits 0..%d)",
				c.name, c.p, phaseSetBits-1))
		}
		if c.bit != PhaseSet(1<<uint(c.p)) {
			panic(fmt.Sprintf(
				"registry: PhaseSet%s = %d does not match 1<<environment.Phase%s (=%d)",
				c.name, c.bit, c.name, 1<<uint(c.p)))
		}
	}

	// PhaseTemplate must remain unrepresentable in a PhaseSet — its
	// negative value is the load-bearing invariant for With/Has guards.
	if environment.PhaseTemplate >= 0 {
		panic(fmt.Sprintf(
			"registry: environment.PhaseTemplate must be negative to remain unrepresentable in PhaseSet (got %d)",
			environment.PhaseTemplate))
	}
}

// Has reports whether p is in the set. Returns false for any phase that
// cannot appear in a PhaseSet — negative phases (e.g., PhaseTemplate) and
// phases ≥ phaseSetBits.
func (s PhaseSet) Has(p environment.Phase) bool {
	if p < 0 || int(p) >= phaseSetBits {
		return false
	}
	return s&(1<<uint(p)) != 0
}

// With returns a new PhaseSet with p added. Panics if p is unrepresentable
// (negative, e.g. PhaseTemplate, or ≥ phaseSetBits — would silently shift
// past the bitset width). PhaseSet is a registration-time API; programmer
// errors at this layer should fail loudly. If a runtime caller is added,
// introduce a non-panicking sibling rather than weakening this contract.
func (s PhaseSet) With(p environment.Phase) PhaseSet {
	if p < 0 || int(p) >= phaseSetBits {
		panic(werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"PhaseSet.With: phase %d not representable in PhaseSet (valid range 0..%d)",
			p, phaseSetBits-1))
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
