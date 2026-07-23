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

package environment

import (
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// BindingID is a stable identifier for a local binding, safe to use as a
// map key even when the backing []Binding slice is reallocated by append.
// The Frame pointer identifies the heap-allocated LocalEnvironmentFrame
// (stable across slice growth) and Slot is the index within that frame.
//
// This is the local-binding analog of GlobalIndex: GlobalIndex uses
// (*GlobalEnvironmentFrame, *Symbol), BindingID uses
// (*LocalEnvironmentFrame, int).
type BindingID struct {
	Frame *LocalEnvironmentFrame
	Slot  int
}

// ResolveBindingID looks up a local binding by symbol and scope-set query and
// returns a stable BindingID. Returns the zero BindingID and false if the
// symbol does not resolve to a local binding.
func (p *EnvironmentFrame) ResolveBindingID(key *values.Symbol, q syntax.ScopeSet) (BindingID, bool) {
	var bid BindingID
	result := p.resolveLocal(key, q, func(_ *Binding, slot int, depth int) any {
		frame := p
		for range depth {
			frame = frame.parent
		}
		bid = BindingID{Frame: &frame.local, Slot: slot}
		return true
	})
	return bid, result != nil
}

// ResolveBindingRef names the binding a symbol refers to: a local ref when the
// symbol resolves to a local binding, otherwise a symbolic global ref. It is
// total — always returns a valid BindingRef — because the global arm needs no
// existing binding (a top-level set!/define is named before the compiler
// creates its global). Unlike ResolveBindingID, callers do not branch on a
// found/not-found bool; "not local" is itself a nameable (global) outcome.
func (p *EnvironmentFrame) ResolveBindingRef(key *values.Symbol, q syntax.ScopeSet) BindingRef {
	bid, ok := p.ResolveBindingID(key, q)
	if ok {
		return LocalRef(bid)
	}
	return GlobalRef(key.Key)
}
