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
	"sync"

	"github.com/aalpar/wile/pkg/values"
)

// ImmutableLiterals is the engine-scoped set of literal pair and vector
// objects that R7RS §4.1.2 makes immutable. Membership is determined once, at
// compile time, when a quoted literal is interned into a template's literal
// pool, and read on the cold mutation path (set-car!/set-cdr!/list-set!/
// vector-set!/vector-fill!).
//
// The set is grow-only and write-once-per-key. sync.Map is chosen for its
// lock-free read path (an atomic load of an internal read-only map), which
// matches the "written once at compile time, read many at run time" profile —
// reads land on set-car!/set-cdr!, a path the codebase keeps explicitly hot
// (registry/core/prim_pairs.go). No unsafe: keys are ordinary *Pair/*Vector
// pointers boxed as values.Value; pointer identity survives interface boxing,
// and Go's heap GC is non-moving.
//
// The set is NOT a struct field on Pair/Vector by design: type Pair is
// [2]Value and type Vector is []Value (not structs), and adding a word would
// grow the 32-byte cons cell ~25% — the dominant heap object, directly
// opposing the GC-reduction goal. The side-set keeps pairs at 32 bytes; the
// cost lands only on the cold mutation path.
type ImmutableLiterals struct {
	m sync.Map // key: values.Value (*values.Pair / *values.Vector), value: struct{}
}

// Mark records v as immutable. Called at compile time from the quote hook.
func (p *ImmutableLiterals) Mark(v values.Value) {
	p.m.Store(v, struct{}{})
}

// Contains reports whether v was marked immutable. Membership is by pointer
// identity, not equal? — a distinct but structurally-equal value is not a
// member unless it too was marked.
func (p *ImmutableLiterals) Contains(v values.Value) bool {
	_, ok := p.m.Load(v)
	return ok
}

// IsImmutable reports whether in-place mutation of v is forbidden, spanning
// both immutability mechanisms with a single query:
//
//   - Values that carry an intrinsic flag (values.Immutable, currently *String)
//     answer from that flag. This works even when p is nil, since the flag does
//     not depend on the side-set.
//   - Pair and Vector answer from this engine-scoped side-set by pointer
//     identity (the literals marked at compile time per R7RS §4.1.2).
//   - Every other value — including non-aggregate scalars and unmarked,
//     runtime-constructed pairs/vectors — is not constrained, so the result is
//     false.
//
// Mutation gate sites that already hold a concrete *Pair/*Vector use this in
// place of the inline `set != nil && set.Contains(v)` guard. *String primitives
// keep self-enforcing inside their mutators (SetChar/Fill), which is the correct
// layer for a value that owns its own bit; this predicate is the canonical
// answer for any caller that must ask without knowing the type.
func (p *ImmutableLiterals) IsImmutable(v values.Value) bool {
	q, ok := v.(values.Immutable)
	if ok {
		return q.IsImmutable()
	}
	return p != nil && p.Contains(v)
}
