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

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"
)

// appendConstantLiteral marks v immutable and THEN appends it to the template's
// literal pool, returning the pool index. Every constant appender must route
// through here; nothing else may call MaybeAppendLiteral with a datum the
// program can name.
//
// The order is the fix, not a detail. MaybeAppendLiteral dedups by
// literalIdentical (reflect type + EqualTo), so the object that survives is
// whichever equal? twin was appended FIRST, and membership in
// ImmutableLiterals is pointer identity. Marking after the append therefore
// stamped the discarded copy whenever an unmarked twin got there first, which
// made literal immutability depend on declaration order within a compilation
// unit: '#(1 2 3) mutated iff a bare #(1 2 3) appeared above it. Marking before
// the append means the survivor was marked at its own append in either order,
// so no pooled aggregate is ever reachable unmarked and no dedup key has to
// know about immutability.
//
// A nil namespace (phase frames, some transient compile environments) is
// tolerated: markLiteralImmutable is a no-op on a nil set.
func (p *CompileTimeContinuation) appendConstantLiteral(v values.Value) machine.LiteralIndex {
	ns := p.env.Namespace()
	if ns != nil {
		markLiteralImmutable(v, ns.ImmutableLiterals(), make(map[values.Value]struct{}))
	}
	return p.template.MaybeAppendLiteral(v)
}

// markLiteralImmutable records every pair and vector reachable from v in the
// engine-scoped immutable-literal set. R7RS §4.1.2 makes quoted-literal
// constants immutable. Call it through appendConstantLiteral, which fixes the
// order relative to the literal pool; marking a datum that is then discarded by
// dedup is exactly the defect that order guards against. The visited map makes
// cyclic literals (#0=(1 . #0#)) terminate.
//
// The cdr spine is walked iteratively; only cars recurse, matching
// validateQuotedLiteralWithVisited (compile_time_continuation.go), which
// CompileValidatedQuote runs over the same literal just before this. Recursing
// per cdr made a proper list's LENGTH into Go stack depth, so a multi-million-
// element quoted literal overflowed the host stack at compile time. Car depth
// is nesting, which the parser already bounds.
//
// Unlike the validator's, these marks are never unwound: visited is a
// termination and de-duplication set, not a path-scoped cycle detector, so a
// node reached a second time by any route is already done.
func markLiteralImmutable(v values.Value, set *environment.ImmutableLiterals, visited map[values.Value]struct{}) {
	if set == nil {
		return
	}
	switch obj := v.(type) {
	case *values.Pair:
		cur := obj
		for {
			_, seen := visited[cur]
			if seen {
				return
			}
			visited[cur] = struct{}{}
			set.Mark(cur)
			markLiteralImmutable(cur.Car(), set, visited)

			next, isPair := cur.Cdr().(*values.Pair)
			if !isPair || next == nil {
				markLiteralImmutable(cur.Cdr(), set, visited)
				return
			}
			cur = next
		}
	case *values.Vector:
		_, seen := visited[obj]
		if seen {
			return
		}
		visited[obj] = struct{}{}
		set.Mark(obj)
		for _, elem := range *obj {
			markLiteralImmutable(elem, set, visited)
		}
	case *values.ByteVector:
		// Elements are *Byte leaves (no nested aggregates), so mark the
		// bytevector itself without recursing. R7RS §4.1.2.
		set.Mark(obj)
	}
}
