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

package schemeutil

import (
	"context"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// DefaultMaxDatumToSyntaxDepth bounds structural NESTING depth when converting
// a datum to syntax. Without a bound, a deeply nested datum — one built at
// runtime, so no reader ever bounded it — triggers a fatal, unrecoverable Go
// stack overflow that kills the host process. Same rationale, and the same
// value, as parser.DefaultMaxParseDepth and values.DefaultMaxWriteDepth: what
// the reader refuses to READ and the writer refuses to WRITE, the converter
// refuses to CONVERT. Declared here rather than taken from pkg/parser because
// parser imports this package.
//
// LENGTH is not bounded and does not need to be: both spine walks are
// iterative, so a list of any length costs constant stack.
const DefaultMaxDatumToSyntaxDepth = 10000

// DatumToSyntaxValue wraps a raw Scheme datum in syntax objects, attaching
// the provided SourceContext for source location and scope tracking.
// Recursively wraps pairs, vectors, and boxed values. If the input is already
// a SyntaxValue, it is returned unchanged.
//
// A circular datum is refused with werr.ErrCircularList. It has to be: every
// caller hands the result to the expander or the compiler, and a circular
// program is not a program. Before the refusal, (eval x) on a datum with a
// car, vector or box cycle killed the HOST PROCESS with a runtime stack
// overflow — a throw, not a panic, so no recover could contain it and every
// other engine in the process died with it. A cdr cycle failed more quietly
// and worse: the spine walk's ErrCircularList was discarded, the list was
// silently truncated at the cycle, and the mangled graph surfaced downstream
// as a diagnostic naming an internal syntax type.
//
// Refusing here rather than memoizing into a cyclic syntax graph is the same
// rule the compiler already applies to circular quoted data, and it is the
// only one that terminates: a faithful cyclic graph merely moves the hang into
// the expander, which was measured before this shape was chosen.
func DatumToSyntaxValue(ctx context.Context, sctx *syntax.SourceContext, o values.Value) (syntax.SyntaxValue, error) {
	return datumToSyntaxValueVisited(ctx, sctx, o, values.NewMapSet[values.Value](0), 0)
}

// datumToSyntaxValueVisited is DatumToSyntaxValue's worker. visited is
// PATH-scoped — entries are removed on the way back out — so shared structure
// converts exactly as it did before and only a true cycle is refused.
func datumToSyntaxValueVisited(ctx context.Context, sctx *syntax.SourceContext, o values.Value, visited values.MapSet[values.Value], depth int) (syntax.SyntaxValue, error) {
	if depth > DefaultMaxDatumToSyntaxDepth {
		return nil, werr.WrapForeignErrorf(werr.ErrParseDepthExceeded,
			"DatumToSyntaxValue: datum nested deeper than %d", DefaultMaxDatumToSyntaxDepth)
	}
	if values.IsVoid(o) {
		return syntax.SyntaxVoid, nil
	}
	if values.IsEmptyList(o) {
		return syntax.SyntaxEmptyList, nil
	}
	switch v := o.(type) {
	case syntax.SyntaxValue:
		// Already-wrapped syntax passes through unchanged. This arm MUST precede
		// the values.Tuple arm below: *SyntaxPair also satisfies Tuple (see the
		// `var _ values.Tuple = (*SyntaxPair)(nil)` assertion in
		// pkg/syntax/syntax_pair.go), so matching Tuple first would rebuild it
		// with the caller's sctx, discarding its own source location and scope
		// set. (*SyntaxVector is not a Tuple, but reaches this arm as a
		// SyntaxValue and is likewise returned unchanged.)
		return v, nil
	case *values.Symbol:
		return syntax.NewSyntaxSymbol(v.Key, sctx), nil
	case values.Tuple:
		return datumListToSyntaxValue(ctx, sctx, o, v, visited, depth)
	case *values.Box:
		dup := visited.Get(o)
		if dup {
			return nil, circularDatumError()
		}
		visited.Set(o)
		defer visited.Unset(o)
		inner, err := datumToSyntaxValueVisited(ctx, sctx, v.Unbox(), visited, depth+1)
		if err != nil {
			return nil, err
		}
		return syntax.NewSyntaxObject(values.NewBox(inner), sctx), nil
	case *values.Vector:
		dup := visited.Get(o)
		if dup {
			return nil, circularDatumError()
		}
		visited.Set(o)
		defer visited.Unset(o)
		vt0 := syntax.NewSyntaxVector(sctx)
		for _, elem := range v.Elems() {
			e, err := datumToSyntaxValueVisited(ctx, sctx, elem, visited, depth+1)
			if err != nil {
				return nil, err
			}
			vt0.Values = append(vt0.Values, e)
		}
		return vt0, nil
	default:
		// If the datum is not a Datum, we convert it to a Datum first.
		return syntax.NewSyntaxObject(v, sctx), nil
	}
}

// datumListToSyntaxValue converts one cons spine iteratively, so list LENGTH
// never becomes Go stack depth; only cars recurse, and that nesting is bounded
// by DefaultMaxDatumToSyntaxDepth. Every cell walked stays in visited for the whole
// walk, so a cdr pointing back at ANY earlier cell is caught, not only one
// pointing at the head. An improper tail falls out of the same loop: it is
// just a cdr that is not a Tuple.
//
// The depth counter does NOT advance along the spine, only into cars: a long
// list is not a deep one, and bounding length would refuse legal data.
//
// This replaces a Pair.ForEach whose ErrCircularList return was discarded.
// ctx is no longer polled: the walk is now bounded by the size of a datum
// already resident in memory, and the caller re-checks the deadline on the
// very next step.
func datumListToSyntaxValue(ctx context.Context, sctx *syntax.SourceContext, o values.Value, v values.Tuple, visited values.MapSet[values.Value], depth int) (syntax.SyntaxValue, error) {
	dup := visited.Get(o)
	if dup {
		return nil, circularDatumError()
	}
	spine := []values.Value{o}
	visited.Set(o)
	defer func() {
		for _, cell := range spine {
			visited.Unset(cell)
		}
	}()

	head := syntax.NewSyntaxCons(nil, nil, sctx)
	cur, place := v, head
	for {
		car, err := datumToSyntaxValueVisited(ctx, sctx, cur.Car(), visited, depth+1)
		if err != nil {
			return nil, err
		}
		place.Values[0] = car

		cdr := cur.Cdr()
		next, isTuple := cdr.(values.Tuple)
		if !isTuple || values.IsEmptyList(cdr) {
			tail, err := datumToSyntaxValueVisited(ctx, sctx, cdr, visited, depth+1)
			if err != nil {
				return nil, err
			}
			place.Values[1] = tail
			return head, nil
		}
		dup := visited.Get(cdr)
		if dup {
			return nil, circularDatumError()
		}
		visited.Set(cdr)
		spine = append(spine, cdr)

		cell := syntax.NewSyntaxCons(nil, nil, sctx)
		place.Values[1] = cell
		cur, place = next, cell
	}
}

// circularDatumError is the one diagnostic every cyclic shape produces here.
func circularDatumError() error {
	return werr.WrapForeignErrorf(werr.ErrCircularList, "DatumToSyntaxValue: circular datum")
}

// IsSyntaxComment returns true if the given value is a SyntaxComment.
func IsSyntaxComment(v values.Value) bool {
	switch v.(type) {
	case *syntax.SyntaxComment, *syntax.SyntaxDatumComment:
		return true
	}
	return false
}
