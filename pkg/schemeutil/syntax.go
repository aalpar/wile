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
)

// DatumToSyntaxValue wraps a raw Scheme datum in syntax objects, attaching
// the provided SourceContext for source location and scope tracking.
// Recursively wraps pairs, vectors, and boxed values. If the input is already
// a SyntaxValue, it is returned unchanged.
func DatumToSyntaxValue(ctx context.Context, sctx *syntax.SourceContext, o values.Value) syntax.SyntaxValue {
	if values.IsVoid(o) {
		return syntax.SyntaxVoid
	}
	if values.IsEmptyList(o) {
		return syntax.SyntaxEmptyList
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
		return v
	case *values.Symbol:
		return syntax.NewSyntaxSymbol(v.Key, sctx)
	case values.Tuple:
		// If the datum is a Tuple (Pair), wrap it in SyntaxValue
		var tuple0stx *syntax.SyntaxPair
		tuple1, ok := v.Cdr().(values.Tuple)
		if !ok {
			// If the cdr is not a Tuple, we have an improper list - wrap both car and cdr
			return syntax.NewSyntaxCons(DatumToSyntaxValue(ctx, sctx, v.Car()), DatumToSyntaxValue(ctx, sctx, v.Cdr()), sctx)
		}
		var v0 values.Value
		var tuple *syntax.SyntaxPair
		tuple0stx = syntax.NewSyntaxCons(DatumToSyntaxValue(ctx, sctx, v.Car()), DatumToSyntaxValue(ctx, sctx, values.EmptyList), sctx)
		tuple = tuple0stx
		// improper-list aware: v0 captures the trailing non-list cdr (if any)
		// and is wired into the constructed syntax tail below. Do not migrate
		// to values.ForEachProperList — that helper rejects improper lists.
		v0, _ = tuple1.ForEach(ctx, func(_ context.Context, _ int, _ bool, v1 values.Value) error {
			tuple.SetCdr(
				syntax.NewSyntaxCons(
					DatumToSyntaxValue(ctx, sctx, v1),
					DatumToSyntaxValue(ctx, sctx, values.EmptyList),
					sctx))
			tuple = tuple.Cdr().(*syntax.SyntaxPair)
			return nil
		})
		tuple.SetCdr(DatumToSyntaxValue(ctx, sctx, v0))
		return tuple0stx
	case *values.Box:
		bx0 := values.NewBox(DatumToSyntaxValue(ctx, sctx, v.Unbox()))
		return syntax.NewSyntaxObject(bx0, sctx)
	case *values.Vector:
		vt0 := syntax.NewSyntaxVector(sctx)
		for i := range *v {
			vt0.Values = append(vt0.Values, DatumToSyntaxValue(ctx, sctx, (*v)[i]))
		}
		return vt0
	default:
		// If the datum is not a Datum, we convert it to a Datum first.
		return syntax.NewSyntaxObject(v, sctx)
	}
}

// IsSyntaxComment returns true if the given value is a SyntaxComment.
func IsSyntaxComment(v values.Value) bool {
	switch v.(type) {
	case *syntax.SyntaxComment, *syntax.SyntaxDatumComment:
		return true
	}
	return false
}
