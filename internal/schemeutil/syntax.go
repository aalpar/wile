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

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

// SyntaxValueToDatum converts a syntax object back to a raw Scheme datum,
// stripping away source location and scope information. Recursively unwraps
// pairs, vectors, and boxed values.
func SyntaxValueToDatum(sv values.Value) values.Value {
	syntaxVal, ok := sv.(syntax.SyntaxValue)
	if ok {
		if syntax.IsSyntaxVoid(syntaxVal) {
			return values.Void
		}
		if syntax.IsSyntaxEmptyList(syntaxVal) {
			return values.EmptyList
		}
	}
	switch v := sv.(type) {
	case *syntax.SyntaxPair:
		// Use a loop to traverse the list spine to avoid stack overflow
		var cars []values.Value
		var improperCdr values.Value
		curr := v
		for {
			cars = append(cars, SyntaxValueToDatum(curr.Car()))
			cdr := curr.Cdr()
			cdrSyntax, ok := cdr.(syntax.SyntaxValue)
			if ok && syntax.IsSyntaxEmptyList(cdrSyntax) {
				break
			}
			next, ok := cdr.(*syntax.SyntaxPair)
			if !ok {
				// Improper list - convert the final cdr
				improperCdr = SyntaxValueToDatum(cdr)
				break
			}
			curr = next
		}
		// Build list from end to avoid needing SetCdr
		var result values.Value
		if improperCdr != nil {
			result = improperCdr
		} else {
			result = values.EmptyList
		}
		for i := len(cars) - 1; i >= 0; i-- {
			result = values.NewCons(cars[i], result)
		}
		return result
	case *syntax.SyntaxVector:
		vt := make(values.Vector, len(v.Values))
		for i := range v.Values {
			vt[i] = SyntaxValueToDatum(v.Values[i])
		}
		return &vt
	case *syntax.SyntaxObject:
		bx, ok := v.Datum().(*values.Box)
		if ok {
			return values.NewBox(SyntaxValueToDatum(bx.Unbox()))
		}
		return v.Datum()
	case *syntax.SyntaxSymbol:
		return v.Datum()
	case values.Value:
		return v
	default:
		return nil
	}
}

// DatumToSyntaxValue wraps a raw Scheme datum in syntax objects, attaching
// the provided SourceContext for source location and scope tracking.
// Recursively wraps pairs, vectors, and boxed values. If the input is already
// a SyntaxValue, it is returned unchanged.
func DatumToSyntaxValue(ctx context.Context, sctx *syntax.SourceContext, o values.Value) syntax.SyntaxValue {
	if values.IsVoid(o) {
		return syntax.SyntaxVoid
	}
	if values.IsEmptyList(o) {
		return syntax.NewSyntaxEmptyList(sctx)
	}
	switch v := o.(type) {
	case *values.Symbol:
		return syntax.NewSyntaxSymbol(v.Key, sctx)
	case values.Tuple:
		// If the datum is a Tuple (Pair or ArrayList), wrap it in SyntaxValue
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
	case syntax.SyntaxValue:
		// If the datum is already a SyntaxValue, we can just return it.
		return v
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
