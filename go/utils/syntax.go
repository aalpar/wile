package utils

import (
	"skeme/syntax"
	"skeme/values"
)

// SyntaxValueToDatum converts a syntax object back to a raw Scheme datum,
// stripping away source location and scope information. Recursively unwraps
// pairs, vectors, and boxed values.
func SyntaxValueToDatum(sv values.Value) values.Value {
	if syntax.IsSyntaxVoid(sv) {
		return values.Void
	}
	if syntax.IsSyntaxEmptyList(sv) {
		return values.EmptyList
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
			if syntax.IsSyntaxEmptyList(cdr) {
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
		if bx, ok := v.Datum.(*values.Box); ok {
			return values.NewBox(SyntaxValueToDatum(bx.Unbox()))
		}
		return v.Datum
	case *syntax.SyntaxSymbol:
		return values.NewSymbol(v.Key)
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
func DatumToSyntaxValue(sctx *syntax.SourceContext, o values.Value) syntax.SyntaxValue {
	if values.IsVoid(o) {
		return syntax.SyntaxVoid
	}
	if values.IsEmptyList(o) {
		return syntax.NewSyntaxEmptyList(sctx)
	}
	switch v := o.(type) {
	case *values.Symbol:
		return syntax.NewSyntaxSymbol(v.Key, sctx)
	case *values.Pair:
		// If the datum is a Datum, we wrap it in a SyntaxValue with a zero source context.
		var pr0stx *syntax.SyntaxPair
		pr1, ok := v.Cdr().(*values.Pair)
		if !ok {
			// If the cdr is not a Pair, we create a new Pair with an empty list as the cdr.
			return syntax.NewSyntaxCons(DatumToSyntaxValue(sctx, v.Car()), DatumToSyntaxValue(sctx, v.Cdr()), sctx)
		} else {
			var v0 values.Value
			var pr *syntax.SyntaxPair
			pr0stx = syntax.NewSyntaxCons(DatumToSyntaxValue(sctx, v.Car()), DatumToSyntaxValue(sctx, values.EmptyList), sctx)
			pr = pr0stx
			v0, _ = pr1.ForEach(func(i int, hasNext bool, v1 values.Value) error {
				pr.SetCdr(
					syntax.NewSyntaxCons(
						DatumToSyntaxValue(sctx, v1),
						DatumToSyntaxValue(sctx, values.EmptyList),
						sctx))
				pr = pr.Cdr().(*syntax.SyntaxPair)
				return nil
			})
			pr.SetCdr(DatumToSyntaxValue(sctx, v0))
			return pr0stx
		}
	case *values.Box:
		bx0 := values.NewBox(DatumToSyntaxValue(sctx, v.Unbox()))
		return syntax.NewSyntaxObject(bx0, sctx)
	case *values.Vector:
		vt0 := syntax.NewSyntaxVector(sctx)
		for i := range *v {
			vt0.Values = append(vt0.Values, DatumToSyntaxValue(sctx, (*v)[i]))
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
