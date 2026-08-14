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

package core

import (
	"context"
	"fmt"
	"sync/atomic"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/schemeutil"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// gensymCounter is used to generate unique symbol names
var gensymCounter atomic.Uint64

// PrimIdentifierQ implements the identifier? predicate (R6RS).
// Returns #t if the argument is a syntax object representing an identifier.
func PrimIdentifierQ(mc machine.CallContext) error {
	obj := mc.Arg(0)

	_, ok := obj.(*syntax.SyntaxSymbol)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimSyntaxToDatum implements the syntax->datum procedure (R6RS).
// Recursively unwraps a syntax object to its underlying datum, stripping
// all lexical context information.
func PrimSyntaxToDatum(mc machine.CallContext) error {
	obj := mc.Arg(0)

	stx, ok := obj.(syntax.SyntaxValue)
	if !ok {
		// If not a syntax object, return the value unchanged
		mc.SetValue(obj)
		return nil
	}

	mc.SetValue(stx.UnwrapAll())
	return nil
}

// PrimDatumToSyntax implements the datum->syntax procedure (R6RS).
// Converts a datum to a syntax object using the lexical context from template-id.
// If template-id is #f, the datum has no lexical context.
//
// (datum->syntax template-id datum) -> syntax-object
func PrimDatumToSyntax(mc machine.CallContext) error {
	templateArg := mc.Arg(0)
	datum := mc.Arg(1)

	// Get source context from template identifier
	var sctx *syntax.SourceContext
	if templateArg == values.FalseValue {
		// #f means no lexical context
		sctx = nil
	} else {
		templateID, ok := templateArg.(*syntax.SyntaxSymbol)
		if ok {
			sctx = templateID.SourceContext()
		} else {
			templateStx, ok := templateArg.(syntax.SyntaxValue)
			if ok {
				sctx = templateStx.SourceContext()
			} else {
				return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "datum->syntax: template-id must be an identifier, syntax object, or #f")
			}
		}
	}

	// Convert datum to syntax
	result, err := datumToSyntax(datum, sctx)
	if err != nil {
		return err
	}
	mc.SetValue(result)
	return nil
}

// datumToSyntax converts a datum to a syntax object, refusing a circular one.
//
// Two failures live here, and only one of them is the cycle. (set-cdr! x x)
// followed by (datum->syntax #f x) killed the host process with a runtime
// stack overflow — a throw, not a panic, so the VM boundary's recover could
// not contain it and every other engine in the process died with it. So did a
// car cycle and a self-referential vector. Separately, a long PROPER list did
// the same, because recursing per cdr turns list length into Go stack depth,
// which no reader bound reaches: a datum built at runtime never passed one.
// The iterative spine below fixes the second, visited fixes the first, and a
// depth counter fixes the third — NESTING, which stays recursive and which
// the same runtime-construction argument leaves unbounded.
//
// Refusal, rather than a memoized cyclic syntax graph, keeps one rule instead
// of two: the compiler already refuses circular quoted data
// (validateQuotedLiteralWithVisited), and a cyclic graph handed to the
// expander merely relocates the hang — measured.
func datumToSyntax(datum values.Value, sctx *syntax.SourceContext) (syntax.SyntaxValue, error) {
	return datumToSyntaxVisited(datum, sctx, map[values.Value]bool{}, 0)
}

// datumToSyntaxVisited is datumToSyntax's worker. visited is PATH-scoped —
// entries are removed on the way back out — so shared structure converts as
// before and only a true cycle is refused.
func datumToSyntaxVisited(datum values.Value, sctx *syntax.SourceContext, visited map[values.Value]bool, depth int) (syntax.SyntaxValue, error) {
	if depth > schemeutil.DefaultMaxDatumToSyntaxDepth {
		return nil, werr.WrapForeignErrorf(werr.ErrParseDepthExceeded,
			"datum->syntax: datum nested deeper than %d", schemeutil.DefaultMaxDatumToSyntaxDepth)
	}
	switch v := datum.(type) {
	case *values.Symbol:
		return syntax.NewSyntaxSymbol(v.Key, sctx), nil

	case syntax.SyntaxValue:
		return v, nil

	case values.Tuple:
		if v.IsEmptyList() {
			return syntax.SyntaxEmptyList, nil
		}
		return datumListToSyntax(datum, v, sctx, visited, depth)

	case *values.Vector:
		// Void guard is required: *v panics on a nil *Vector. Empty
		// syntax-vector matches the historical Datum()-returns-nil
		// path. Pinned by TestDatumToSyntax_VoidVector.
		if v.IsVoid() {
			return syntax.NewSyntaxVector(sctx), nil
		}
		if visited[datum] {
			return nil, circularDatumError()
		}
		visited[datum] = true
		defer delete(visited, datum)
		elems := make([]syntax.SyntaxValue, v.Length())
		for i, elem := range v.Elems() {
			e, err := datumToSyntaxVisited(elem, sctx, visited, depth+1)
			if err != nil {
				return nil, err
			}
			elems[i] = e
		}
		return syntax.NewSyntaxVector(sctx, elems...), nil

	default:
		// Other values (numbers, strings, booleans, etc.) get wrapped
		return syntax.NewSyntaxObject(v, sctx), nil
	}
}

// datumListToSyntax converts one cons spine iteratively. Every cell it walks
// stays in visited for the whole walk, so a cdr pointing back at ANY earlier
// cell of the same spine is caught, not just one pointing at the head.
//
// depth does NOT advance along the spine, only into cars: a long list is not
// a deep one, and bounding length would refuse legal data.
func datumListToSyntax(datum values.Value, v values.Tuple, sctx *syntax.SourceContext, visited map[values.Value]bool, depth int) (syntax.SyntaxValue, error) {
	if visited[datum] {
		return nil, circularDatumError()
	}
	spine := []values.Value{datum}
	visited[datum] = true
	defer func() {
		for _, cell := range spine {
			delete(visited, cell)
		}
	}()

	head := syntax.NewSyntaxCons(nil, nil, sctx)
	cur, place := v, head
	for {
		car, err := datumToSyntaxVisited(cur.Car(), sctx, visited, depth+1)
		if err != nil {
			return nil, err
		}
		place.Values[0] = car

		cdr := cur.Cdr()
		next, isTuple := cdr.(values.Tuple)
		if !isTuple || next.IsEmptyList() {
			tail, err := datumToSyntaxVisited(cdr, sctx, visited, depth+1)
			if err != nil {
				return nil, err
			}
			place.Values[1] = tail
			return head, nil
		}
		if visited[cdr] {
			return nil, circularDatumError()
		}
		visited[cdr] = true
		spine = append(spine, cdr)

		cell := syntax.NewSyntaxCons(nil, nil, sctx)
		place.Values[1] = cell
		cur, place = next, cell
	}
}

// circularDatumError is the one diagnostic all three cyclic shapes produce.
func circularDatumError() error {
	return werr.WrapForeignErrorf(werr.ErrCircularList, "datum->syntax: circular datum")
}

// PrimGenerateTemporaries implements the generate-temporaries procedure (R6RS).
// Takes a list (or syntax list) and returns a list of fresh identifiers
// with the same length. Each identifier is guaranteed to be unique.
//
// (generate-temporaries stx-list) -> list of identifiers
func PrimGenerateTemporaries(mc machine.CallContext) error {
	arg := mc.Arg(0)

	// H7 FIX: Check that argument is a list before type assertion
	tuple, ok := arg.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList,
			"generate-temporaries: expected a list but got %T", arg)
	}

	// Walk with the proper-list eliminator rather than Tuple.Length: Length
	// panics on an improper tail (an uncatchable error at the Scheme level)
	// and loops forever on a circular list. ForEachProperList rejects both as
	// a wrapped ErrNotAList, which guard can catch.
	var elems []values.Value
	err := values.ForEachProperList(mc.Context(), tuple, "generate-temporaries",
		func(_ context.Context, _ int, _ bool, _ values.Value) error {
			id := gensymCounter.Add(1)
			elems = append(elems, syntax.NewSyntaxSymbol(fmt.Sprintf("g%d", id), nil))
			return nil
		})
	if err != nil {
		return err
	}

	mc.SetValue(values.List(elems...))
	return nil
}

// PrimBoundIdentifierEqualQ implements the bound-identifier=? predicate (R6RS).
// Returns #t if two identifiers have the same name AND the same scope sets,
// meaning they would create the same binding if used as binding occurrences.
func PrimBoundIdentifierEqualQ(mc machine.CallContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	id0, ok0 := o0.(*syntax.SyntaxSymbol)
	if !ok0 {
		return werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "bound-identifier=?: argument 1 is not an identifier")
	}
	id1, ok1 := o1.(*syntax.SyntaxSymbol)
	if !ok1 {
		return werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "bound-identifier=?: argument 2 is not an identifier")
	}

	// Same name?
	if id0.Sym.Key != id1.Sym.Key {
		mc.SetValue(values.FalseValue)
		return nil
	}

	// Same scopes? (set equality = mutual subset check)
	scopes0 := id0.Scopes()
	scopes1 := id1.Scopes()
	result := syntax.ScopesMatch(scopes0, scopes1) && syntax.ScopesMatch(scopes1, scopes0)
	mc.SetValue(values.BoolToBoolean(result))
	return nil
}

// PrimFreeIdentifierEqualQ implements the free-identifier=? predicate (R6RS).
// Returns #t if two identifiers would resolve to the same binding in the current environment.
// For unbound identifiers, returns #t if they have the same name.
func PrimFreeIdentifierEqualQ(mc machine.CallContext) error {
	o0 := mc.Arg(0)
	o1 := mc.Arg(1)

	id0, ok0 := o0.(*syntax.SyntaxSymbol)
	if !ok0 {
		return werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "free-identifier=?: argument 1 is not an identifier")
	}
	id1, ok1 := o1.(*syntax.SyntaxSymbol)
	if !ok1 {
		return werr.WrapForeignErrorf(werr.ErrNotASyntaxSymbol, "free-identifier=?: argument 2 is not an identifier")
	}

	env := mc.EnvironmentFrame()
	sym0 := values.NewSymbol(id0.Sym.Key)
	sym1 := values.NewSymbol(id1.Sym.Key)

	// Look up bindings for both identifiers
	binding0 := env.GetBinding(sym0, syntax.ScopesOf(id0.Scopes()))
	binding1 := env.GetBinding(sym1, syntax.ScopesOf(id1.Scopes()))

	// Both unbound → compare names (free references to same global)
	if binding0 == nil && binding1 == nil {
		mc.SetValue(values.BoolToBoolean(id0.Sym.Key == id1.Sym.Key))
		return nil
	}

	// One bound, one unbound → not equal
	if binding0 == nil || binding1 == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}

	// Both bound → same variable? Same object, or two imports sharing one
	// import-provenance root (renamed/re-exported aliases of one binding).
	mc.SetValue(values.BoolToBoolean(environment.SameBinding(binding0, binding1)))
	return nil
}
