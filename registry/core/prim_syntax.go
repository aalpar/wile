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
	"fmt"
	"sync/atomic"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// gensymCounter is used to generate unique symbol names
var gensymCounter atomic.Uint64

// PrimIdentifierQ implements the identifier? predicate (R6RS).
// Returns #t if the argument is a syntax object representing an identifier.
func PrimIdentifierQ(mc *machine.MachineContext) error {
	obj := mc.Arg(0)

	_, ok := obj.(*syntax.SyntaxSymbol)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimSyntaxToDatum implements the syntax->datum procedure (R6RS).
// Recursively unwraps a syntax object to its underlying datum, stripping
// all lexical context information.
func PrimSyntaxToDatum(mc *machine.MachineContext) error {
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
func PrimDatumToSyntax(mc *machine.MachineContext) error {
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
	result := datumToSyntax(datum, sctx)
	mc.SetValue(result)
	return nil
}

// datumToSyntax recursively converts a datum to a syntax object.
func datumToSyntax(datum values.Value, sctx *syntax.SourceContext) syntax.SyntaxValue {
	switch v := datum.(type) {
	case *values.Symbol:
		return syntax.NewSyntaxSymbol(v.Key, sctx)

	case syntax.SyntaxValue:
		return v

	case values.Tuple:
		if v.IsEmptyList() {
			return syntax.SyntaxEmptyList
		}
		car := datumToSyntax(v.Car(), sctx)
		cdr := datumToSyntax(v.Cdr(), sctx)
		return syntax.NewSyntaxCons(car, cdr, sctx)

	case *values.Vector:
		data := v.Datum()
		elems := make([]syntax.SyntaxValue, len(data))
		for i, elem := range data {
			elems[i] = datumToSyntax(elem, sctx)
		}
		return syntax.NewSyntaxVector(sctx, elems...)

	default:
		// Other values (numbers, strings, booleans, etc.) get wrapped
		return syntax.NewSyntaxObject(v, sctx)
	}
}

// PrimGenerateTemporaries implements the generate-temporaries procedure (R6RS).
// Takes a list (or syntax list) and returns a list of fresh identifiers
// with the same length. Each identifier is guaranteed to be unique.
//
// (generate-temporaries stx-list) -> list of identifiers
func PrimGenerateTemporaries(mc *machine.MachineContext) error {
	arg := mc.Arg(0)

	// H7 FIX: Check that argument is a list before type assertion
	tuple, ok := arg.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList,
			"generate-temporaries: expected a list but got %T", arg)
	}

	// Count the length of the list
	count := tuple.Length()
	// Generate fresh identifiers
	elems := make([]values.Value, count)
	for i := range elems {
		id := gensymCounter.Add(1)
		name := fmt.Sprintf("g%d", id)
		elems[i] = syntax.NewSyntaxSymbol(name, nil)
	}
	result := values.List(elems...)

	mc.SetValue(result)
	return nil
}

// PrimBoundIdentifierEqualQ implements the bound-identifier=? predicate (R7RS).
// Returns #t if two identifiers have the same name AND the same scope sets,
// meaning they would create the same binding if used as binding occurrences.
func PrimBoundIdentifierEqualQ(mc *machine.MachineContext) error {
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

// PrimFreeIdentifierEqualQ implements the free-identifier=? predicate (R7RS).
// Returns #t if two identifiers would resolve to the same binding in the current environment.
// For unbound identifiers, returns #t if they have the same name.
func PrimFreeIdentifierEqualQ(mc *machine.MachineContext) error {
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
	binding0 := env.GetBinding(sym0, id0.Scopes())
	binding1 := env.GetBinding(sym1, id1.Scopes())

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

	// Both bound → same binding object?
	mc.SetValue(values.BoolToBoolean(binding0 == binding1))
	return nil
}
