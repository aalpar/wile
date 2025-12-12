// Copyright 2025 Aaron Alpar
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

package primitives

import (
	"context"

	"wile/machine"
	"wile/syntax"
	"wile/values"
)

// PrimDatumToSyntax implements the datum->syntax procedure (R6RS).
// Converts a datum to a syntax object using the lexical context from template-id.
// If template-id is #f, the datum has no lexical context.
//
// (datum->syntax template-id datum) -> syntax-object
func PrimDatumToSyntax(_ context.Context, mc *machine.MachineContext) error {
	templateArg := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	datum := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	// Get source context from template identifier
	var sctx *syntax.SourceContext
	if templateArg == values.FalseValue {
		// #f means no lexical context
		sctx = nil
	} else if templateID, ok := templateArg.(*syntax.SyntaxSymbol); ok {
		sctx = templateID.SourceContext()
	} else if templateStx, ok := templateArg.(syntax.SyntaxValue); ok {
		sctx = templateStx.SourceContext()
	} else {
		return values.NewForeignError("datum->syntax: template-id must be an identifier, syntax object, or #f")
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

	case *values.Pair:
		if v.IsEmptyList() {
			return syntax.NewSyntaxEmptyList(sctx)
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

	case syntax.SyntaxValue:
		// Already a syntax value, return as-is
		return v

	default:
		// Other values (numbers, strings, booleans, etc.) get wrapped
		return syntax.NewSyntaxObject(v, sctx)
	}
}
