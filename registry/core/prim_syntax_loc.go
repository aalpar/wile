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
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// requireSyntaxValue extracts a SyntaxValue from mc.Arg(0) or returns an error.
func requireSyntaxValue(mc *machine.MachineContext, name string) (syntax.SyntaxValue, error) {
	sv, ok := mc.Arg(0).(syntax.SyntaxValue)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotASyntaxObject,
			"%s: expected syntax object, got %T", name, mc.Arg(0))
	}
	return sv, nil
}

// PrimSyntaxSource returns the source file path of a syntax object, or #f
// if the syntax object has no source location.
//
// Racket §12.2: syntax-source
func PrimSyntaxSource(mc *machine.MachineContext) error {
	sv, err := requireSyntaxValue(mc, "syntax-source")
	if err != nil {
		return err
	}
	sctx := sv.SourceContext()
	if sctx == nil || sctx.File == "" {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.NewString(sctx.File))
	return nil
}

// PrimSyntaxLine returns the 1-based line number of a syntax object, or #f.
//
// Racket §12.2: syntax-line
func PrimSyntaxLine(mc *machine.MachineContext) error {
	sv, err := requireSyntaxValue(mc, "syntax-line")
	if err != nil {
		return err
	}
	sctx := sv.SourceContext()
	if sctx == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.NewInteger(int64(sctx.Start.Line())))
	return nil
}

// PrimSyntaxColumn returns the 0-based column of a syntax object, or #f.
//
// Racket §12.2: syntax-column
func PrimSyntaxColumn(mc *machine.MachineContext) error {
	sv, err := requireSyntaxValue(mc, "syntax-column")
	if err != nil {
		return err
	}
	sctx := sv.SourceContext()
	if sctx == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.NewInteger(int64(sctx.Start.Column())))
	return nil
}

// PrimSyntaxPosition returns the 0-based byte position of a syntax object, or #f.
//
// Racket §12.2: syntax-position
func PrimSyntaxPosition(mc *machine.MachineContext) error {
	sv, err := requireSyntaxValue(mc, "syntax-position")
	if err != nil {
		return err
	}
	sctx := sv.SourceContext()
	if sctx == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.NewInteger(int64(sctx.Start.Index())))
	return nil
}

// PrimSyntaxSpan returns the byte span (end - start) of a syntax object, or #f.
//
// Racket §12.2: syntax-span
func PrimSyntaxSpan(mc *machine.MachineContext) error {
	sv, err := requireSyntaxValue(mc, "syntax-span")
	if err != nil {
		return err
	}
	sctx := sv.SourceContext()
	if sctx == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}
	span := sctx.End.Index() - sctx.Start.Index()
	mc.SetValue(values.NewInteger(int64(span)))
	return nil
}

// PrimSyntaxToList converts a syntax pair chain to a list of syntax objects.
// Returns #f if the argument is a syntax object but not a proper syntax list.
// Raises an error if the argument is not a syntax object at all.
//
// Racket §12.2: syntax->list
func PrimSyntaxToList(mc *machine.MachineContext) error {
	sv, err := requireSyntaxValue(mc, "syntax->list")
	if err != nil {
		return err
	}

	var result []values.Value
	current := sv
	for {
		if syntax.IsSyntaxEmptyList(current) {
			mc.SetValue(values.List(result...))
			return nil
		}
		sp, ok := current.(*syntax.SyntaxPair)
		if !ok {
			mc.SetValue(values.FalseValue)
			return nil
		}
		result = append(result, sp.SyntaxCar())
		current = sp.SyntaxCdr()
	}
}
