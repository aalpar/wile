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
	"sort"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// closureArity returns the Scheme arity value for a single closure.
// Fixed arity -> integer, variadic -> (min . #f).
func closureArity(paramCount int, isVariadic bool) values.Value {
	if isVariadic {
		required := values.NewInteger(int64(paramCount - 1))
		return values.NewCons(required, values.FalseValue)
	}
	return values.NewInteger(int64(paramCount))
}

// PrimProcedureArity implements (procedure-arity proc).
func PrimProcedureArity(mc machine.CallContext) error {
	o := mc.Arg(0)
	callable, ok := o.(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"procedure-arity: expected procedure")
	}
	switch v := callable.(type) {
	case *machine.MachineClosure:
		tpl := v.Template()
		mc.SetValue(closureArity(tpl.ParameterCount(), tpl.IsVariadic()))
	case *machine.ForeignClosure:
		mc.SetValue(closureArity(v.ParameterCount(), v.IsVariadic()))
	case *machine.CaseLambdaClosure:
		clauses := v.Clauses()
		items := make([]values.Value, len(clauses))
		for i, clause := range clauses {
			tpl := clause.Template()
			items[i] = closureArity(tpl.ParameterCount(), tpl.IsVariadic())
		}
		mc.SetValue(values.List(items...))
	case *machine.Parameter:
		mc.SetValue(closureArity(1, true))
	case *machine.ComposableContinuation:
		mc.SetValue(values.NewInteger(1))
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimProcedureName implements (procedure-name proc).
func PrimProcedureName(mc machine.CallContext) error {
	o := mc.Arg(0)
	callable, ok := o.(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"procedure-name: expected procedure")
	}
	if nc, ok := callable.(interface{ Name() string }); ok {
		mc.SetValue(values.StringOrFalse(nc.Name()))
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// templateSourceLocation extracts the first non-nil source context from
// a NativeTemplate and returns it as a (file line column) list, or #f.
func templateSourceLocation(tpl *machine.NativeTemplate) values.Value {
	src := tpl.SourceAt(0)
	if src == nil {
		return values.FalseValue
	}
	if src.File == "" {
		return values.FalseValue
	}
	return values.List(
		values.NewString(src.File),
		values.NewInteger(int64(src.Start.Line())),
		values.NewInteger(int64(src.Start.Column())),
	)
}

// PrimProcedureSourceLocation implements (procedure-source-location proc).
func PrimProcedureSourceLocation(mc machine.CallContext) error {
	o := mc.Arg(0)
	callable, ok := o.(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"procedure-source-location: expected procedure")
	}
	switch v := callable.(type) {
	case *machine.MachineClosure:
		mc.SetValue(templateSourceLocation(v.Template()))
	case *machine.ForeignClosure:
		// Foreign closures (Go primitives) have no Scheme source location.
		mc.SetValue(values.FalseValue)
	case *machine.CaseLambdaClosure:
		clauses := v.Clauses()
		if len(clauses) > 0 {
			mc.SetValue(templateSourceLocation(clauses[0].Template()))
		} else {
			mc.SetValue(values.FalseValue)
		}
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// closureBoundSymbols extracts the bound symbols from a MachineClosure's
// captured local environment.
func closureBoundSymbols(cls *machine.MachineClosure) values.Value {
	env := cls.Env()
	if env == nil {
		return values.FalseValue
	}
	local := env.LocalEnvironment()
	if local == nil {
		return values.FalseValue
	}
	keys := local.Keys()
	if len(keys) == 0 {
		return values.EmptyList
	}
	// Symbols returned here are un-interned copies (addressable stack
	// locals), so they will not be eq? to interned symbols. Callers
	// should compare via symbol->string.
	syms := make([]values.Value, 0, len(keys))
	for sym := range keys {
		syms = append(syms, &sym)
	}
	return values.List(syms...)
}

// PrimProcedureBoundSymbols implements (procedure-bound-symbols proc).
func PrimProcedureBoundSymbols(mc machine.CallContext) error {
	o := mc.Arg(0)
	callable, ok := o.(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"procedure-bound-symbols: expected procedure")
	}
	switch v := callable.(type) {
	case *machine.MachineClosure:
		mc.SetValue(closureBoundSymbols(v))
	case *machine.CaseLambdaClosure:
		clauses := v.Clauses()
		if len(clauses) > 0 {
			mc.SetValue(closureBoundSymbols(clauses[0]))
		} else {
			mc.SetValue(values.FalseValue)
		}
	default:
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimProcedureType implements (procedure-type proc).
func PrimProcedureType(mc machine.CallContext) error {
	o := mc.Arg(0)
	callable, ok := o.(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"procedure-type: expected procedure")
	}
	var typeName string
	switch callable.(type) {
	case *machine.MachineClosure:
		typeName = "lambda"
	case *machine.ForeignClosure:
		typeName = "foreign"
	case *machine.CaseLambdaClosure:
		typeName = "case-lambda"
	case *machine.Parameter:
		typeName = "parameter"
	case *machine.ComposableContinuation:
		typeName = "continuation"
	default:
		typeName = "unknown"
	}
	mc.SetValue(values.NewSymbol(typeName))
	return nil
}

// PrimProcedureDocumentation implements (procedure-documentation proc).
// Returns the docstring attached to a procedure, or #f if none.
// For Scheme closures, the docstring is extracted from the body (Guile convention).
// For foreign closures, the docstring comes from PrimitiveSpec.Doc.
func PrimProcedureDocumentation(mc machine.CallContext) error {
	o := mc.Arg(0)
	callable, ok := o.(values.Callable)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure,
			"procedure-documentation: expected procedure")
	}
	if dc, ok := callable.(interface{ Doc() string }); ok {
		mc.SetValue(values.StringOrFalse(dc.Doc()))
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// registryFromContext extracts the *registry.Registry from the MachineContext's
// namespace. Returns nil if unavailable.
func registryFromContext(mc machine.CallContext) *registry.Registry {
	ns := mc.EnvironmentFrame().Namespace()
	if ns == nil {
		return nil
	}
	regAny := ns.Registry()
	if regAny == nil {
		return nil
	}
	reg, ok := regAny.(*registry.Registry)
	if !ok {
		return nil
	}
	return reg
}

// PrimDocTopics implements (doc-topics).
// Returns a sorted list of category name strings.
func PrimDocTopics(mc machine.CallContext) error {
	reg := registryFromContext(mc)
	if reg == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}

	byCategory := reg.PrimitivesByCategory()
	cats := make([]string, 0, len(byCategory))
	for cat := range byCategory {
		if cat != "" {
			cats = append(cats, cat)
		}
	}
	sort.Strings(cats)

	items := make([]values.Value, len(cats))
	for i, cat := range cats {
		items[i] = values.NewString(cat)
	}
	mc.SetValue(values.List(items...))
	return nil
}

// PrimDocTopic implements (doc-topic category).
// Returns a sorted list of symbols in the named category.
func PrimDocTopic(mc machine.CallContext) error {
	s, ok := mc.Arg(0).(*values.String)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAString,
			"doc-topic: expected string category name")
	}
	category := s.Value

	reg := registryFromContext(mc)
	if reg == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}

	byCategory := reg.PrimitivesByCategory()
	prims, found := byCategory[category]
	if !found {
		mc.SetValue(values.EmptyList)
		return nil
	}

	names := make([]string, len(prims))
	for i, pr := range prims {
		names[i] = pr.Spec.Name
	}
	sort.Strings(names)

	syms := make([]values.Value, len(names))
	for i, n := range names {
		syms[i] = values.NewSymbol(n)
	}
	mc.SetValue(values.List(syms...))
	return nil
}

// PrimLibraryDescription implements (library-description library-name).
// Returns the description string of a loaded library, or #f if none or not loaded.
func PrimLibraryDescription(mc machine.CallContext) error {
	nameList := mc.Arg(0)
	libName, err := compilation.ParseLibraryNameFromDatum(mc.Context(), nameList)
	if err != nil {
		return werr.WrapForeignErrorf(err, "library-description: invalid library name")
	}

	regAny := mc.EnvironmentFrame().LibraryRegistry()
	if regAny == nil {
		mc.SetValue(values.FalseValue)
		return nil
	}
	reg, ok := regAny.(*compilation.LibraryRegistry)
	if !ok {
		mc.SetValue(values.FalseValue)
		return nil
	}

	lib := reg.Lookup(libName)
	if lib == nil || lib.Description == "" {
		mc.SetValue(values.FalseValue)
		return nil
	}
	mc.SetValue(values.NewString(lib.Description))
	return nil
}

// PrimApropos implements (apropos pattern).
// Returns a sorted list of symbols whose name, doc, or category contains
// the pattern as a case-insensitive substring. Searches all documentation
// sources: primitives, binding specs, doc entries, environment bindings,
// and loaded libraries.
func PrimApropos(mc machine.CallContext) error {
	s, ok := mc.Arg(0).(*values.String)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAString,
			"apropos: expected string pattern")
	}

	reg := registryFromContext(mc)
	if reg == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}

	env := mc.EnvironmentFrame()
	var libReg *compilation.LibraryRegistry
	if env != nil {
		lr, ok := env.LibraryRegistry().(*compilation.LibraryRegistry)
		if ok {
			libReg = lr
		}
	}

	results := registry.SearchDoc(reg, env, libReg, s.Value)
	syms := make([]values.Value, len(results))
	for i, r := range results {
		syms[i] = values.NewSymbol(r.Name)
	}
	mc.SetValue(values.List(syms...))
	return nil
}
