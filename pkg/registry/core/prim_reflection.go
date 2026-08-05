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
	"errors"
	"reflect"
	"runtime"
	"slices"
	"strconv"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// primitiveSpecTag is the OpaqueValue tag for registry.PrimitiveRegistration
// wrappers returned by PrimRegisteredPrimitives.
const primitiveSpecTag = "primitive-spec"

// errNotPrimitiveSpec is the sentinel for accessor calls on non-spec values.
var errNotPrimitiveSpec = werr.NewStaticError("not a primitive-spec")

// unwrapPrimitiveSpec extracts a *registry.PrimitiveRegistration from a
// Scheme value, returning a descriptive error if the value has the wrong
// type, tag, or payload.
func unwrapPrimitiveSpec(v values.Value, op string) (*registry.PrimitiveRegistration, error) {
	o, ok := v.(*values.OpaqueValue)
	if !ok {
		return nil, werr.WrapForeignErrorf(errNotPrimitiveSpec,
			"%s: expected primitive-spec, got %T", op, v)
	}
	if o.OpaqueTag() != primitiveSpecTag {
		return nil, werr.WrapForeignErrorf(errNotPrimitiveSpec,
			"%s: expected primitive-spec, got opaque tag %q", op, o.OpaqueTag())
	}
	reg, ok := o.Unwrap().(*registry.PrimitiveRegistration)
	if !ok {
		return nil, werr.WrapForeignErrorf(errNotPrimitiveSpec,
			"%s: corrupted primitive-spec payload", op)
	}
	return reg, nil
}

// resolveImplLocation returns the fully-qualified Go function name and
// "file:line" source location for a PrimitiveSpec.Impl, or empty strings
// for binding-only primitives (nil Impl). Callers mirror the wile repo
// audit_manifest_test.go resolveImpl logic — this is the runtime
// equivalent, exposed to Scheme.
func resolveImplLocation(impl machine.ForeignFunction) (goFunc, goSource string) {
	if impl == nil {
		return "", ""
	}
	v := reflect.ValueOf(impl)
	if v.Kind() != reflect.Func || v.IsNil() {
		return "", ""
	}
	pc := v.Pointer()
	rf := runtime.FuncForPC(pc)
	if rf == nil {
		return "", ""
	}
	file, line := rf.FileLine(pc)
	if file == "" || line == 0 {
		return rf.Name(), ""
	}
	return rf.Name(), file + ":" + strconv.Itoa(line)
}

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
	case *machine.ComposableContinuation, *machine.CapturedContinuation:
		// A continuation accepts any number of values (R7RS §6.10): it resumes
		// with however many it is invoked with. Report (0 . #f) — variadic from
		// zero — matching its AcceptsArity (always true). call/cc hands Scheme a
		// CapturedContinuation; call-with-composable-continuation a
		// ComposableContinuation; both are variadic.
		mc.SetValue(closureArity(1, true))
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
	if nc, ok := callable.(machine.NamedCallable); ok {
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
		typeName = "closure"
	case *machine.ForeignClosure:
		typeName = "foreign"
	case *machine.CaseLambdaClosure:
		typeName = "case-lambda"
	case *machine.Parameter:
		typeName = "parameter"
	case *machine.ComposableContinuation, *machine.CapturedContinuation:
		// Both continuation flavours classify alike, matching
		// PrimProcedureArity: call/cc yields a CapturedContinuation,
		// call-with-composable-continuation a ComposableContinuation.
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
	if dc, ok := callable.(machine.NamedCallable); ok {
		mc.SetValue(values.StringOrFalse(dc.Doc()))
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// registryFromContext extracts the *registry.PrimitiveRegistry from the MachineContext's
// namespace. Returns nil if unavailable.
func registryFromContext(mc machine.CallContext) *registry.PrimitiveRegistry {
	ns := mc.EnvironmentFrame().Namespace()
	if ns == nil {
		return nil
	}
	regAny := ns.Registry()
	if regAny == nil {
		return nil
	}
	reg, ok := regAny.(*registry.PrimitiveRegistry)
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
	slices.Sort(cats)

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
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "doc-topic")
	if err != nil {
		return err
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
	slices.Sort(names)

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
// Returns a sorted list of symbols whose name, doc, category, or keywords
// contain the pattern as a case-insensitive substring. Searches all documentation
// sources: primitives, binding specs, doc entries, environment bindings,
// loaded libraries, and unloaded library exports.
func PrimApropos(mc machine.CallContext) error {
	s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "apropos")
	if err != nil {
		return err
	}

	reg := registryFromContext(mc)
	if reg == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}

	env := mc.EnvironmentFrame()

	var libs registry.LibrarySearcher
	lr, ok := env.LibraryRegistry().(*compilation.LibraryRegistry)
	if ok && lr != nil {
		libs = libraryRegistrySearcher{reg: lr}
	}
	var exports registry.LibraryExportSearcher
	exportIndex := ensureExportIndex(mc)
	if exportIndex != nil {
		exports = libraryExportIndexSearcher{idx: exportIndex}
	}

	results := registry.SearchDoc(reg, env, libs, exports, s.Value)
	syms := make([]values.Value, len(results))
	for i, r := range results {
		syms[i] = values.NewSymbol(r.Name)
	}
	mc.SetValue(values.List(syms...))
	return nil
}

// libraryRegistrySearcher adapts *compilation.LibraryRegistry to
// registry.LibrarySearcher. The adapter lives here, not in the registry
// package, because registry/ must not import machine/compilation (Finding 7
// of plans/2026-05-18-registry-structural-reduction.md). registry/core
// already imports both packages, so the conversion belongs here.
type libraryRegistrySearcher struct {
	reg *compilation.LibraryRegistry
}

// AllLibraries flattens the loaded libraries into registry.LibraryDoc values.
func (p libraryRegistrySearcher) AllLibraries() []registry.LibraryDoc {
	libs := p.reg.All()
	q := make([]registry.LibraryDoc, len(libs))
	for i, lib := range libs {
		q[i] = registry.LibraryDoc{
			Name:        lib.Name.SchemeString(),
			Description: lib.Description,
		}
	}
	return q
}

// libraryExportIndexSearcher adapts *compilation.LibraryExportIndex to
// registry.LibraryExportSearcher.
type libraryExportIndexSearcher struct {
	idx *compilation.LibraryExportIndex
}

// AllLibraryExports flattens the indexed library summaries into
// registry.LibraryExportDoc values.
func (p libraryExportIndexSearcher) AllLibraryExports() []registry.LibraryExportDoc {
	entries := p.idx.Entries()
	q := make([]registry.LibraryExportDoc, len(entries))
	for i, e := range entries {
		q[i] = registry.LibraryExportDoc{
			Name:        e.Name.SchemeString(),
			Description: e.Description,
			Exports:     e.Exports,
		}
	}
	return q
}

// ensureExportIndex returns the cached library export index from the
// namespace, lazily building it on first call. Returns a partial index
// when some .sld files are malformed (matching Engine.ensureExportIndex
// semantics). Returns nil only on transient errors or missing resolver.
// Non-transient failures store their result (even nil) to stop retrying.
func ensureExportIndex(mc machine.CallContext) *compilation.LibraryExportIndex {
	ns := mc.EnvironmentFrame().Namespace()
	if ns == nil {
		return nil
	}
	cached, built := ns.ExportIndex()
	if built {
		idx, _ := cached.(*compilation.LibraryExportIndex)
		return idx
	}
	// Index not yet built — build it now.
	resolver := ns.FileResolver()
	if resolver == nil {
		return nil
	}
	regAny := ns.LibraryRegistry()
	if regAny == nil {
		return nil
	}
	lr, ok := regAny.(*compilation.LibraryRegistry)
	if !ok {
		return nil
	}
	result, err := compilation.BuildExportIndex(mc.Context(), resolver, lr)
	if err != nil {
		if errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded) {
			return nil // transient — retry next call
		}
		// Non-transient errors (malformed .sld files, etc.):
		// store whatever partial index was returned and stop retrying.
	}
	// Re-check: another goroutine may have stored an index while we
	// were building. Prefer the existing one to avoid redundant writes.
	cached, built = ns.ExportIndex()
	if built {
		idx, _ := cached.(*compilation.LibraryExportIndex)
		return idx
	}
	ns.SetExportIndex(result)
	return result
}

// PrimRegisteredPrimitives implements (registered-primitives).
// Returns a list of opaque primitive-spec values wrapping the current
// namespace's registry entries. Each value can be queried via the
// primitive-spec-* accessors below.
func PrimRegisteredPrimitives(mc machine.CallContext) error {
	reg := registryFromContext(mc)
	if reg == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}
	prims := reg.Primitives()
	result := make([]values.Value, len(prims))
	for i := range prims {
		// Copy the registration into a heap-allocated value so the
		// OpaqueValue payload does not alias reg.Primitives()'s backing
		// array. Identity is per-call: two calls yield non-eq? specs for
		// the same index.
		r := prims[i]
		result[i] = values.NewOpaqueValue(primitiveSpecTag, &r)
	}
	mc.SetValue(values.List(result...))
	return nil
}

// PrimPrimitiveSpecQ implements (primitive-spec? v).
func PrimPrimitiveSpecQ(mc machine.CallContext) error {
	o, ok := mc.Arg(0).(*values.OpaqueValue)
	mc.SetValue(values.BoolToBoolean(ok && o.OpaqueTag() == primitiveSpecTag))
	return nil
}

// PrimPrimitiveSpecName implements (primitive-spec-name spec).
func PrimPrimitiveSpecName(mc machine.CallContext) error {
	reg, err := unwrapPrimitiveSpec(mc.Arg(0), "primitive-spec-name")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewString(reg.Spec.Name))
	return nil
}

// PrimPrimitiveSpecReturnType implements (primitive-spec-return-type spec).
// Returns the declared ReturnType name, or the empty string if unset.
func PrimPrimitiveSpecReturnType(mc machine.CallContext) error {
	reg, err := unwrapPrimitiveSpec(mc.Arg(0), "primitive-spec-return-type")
	if err != nil {
		return err
	}
	name := ""
	if reg.Spec.ReturnType != nil {
		name = reg.Spec.ReturnType.Name()
	}
	mc.SetValue(values.NewString(name))
	return nil
}

// PrimPrimitiveSpecParamCount implements (primitive-spec-param-count spec).
func PrimPrimitiveSpecParamCount(mc machine.CallContext) error {
	reg, err := unwrapPrimitiveSpec(mc.Arg(0), "primitive-spec-param-count")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewInteger(int64(reg.Spec.ParamCount)))
	return nil
}

// PrimPrimitiveSpecVariadicQ implements (primitive-spec-variadic? spec).
func PrimPrimitiveSpecVariadicQ(mc machine.CallContext) error {
	reg, err := unwrapPrimitiveSpec(mc.Arg(0), "primitive-spec-variadic?")
	if err != nil {
		return err
	}
	mc.SetValue(values.BoolToBoolean(reg.Spec.IsVariadic))
	return nil
}

// PrimPrimitiveSpecGoFunction implements (primitive-spec-go-function spec).
// Returns the fully-qualified Go function name, or the empty string for
// binding-only primitives (nil Impl).
func PrimPrimitiveSpecGoFunction(mc machine.CallContext) error {
	reg, err := unwrapPrimitiveSpec(mc.Arg(0), "primitive-spec-go-function")
	if err != nil {
		return err
	}
	goFunc, _ := resolveImplLocation(reg.Spec.Impl)
	mc.SetValue(values.NewString(goFunc))
	return nil
}

// PrimPrimitiveSpecGoSource implements (primitive-spec-go-source spec).
// Returns "file:line" (absolute path), or the empty string for
// binding-only primitives. Callers wanting a repo-relative path should
// strip the known workspace prefix themselves.
func PrimPrimitiveSpecGoSource(mc machine.CallContext) error {
	reg, err := unwrapPrimitiveSpec(mc.Arg(0), "primitive-spec-go-source")
	if err != nil {
		return err
	}
	_, goSource := resolveImplLocation(reg.Spec.Impl)
	mc.SetValue(values.NewString(goSource))
	return nil
}

// PrimPrimitiveSpecCategory implements (primitive-spec-category spec).
func PrimPrimitiveSpecCategory(mc machine.CallContext) error {
	reg, err := unwrapPrimitiveSpec(mc.Arg(0), "primitive-spec-category")
	if err != nil {
		return err
	}
	mc.SetValue(values.NewString(reg.Spec.Category))
	return nil
}
