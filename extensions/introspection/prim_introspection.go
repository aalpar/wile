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

package introspection

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimEnvironmentQ implements the (environment?) predicate.
// Returns #t if the argument is an environment, #f otherwise.
var PrimEnvironmentQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*environment.Namespace)
	return ok
})

// PrimInteractionEnvironment implements the (interaction-environment) primitive.
// Returns the REPL environment (the current top-level namespace).
//
// This is a pure read: it must not mutate the returned namespace, which is shared
// across the engine's SRFI-18 threads. The engine names its top level
// "interaction-environment" eagerly at init (engine.go), so the descriptive name
// is already in place; labeling it lazily here raced when concurrent threads each
// observed an empty Name and wrote it.
func PrimInteractionEnvironment(mc machine.CallContext) error {
	mc.SetValue(mc.EnvironmentFrame().Namespace())
	return nil
}

// PrimEnvironmentBoundNames implements the (environment-bound-names) primitive.
// Returns a list of all symbols bound in the given environment.
// (environment-bound-names env) -> list
func PrimEnvironmentBoundNames(mc machine.CallContext) error {
	envVal := mc.Arg(0)

	topLevelEnv, err := helpers.RequireType[*environment.Namespace](envVal, werr.ErrNotANamespace, "environment-bound-names")
	if err != nil {
		return err
	}

	mc.SetValue(topLevelEnv.BoundSymbolNames())
	return nil
}

// PrimEnvironmentRef implements the (environment-ref) primitive.
// Returns the value bound to a symbol in the given environment.
// Signals an error if the symbol is unbound.
// (environment-ref env symbol) -> value
//
// Reads under AmbientScopes, not nil. The symbol argument is a runtime
// values.Symbol carrying no scope set, so when several hygiene-distinct bindings
// share a name a wildcard read resolves by slot order: two macros each
// introducing w make this return the one that expanded first, which is an
// expansion-order artifact rather than an answer to the caller's question. It
// also hands back a macro's private variable, which no source-written reference
// in the namespace could name. environment-bound-names lists ambient names only,
// so a wildcard here would also make the listing disagree with the read.
func PrimEnvironmentRef(mc machine.CallContext) error {
	envVal := mc.Arg(0)
	symVal := mc.Arg(1)

	topLevelEnv, err := helpers.RequireType[*environment.Namespace](envVal, werr.ErrNotANamespace, "environment-ref")
	if err != nil {
		return err
	}

	sym, err := helpers.RequireType[*values.Symbol](symVal, werr.ErrNotASymbol, "environment-ref")
	if err != nil {
		return err
	}

	env := topLevelEnv.Runtime()
	binding := env.GetBinding(sym, values.EmptyScopes())
	if binding == nil {
		return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "environment-ref: unbound symbol %s", sym.Key)
	}

	mc.SetValue(binding.Value())
	return nil
}

// PrimEnvironmentBoundQ implements the (environment-bound?) primitive.
// Returns #t if the symbol is bound in the given environment, #f otherwise.
// (environment-bound? env symbol) -> boolean
//
// Shares environment-ref's lookup line so bound? and ref cannot disagree: a name
// this reports as bound must be one ref can retrieve.
func PrimEnvironmentBoundQ(mc machine.CallContext) error {
	envVal := mc.Arg(0)
	symVal := mc.Arg(1)

	topLevelEnv, err := helpers.RequireType[*environment.Namespace](envVal, werr.ErrNotANamespace, "environment-bound?")
	if err != nil {
		return err
	}

	sym, err := helpers.RequireType[*values.Symbol](symVal, werr.ErrNotASymbol, "environment-bound?")
	if err != nil {
		return err
	}

	env := topLevelEnv.Runtime()
	binding := env.GetBinding(sym, values.EmptyScopes())
	mc.SetValue(values.BoolToBoolean(binding != nil))
	return nil
}

// PrimFeatures implements the (features) primitive.
// Returns list of implementation features.
func PrimFeatures(mc machine.CallContext) error {
	features := compilation.AllFeatures()

	elems := make([]values.Value, len(features))
	for i, f := range features {
		elems[i] = values.NewSymbol(f)
	}

	mc.SetValue(values.List(elems...))
	return nil
}

// PrimAvailableLibraries implements the (available-libraries) primitive.
// Returns a sorted list of all importable library names.
// Each library name is a list of symbols/integers matching R7RS syntax.
func PrimAvailableLibraries(mc machine.CallContext) error {
	env := mc.EnvironmentFrame()

	regSearcher := env.LibraryRegistry()
	if regSearcher == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}
	reg, ok := regSearcher.(*compilation.LibraryRegistry)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrLibraryConfiguration,
			"available-libraries: library registry has unexpected type %T", regSearcher)
	}

	libs, err := compilation.DiscoverAvailableLibraries(env.FileResolver(), reg)
	if err != nil {
		return werr.WrapForeignErrorWithCause(
			werr.ErrLibraryConfiguration, err,
			"available-libraries",
		)
	}

	elems := make([]values.Value, len(libs))
	for i, lib := range libs {
		elems[i] = lib.ToSchemeValue()
	}
	mc.SetValue(values.List(elems...))
	return nil
}
