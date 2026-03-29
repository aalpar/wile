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
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimEnvironmentQ implements the (environment?) predicate.
// Returns #t if the argument is an environment, #f otherwise.
var PrimEnvironmentQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*environment.Namespace)
	return ok
})

// PrimInteractionEnvironment implements the (interaction-environment) primitive.
// Returns the REPL environment (the current top-level environment).
func PrimInteractionEnvironment(mc *machine.MachineContext) error {
	topLevel := mc.EnvironmentFrame().Namespace()
	if topLevel.Name == "" {
		topLevel.Name = "interaction-environment"
	}
	mc.SetValue(topLevel)
	return nil
}

// PrimEnvironmentBoundNames implements the (environment-bound-names) primitive.
// Returns a list of all symbols bound in the given environment.
// (environment-bound-names env) -> list
func PrimEnvironmentBoundNames(mc *machine.MachineContext) error {
	envVal := mc.Arg(0)

	topLevelEnv, err := helpers.RequireType[*environment.Namespace](envVal, werr.ErrInvalidArgument, "environment-bound-names")
	if err != nil {
		return err
	}

	env := topLevelEnv.Runtime()
	keys := env.GlobalEnvironment().Keys()
	var result values.Value = values.EmptyList
	for key := range keys {
		sym := values.NewSymbol(key.Key)
		result = values.NewCons(sym, result)
	}

	mc.SetValue(result)
	return nil
}

// PrimEnvironmentRef implements the (environment-ref) primitive.
// Returns the value bound to a symbol in the given environment.
// Signals an error if the symbol is unbound.
// (environment-ref env symbol) -> value
func PrimEnvironmentRef(mc *machine.MachineContext) error {
	envVal := mc.Arg(0)
	symVal := mc.Arg(1)

	topLevelEnv, err := helpers.RequireType[*environment.Namespace](envVal, werr.ErrInvalidArgument, "environment-ref")
	if err != nil {
		return err
	}

	sym, err := helpers.RequireType[*values.Symbol](symVal, werr.ErrNotASymbol, "environment-ref")
	if err != nil {
		return err
	}

	env := topLevelEnv.Runtime()
	binding := env.GetBinding(sym)
	if binding == nil {
		return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "environment-ref: unbound symbol %s", sym.Key)
	}

	mc.SetValue(binding.Value())
	return nil
}

// PrimEnvironmentBoundQ implements the (environment-bound?) primitive.
// Returns #t if the symbol is bound in the given environment, #f otherwise.
// (environment-bound? env symbol) -> boolean
func PrimEnvironmentBoundQ(mc *machine.MachineContext) error {
	envVal := mc.Arg(0)
	symVal := mc.Arg(1)

	topLevelEnv, err := helpers.RequireType[*environment.Namespace](envVal, werr.ErrInvalidArgument, "environment-bound?")
	if err != nil {
		return err
	}

	sym, err := helpers.RequireType[*values.Symbol](symVal, werr.ErrNotASymbol, "environment-bound?")
	if err != nil {
		return err
	}

	env := topLevelEnv.Runtime()
	binding := env.GetBinding(sym)
	mc.SetValue(values.BoolToBoolean(binding != nil))
	return nil
}

// PrimFeatures implements the (features) primitive.
// Returns list of implementation features.
func PrimFeatures(mc *machine.MachineContext) error {
	features := machine.AllFeatures()

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
func PrimAvailableLibraries(mc *machine.MachineContext) error {
	env := mc.EnvironmentFrame()

	regAny := env.LibraryRegistry()
	if regAny == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}
	reg, ok := regAny.(*machine.LibraryRegistry)
	if !ok {
		mc.SetValue(values.EmptyList)
		return nil
	}

	resolverAny := env.FileResolver()
	resolver, _ := resolverAny.(machine.FileResolver)

	libs, err := machine.DiscoverAvailableLibraries(resolver, reg)
	if err != nil {
		return werr.WrapForeignErrorf(
			werr.ErrLibraryConfiguration,
			"available-libraries: %s", err,
		)
	}

	elems := make([]values.Value, len(libs))
	for i, lib := range libs {
		elems[i] = lib.ToSchemeValue()
	}
	mc.SetValue(values.List(elems...))
	return nil
}
