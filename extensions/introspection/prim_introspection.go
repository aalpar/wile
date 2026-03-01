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
	_, ok := o.(*environment.TopLevelEnvironment)
	return ok
})

// PrimInteractionEnvironment implements the (interaction-environment) primitive.
// Returns the REPL environment (the current top-level environment).
func PrimInteractionEnvironment(mc *machine.MachineContext) error {
	topLevel := mc.EnvironmentFrame().TopLevelEnv()
	topLevel.Name = "interaction-environment"
	mc.SetValue(topLevel)
	return nil
}

// PrimEnvironmentBoundNames implements the (environment-bound-names) primitive.
// Returns a list of all symbols bound in the given environment.
// (environment-bound-names env) -> list
func PrimEnvironmentBoundNames(mc *machine.MachineContext) error {
	envVal := mc.Arg(0)

	topLevelEnv, ok := envVal.(*environment.TopLevelEnvironment)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "environment-bound-names: expected an environment but got %T", envVal)
	}

	env := topLevelEnv.Runtime()
	keys := env.GlobalEnvironment().Keys()
	var result values.Value = values.EmptyList
	for key := range keys {
		interned := env.InternSymbol(&key)
		result = values.NewCons(interned, result)
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

	topLevelEnv, err := helpers.RequireType[*environment.TopLevelEnvironment](envVal, werr.ErrInvalidArgument, "environment-ref")
	if err != nil {
		return err
	}

	sym, err := helpers.RequireType[*values.Symbol](symVal, werr.ErrNotASymbol, "environment-ref")
	if err != nil {
		return err
	}

	env := topLevelEnv.Runtime()
	sym = env.InternSymbol(sym)
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

	topLevelEnv, err := helpers.RequireType[*environment.TopLevelEnvironment](envVal, werr.ErrInvalidArgument, "environment-bound?")
	if err != nil {
		return err
	}

	sym, err := helpers.RequireType[*values.Symbol](symVal, werr.ErrNotASymbol, "environment-bound?")
	if err != nil {
		return err
	}

	env := topLevelEnv.Runtime()
	sym = env.InternSymbol(sym)
	binding := env.GetBinding(sym)
	mc.SetValue(values.BoolToBoolean(binding != nil))
	return nil
}
