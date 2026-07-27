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

package compilation

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// PhaseEntry represents a named item to register in a phase environment.
type PhaseEntry[F any] struct {
	Name string
	Fn   F
}

// RegisterPhaseBindings binds all entries in the target phase environment.
// This is a generic helper for registering primitives in expand or compile phases.
//
// Parameters:
//   - phaseEnv: Accessor for the target phase (e.g., env.Expand or env.Compile).
//     This alone determines the target; env is not consulted.
//   - entries: Slice of (name, function) pairs to register
//   - wrapper: Creates the values.Value wrapper from name and function
func RegisterPhaseBindings[F any](
	env *environment.EnvironmentFrame,
	phaseEnv func() *environment.EnvironmentFrame,
	entries []PhaseEntry[F],
	wrapper func(name string, fn F) values.Value,
) error {
	targetEnv := phaseEnv()
	for _, entry := range entries {
		sym := values.NewSymbol(entry.Name)
		val := wrapper(entry.Name, entry.Fn)
		idx, _ := targetEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypePrimitive, nil)
		targetEnv.SetOwnGlobalValue(idx, val) //nolint:errcheck
	}
	return nil
}

// LookupPhaseBinding looks up a binding by symbol in the target phase environment.
// Returns the value cast to type T if found, or the zero value if not found
// or if the value is not of type T.
//
// This function handles hygiene by using scoped lookup - it will only match
// bindings whose scopes are a subset of the symbol's scopes.
func LookupPhaseBinding[T any](
	phaseEnv *environment.EnvironmentFrame,
	sym *values.Symbol,
	scopes []*syntax.Scope,
) T {
	var zero T
	bnd := phaseEnv.GetBinding(sym, syntax.ScopesOf(scopes))
	if bnd == nil {
		return zero
	}
	if bnd.BindingType() != environment.BindingTypePrimitive {
		return zero
	}
	val, ok := bnd.Value().(T)
	if ok {
		return val
	}
	return zero
}
