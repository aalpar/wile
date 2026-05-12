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

package validate

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
)

// bindLocalSymbol binds sym in env as a local variable using the canonical
// four-argument shape (symbol, BindingTypeVariable, scopes, source context).
// This is the per-iteration primitive used by let* / letrec / named-let when
// the environment evolves with each binding and a fresh child frame is built
// separately.
func bindLocalSymbol(env *environment.EnvironmentFrame, sym *syntax.SyntaxSymbol) {
	env.MaybeCreateLocalBinding(
		sym.Sym,
		environment.BindingTypeVariable,
		sym.Scopes(),
		sym.SourceContext(),
	)
}

// extendEnvWithSymbols creates a child frame and binds all symbols as
// variables. Returns env unchanged if syms is empty. This is the batch
// primitive used by lambda parameter binding and let (parallel binding) where
// all symbols enter scope at once.
func extendEnvWithSymbols(env *environment.EnvironmentFrame, syms []*syntax.SyntaxSymbol) *environment.EnvironmentFrame {
	if len(syms) == 0 {
		return env
	}
	lenv := environment.NewLocalEnvironment(0)
	childEnv := environment.NewEnvironmentFrameWithParent(lenv, env)
	for _, sym := range syms {
		bindLocalSymbol(childEnv, sym)
	}
	return childEnv
}
