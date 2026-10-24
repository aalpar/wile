// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//	http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package validate

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
)

// An opaque subtree mentioning x under {a,b,c} ties between the binders of x
// under {a,b} and {a,c}: neither scope set is a superset of the other. Expansion
// may settle the reference on either, so both are marked mutated; marking one
// would miss a set! target.
func TestMarkOpaqueCodeTieMarksEveryTiedBinder(t *testing.T) {
	a, b, c := syntax.NewScope(), syntax.NewScope(), syntax.NewScope()
	env := environment.NewEnvironmentFrameWithParent(environment.NewLocalEnvironment(0), environment.NewNamespace().Runtime())
	x := syntax.NewSyntaxSymbol("x", nil)
	ab := withScope(withScope(x, a), b)
	ac := withScope(withScope(x, a), c)
	abc := withScope(ab, c)
	bindLocalSymbol(env, ab)
	bindLocalSymbol(env, ac)

	refAB := env.ResolveBindingRef(ab.Sym, syntax.ScopesOf(ab.Scopes()))
	refAC := env.ResolveBindingRef(ac.Sym, syntax.ScopesOf(ac.Scopes()))
	qt.Assert(t, refAB.IsLocal(), qt.IsTrue)
	qt.Assert(t, refAC.IsLocal(), qt.IsTrue)
	qt.Assert(t, refAB, qt.Not(qt.Equals), refAC)

	result := &ValidationResult{}
	markOpaqueCode(env, abc, result)

	qt.Assert(t, result.isMutated(environment.GlobalRef(abc.Key())), qt.IsTrue)
	qt.Assert(t, result.isMutated(refAB), qt.IsTrue)
	qt.Assert(t, result.isMutated(refAC), qt.IsTrue)
}
