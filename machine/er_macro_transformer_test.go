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

package machine_test

import (
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestERMacroTransformer_IsValue(t *testing.T) {
	c := qt.New(t)

	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := machine.NewNativeTemplate(3, 0, false)
	cls := machine.NewClosureWithTemplate(tpl, env)

	ert := machine.NewERMacroTransformer(cls, env)

	// Satisfies values.Value
	var v values.Value = ert
	c.Assert(v, qt.Not(qt.IsNil))
	c.Assert(ert.SchemeString(), qt.Equals, "#<er-macro-transformer>")
	c.Assert(ert.IsVoid(), qt.IsFalse)

	// Accessors
	c.Assert(ert.Closure(), qt.Equals, cls)
	c.Assert(ert.DefEnv(), qt.Equals, env)
}

func TestERMacroTransformer_EqualTo(t *testing.T) {
	c := qt.New(t)

	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := machine.NewNativeTemplate(3, 0, false)
	cls := machine.NewClosureWithTemplate(tpl, env)

	ert1 := machine.NewERMacroTransformer(cls, env)
	ert2 := machine.NewERMacroTransformer(cls, env)

	// Identity semantics
	c.Assert(ert1.EqualTo(ert1), qt.IsTrue)
	c.Assert(ert1.EqualTo(ert2), qt.IsFalse)
	c.Assert(ert1.EqualTo(values.TrueValue), qt.IsFalse)
}
