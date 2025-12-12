// Copyright 2025 Aaron Alpar
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

package machine

import (
	"wile/environment"
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestMachineClosure_Copy(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	lenv := environment.NewLocalEnvironment(2)
	lenv.Bindings()[0].SetValue(values.NewInteger(42))
	lenv.Bindings()[1].SetValue(values.NewInteger(100))
	env := environment.NewEnvironmentFrame(lenv, genv)
	tpl := NewNativeTemplate(2, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	clsCopy := cls.Copy()

	// Verify copy is a different object
	qt.Assert(t, clsCopy != cls, qt.IsTrue)
	// Verify template is shared (immutable)
	qt.Assert(t, clsCopy.Template(), qt.Equals, cls.Template())
	// Verify environment is copied
	qt.Assert(t, clsCopy.env != cls.env, qt.IsTrue)
}

func TestMachineClosure_IsVoid(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	qt.Assert(t, cls.IsVoid(), qt.IsFalse)

	var nilCls *MachineClosure
	qt.Assert(t, nilCls.IsVoid(), qt.IsTrue)
}

func TestMachineClosure_SchemeString(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)

	cls := NewClosureWithTemplate(tpl, env)
	qt.Assert(t, cls.SchemeString(), qt.Equals, "#<machine-closure>")
}

func TestMachineClosure_EqualTo(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)

	cls1 := NewClosureWithTemplate(tpl, env)
	cls2 := NewClosureWithTemplate(tpl, env)
	cls3 := cls1

	// Same object
	qt.Assert(t, cls1.EqualTo(cls3), qt.IsTrue)
	// Different objects with same env and template
	qt.Assert(t, cls1.EqualTo(cls2), qt.IsTrue)

	// Different template
	tpl2 := NewNativeTemplate(1, 0, false)
	cls4 := NewClosureWithTemplate(tpl2, env)
	qt.Assert(t, cls1.EqualTo(cls4), qt.IsFalse)

	// Different environment
	env2 := environment.NewEnvironmentFrame(nil, genv)
	cls5 := NewClosureWithTemplate(tpl, env2)
	qt.Assert(t, cls1.EqualTo(cls5), qt.IsFalse)

	// Nil comparison
	var nilCls *MachineClosure
	qt.Assert(t, nilCls.EqualTo(nilCls), qt.IsTrue)
	qt.Assert(t, cls1.EqualTo(nilCls), qt.IsFalse)

	// Different type
	qt.Assert(t, cls1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}
