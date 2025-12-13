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
	"testing"

	"wile/environment"
	"wile/syntax"

	qt "github.com/frankban/quicktest"
)

func TestNewExpanderContext(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	expander := NewExpanderTimeContinuation(env)
	ectx := NewExpandTimeCallContext()

	ctx := NewExpanderContext(env, expander, ectx)

	c.Assert(ctx, qt.IsNotNil)
	c.Assert(ctx.Env(), qt.Equals, env)
}

func TestExpanderContext_IntroductionScope(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	expander := NewExpanderTimeContinuation(env)
	ectx := NewExpandTimeCallContext()

	ctx := NewExpanderContext(env, expander, ectx)

	// Initially nil
	c.Assert(ctx.IntroductionScope(), qt.IsNil)

	// Set scope
	scope := syntax.NewScope(nil)
	ctx.SetIntroductionScope(scope)
	c.Assert(ctx.IntroductionScope(), qt.Equals, scope)
}

func TestExpanderContext_UseSiteScope(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	expander := NewExpanderTimeContinuation(env)
	ectx := NewExpandTimeCallContext()

	ctx := NewExpanderContext(env, expander, ectx)

	// Initially nil
	c.Assert(ctx.UseSiteScope(), qt.IsNil)

	// Set scope
	scope := syntax.NewScope(nil)
	ctx.SetUseSiteScope(scope)
	c.Assert(ctx.UseSiteScope(), qt.Equals, scope)
}
