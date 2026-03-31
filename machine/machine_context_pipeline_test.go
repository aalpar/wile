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
	"context"
	"testing"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/testutil"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestExecuteSimpleProcedureCall(t *testing.T) {
	env := testutil.NewFullRuntimeEnv(t)

	sv := testutil.ParseSchemeExpr(t, env, `((lambda (x) x) 42)`)
	cont, err := testutil.NewTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestExecuteVariadicProcedure(t *testing.T) {
	env := testutil.NewFullRuntimeEnv(t)

	sv := testutil.ParseSchemeExpr(t, env, `((lambda args args) 1 2 3)`)
	cont, err := testutil.NewTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValues(), qt.IsNotNil)
}

func TestMachineContextNewSubContext_Pipeline(t *testing.T) {
	env := testutil.NewFullRuntimeEnv(t)

	sv := testutil.ParseSchemeExpr(t, env, `42`)
	cont, err := testutil.NewTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(context.Background(), cont)
	sub := mc.NewSubContext()
	qt.Assert(t, sub, qt.IsNotNil)
}

func TestMachineContextApplySimple_Pipeline(t *testing.T) {
	env := testutil.NewFullRuntimeEnv(t)

	sv := testutil.ParseSchemeExpr(t, env, `((lambda (x) x) 100)`)
	cont, err := testutil.NewTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(100))
}
