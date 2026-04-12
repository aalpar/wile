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

package wile_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/values"
)

func TestDebugger_BreakpointCRUD(t *testing.T) {
	dbg := wile.NewDebugger()

	id := dbg.SetBreakpoint("test.scm", 10, 0)
	qt.Assert(t, id, qt.Equals, 0)

	bps := dbg.Breakpoints()
	qt.Assert(t, len(bps), qt.Equals, 1)
	qt.Assert(t, bps[0].File, qt.Equals, "test.scm")
	qt.Assert(t, bps[0].Line, qt.Equals, 10)
	qt.Assert(t, bps[0].Enabled, qt.IsTrue)

	qt.Assert(t, dbg.DisableBreakpoint(id), qt.IsTrue)
	bps = dbg.Breakpoints()
	qt.Assert(t, bps[0].Enabled, qt.IsFalse)

	qt.Assert(t, dbg.EnableBreakpoint(id), qt.IsTrue)
	bps = dbg.Breakpoints()
	qt.Assert(t, bps[0].Enabled, qt.IsTrue)

	qt.Assert(t, dbg.RemoveBreakpoint(id), qt.IsTrue)
	qt.Assert(t, len(dbg.Breakpoints()), qt.Equals, 0)

	qt.Assert(t, dbg.RemoveBreakpoint(999), qt.IsFalse)
}

func TestDebugger_OnBreak(t *testing.T) {
	dbg := wile.NewDebugger()
	var received bool
	dbg.OnBreak(func(state values.DebugState, bp *wile.BreakpointInfo) {
		received = true
	})
	qt.Assert(t, received, qt.IsFalse)
}

func TestDebugger_StepCommands(t *testing.T) {
	dbg := wile.NewDebugger()
	// These should not panic even with no active context
	dbg.StepInto()
	dbg.Continue()
	dbg.StepOver()
	dbg.StepOut()
}

func TestDebugger_CurrentState_Nil(t *testing.T) {
	dbg := wile.NewDebugger()
	qt.Assert(t, dbg.CurrentState(), qt.IsNil)
}
