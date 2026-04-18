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

package machine

import (
	"context"
	"testing"

	"github.com/aalpar/wile/environment"
	qt "github.com/frankban/quicktest"
)

func newCoverageTestEnv() *environment.EnvironmentFrame {
	return environment.NewNamespace().Runtime()
}

func TestRun_CoverageOff_NoEffect(t *testing.T) {
	c := qt.New(t)
	env := newCoverageTestEnv()
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	err := mc.Run()
	ReleaseTopLevelContext(mc)

	c.Assert(err, qt.IsNil)
	c.Assert(tpl.Executed(), qt.IsNil, qt.Commentf("coverage should remain disabled"))
}

func TestRun_CoverageOn_MarksExecutedPCs(t *testing.T) {
	c := qt.New(t)
	env := newCoverageTestEnv()
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})
	tpl.EnableCoverage()

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	err := mc.Run()
	ReleaseTopLevelContext(mc)

	c.Assert(err, qt.IsNil)
	exec := tpl.Executed()
	c.Assert(exec, qt.HasLen, 2)
	c.Assert(exec[0], qt.IsTrue, qt.Commentf("first OpLoadVoid should be marked"))
	c.Assert(exec[1], qt.IsTrue, qt.Commentf("second OpLoadVoid should be marked"))
}

func TestRun_CoverageOn_UnreachedPCsStayFalse(t *testing.T) {
	c := qt.New(t)
	env := newCoverageTestEnv()
	tpl := NewNativeTemplate(0, 0, false)
	// OpRestoreContinuation with no parent cont returns nil immediately (pc=0 halts).
	tpl.AppendInstruction(Instruction{Op: OpRestoreContinuation}) // pc=0: exits Run()
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})            // pc=1: unreached
	tpl.EnableCoverage()

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	err := mc.Run()
	ReleaseTopLevelContext(mc)

	c.Assert(err, qt.IsNil)
	exec := tpl.Executed()
	c.Assert(exec[0], qt.IsTrue, qt.Commentf("OpRestoreContinuation at pc=0 should be marked"))
	c.Assert(exec[1], qt.IsFalse, qt.Commentf("unreached pc=1 must remain false"))
}
