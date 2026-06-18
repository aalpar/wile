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
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestNativeTemplate_CoverageDisabledByDefault(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	c.Assert(tpl.Executed(), qt.IsNil)
}

func TestNativeTemplate_EnableCoverageAllocatesParallelToCode(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpPop})
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})

	tpl.EnableCoverage()

	exec := tpl.Executed()
	c.Assert(exec, qt.HasLen, 3)
	c.Assert(exec[0], qt.IsFalse)
	c.Assert(exec[1], qt.IsFalse)
	c.Assert(exec[2], qt.IsFalse)
}

func TestNativeTemplate_AppendAfterEnableKeepsLockstep(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.EnableCoverage()
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.AppendInstruction(Instruction{Op: OpPop})

	exec := tpl.Executed()
	c.Assert(exec, qt.HasLen, 2)
	c.Assert(len(tpl.Code()), qt.Equals, 2)
}

func TestNativeTemplate_EnableCoverageIdempotent(t *testing.T) {
	c := qt.New(t)
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpPush})
	tpl.EnableCoverage()
	tpl.Executed()[0] = true

	tpl.EnableCoverage() // second call must not clobber

	c.Assert(tpl.Executed()[0], qt.IsTrue)
}
