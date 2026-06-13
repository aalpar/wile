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
)

// TestOpReleaseEnvFrame_ReleasesPooledFrame pins the runtime: a pool-owned current
// frame is returned to the FreeList (so the next acquire reuses it) and envPooled
// is cleared to prevent a double release.
func TestOpReleaseEnvFrame_ReleasesPooledFrame(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpReleaseEnvFrame})

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)

	frame := mc.acquireEnvFrame()
	mc.env = frame
	mc.envPooled = true
	mc.pc = 0

	err := mc.Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if mc.envPooled {
		t.Errorf("envPooled must be cleared after OpReleaseEnvFrame")
	}
	got := mc.acquireEnvFrame()
	if got != frame {
		t.Errorf("released frame must be reused by the next acquire (pool round-trip)")
	}
}

// TestOpReleaseEnvFrame_SkipsUnpooledFrame pins the guard: a frame that is not
// pool-owned (a parentless thunk or a continuation-shared frame, envPooled=false)
// must NOT be released — releasing it would corrupt a frame the continuation may
// still reach.
func TestOpReleaseEnvFrame_SkipsUnpooledFrame(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpReleaseEnvFrame})

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)

	frame := environment.NewEnvironmentFrameWithParent(environment.NewLocalEnvironment(1), env)
	mc.env = frame
	mc.envPooled = false
	mc.pc = 0

	err := mc.Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}
	if mc.env != frame {
		t.Errorf("a non-pool-owned frame must be left intact")
	}
}
