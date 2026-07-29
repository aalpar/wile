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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// applyForeign unconditionally acquires an env frame (machine_context_apply.go)
// to keep concurrent SRFI-18 threads off shared binding slots. Every acquire owes
// a release. The frame comes back through RestoreAndRelease, which runs only when
// the context has a continuation to return through:
//
//	if p.cont == savedCont {
//	    if p.cont != nil {
//	        p.RestoreAndRelease(p.cont)   // releases the acquired frame
//	    } else {
//	        p.template = immediateReturnTemplate
//	        p.pc = 0                      // frame is dropped on the floor
//	    }
//	}
//
// The rootless arm has no release, so a foreign call on a context with cont == nil
// leaks its frame to GC. That shape is reachable: NewSubContext produces contexts
// with a nil cont, and callForeignCached's own comment (call_foreign_cached.go)
// names applyForeign as the path that "can be called from sub-contexts where cont
// is nil".
//
// The cost is not a correctness bug — a leaked frame is merely collected instead
// of recycled — but it empties the env-frame freelist. Once empty, every acquire
// misses and allocates, which is 93% of allocations in a dynamic-wind-heavy
// parallel kernel and the dominant driver of mheap.lock contention under
// SRFI-18 thread scaling.
//
// The nil-cont case FAILS until the rootless arm releases the frame. The
// with-cont case passes today and is the control: it proves the assertions
// discriminate rather than reporting an unbalanced pool for some unrelated reason.
func TestApplyForeign_ReleasesAcquiredEnvFrame(t *testing.T) {
	tests := []struct {
		name    string
		withCon bool
	}{
		{name: "rootless context (cont == nil)", withCon: false},
		{name: "rooted context (cont != nil)", withCon: true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			fcls := NewForeignClosure(
				&environment.EnvironmentFrame{},
				1,
				false,
				func(cc CallContext) error {
					cc.SetValue(values.NewInteger(42))
					return nil
				},
			)

			// mc.pools stays nil, so acquire/release route through the global
			// envFramePool and its stats observe the imbalance directly.
			mc := &MachineContext{}
			mc.evals = acquireStack()

			if tt.withCon {
				cont := acquireContinuation()
				cont.evals = acquireStack()
				cont.template = &NativeTemplate{}
				// Distinct from the frame applyForeign will acquire: RestoreAndRelease
				// releases the old env only when it differs from the restored one.
				cont.env = &environment.EnvironmentFrame{}
				mc.cont = cont
			}

			before := envFramePool.Stats()

			_, err := mc.applyForeign(fcls, values.NewInteger(0))
			qt.Assert(t, err, qt.IsNil)

			after := envFramePool.Stats()

			qt.Assert(t, mc.counters.EnvsCopied, qt.Equals, uint64(1))
			qt.Assert(t, after.Acquires-before.Acquires, qt.Equals, uint64(1))

			// The claim under test: the acquired frame is returned to the pool.
			qt.Assert(t, after.Releases-before.Releases, qt.Equals, uint64(1))
			qt.Assert(t, mc.counters.EnvFramePoolReleases, qt.Equals, uint64(1))
		})
	}
}
