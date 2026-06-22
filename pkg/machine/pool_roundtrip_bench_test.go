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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// These benchmarks isolate the env-frame pool round-trip that fires on EVERY
// non-tail closure and foreign call: acquireEnvFrame() on entry (machine_context_apply.go),
// releaseEnvFrame() via RestoreAndRelease() on normal return. They quantify the
// part a hypothetical "local call frame elide" (never allocate a separate frame)
// could remove, versus the arg-binding setup that elision would still pay.
//
// newBenchMC roots a per-thread pool (AcquireTopLevelContext), so these drive the
// lock-free unsyncFreeList path, not the synchronized global FreeList:
// Pool round-trip = acquireEnvFrame -> unsyncFreeList.Acquire (slice pop) +
//                   releaseEnvFrame -> unsyncFreeList.Release (ResetForPool of a
//                                      cap-4 frame + slice append).

// BenchmarkEnvFramePoolRoundTrip measures the PURE pool overhead: one
// acquire + release on the steady-state hit path. This is the elidable cost
// per non-tail call.
func BenchmarkEnvFramePoolRoundTrip(b *testing.B) {
	mc := newBenchMC(newBenchEnv())
	// Warm the freelist so we measure the hit path, not the first allocating miss.
	warm := mc.acquireEnvFrame()
	mc.releaseEnvFrame(warm)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		f := mc.acquireEnvFrame()
		mc.releaseEnvFrame(f)
	}
}

// BenchmarkEnvFrameCallLifecycle adds the per-call frame setup the VM performs
// between acquire and release: InitApplyFrame (copy parent links + closure
// locals) + bindArgs (write the actual arguments). Subtracting the pure
// round-trip above isolates the non-elidable setup (an elided call must still
// place its arguments somewhere).
func BenchmarkEnvFrameCallLifecycle(b *testing.B) {
	top := newBenchEnv()
	// A 2-parameter closure source frame (parent = top-level), mirroring a
	// user helper like (lambda (a b) ...).
	local := environment.NewLocalEnvironment(2)
	src := environment.NewEnvironmentFrameWithParent(local, top)
	mc := newBenchMC(top)
	args := []values.Value{values.NewInteger(1), values.NewInteger(2)}

	// Warm the freelist.
	warm := mc.acquireEnvFrame()
	mc.releaseEnvFrame(warm)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		dst := mc.acquireEnvFrame()
		src.InitApplyFrame(dst)
		bindArgs(dst.LocalBindingsSlice()[:2], args, 2, false, nil)
		mc.releaseEnvFrame(dst)
	}
}
