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

	"github.com/aalpar/wile/pkg/environment"

	qt "github.com/frankban/quicktest"
)

// TestReleaseClosesTheBoundary pins the token's lifecycle at the pool: a
// continuation captured at a sub-context's boundary keeps the token after the
// context is released, and the release closes it, which is what marks the
// continuation stale (resumePrompt). The Scheme-level guard in
// extensions/eval relies on pool reuse, which is not guaranteed; this is the
// direct pin.
func TestReleaseClosesTheBoundary(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	root := AcquireTopLevelContext(context.Background(), NewEmptyNativeTemplate(), env)
	defer ReleaseTopLevelContext(root)
	sub := root.NewSubContext()

	comp := NewComposableContinuation(nil, nil, 0, sub.ThreadID(), sub.BarrierValid())
	sub.DelimitAt(comp, nil)
	qt.Assert(t, comp.boundary, qt.Not(qt.IsNil))
	qt.Assert(t, comp.boundary.closed.Load(), qt.IsFalse)
	qt.Assert(t, comp.root, qt.Equals, root.boundaryIdentity())
	qt.Assert(t, comp.root.runtime, qt.IsTrue)

	ReleaseSubContext(sub)
	qt.Assert(t, comp.boundary.closed.Load(), qt.IsTrue)
	qt.Assert(t, comp.root.closed.Load(), qt.IsFalse)
}
