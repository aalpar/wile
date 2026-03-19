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

	qt "github.com/frankban/quicktest"
)

func TestCompileTimeCallContext(t *testing.T) {
	tcs := []struct {
		name    string
		checkFn func(t *testing.T)
	}{
		{
			name: "NewCompileTimeCallContext inTail true",
			checkFn: func(t *testing.T) {
				ctx := context.Background()
				ctctx := NewCompileTimeCallContext(ctx, true)
				qt.Assert(t, ctctx.inTail, qt.IsTrue)
				qt.Assert(t, ctctx.Context(), qt.Equals, ctx)
			},
		},
		{
			name: "NewCompileTimeCallContext inTail false",
			checkFn: func(t *testing.T) {
				ctx := context.Background()
				ctctx := NewCompileTimeCallContext(ctx, false)
				qt.Assert(t, ctctx.inTail, qt.IsFalse)
				qt.Assert(t, ctctx.Context(), qt.Equals, ctx)
			},
		},
		{
			name: "NotInTail returns copy with inTail false",
			checkFn: func(t *testing.T) {
				ctx := context.Background()
				ctctx := NewCompileTimeCallContext(ctx, true)
				notTail := ctctx.NotInTail()
				qt.Assert(t, notTail.inTail, qt.IsFalse)
				// Original unchanged (value semantics)
				qt.Assert(t, ctctx.inTail, qt.IsTrue)
			},
		},
		{
			name: "NotInTail preserves context",
			checkFn: func(t *testing.T) {
				type ctxKey struct{}
				ctx := context.WithValue(context.Background(), ctxKey{}, "val")
				ctctx := NewCompileTimeCallContext(ctx, true)
				notTail := ctctx.NotInTail()
				qt.Assert(t, notTail.Context(), qt.Equals, ctx)
			},
		},
		{
			name: "NotInTail on already non-tail is idempotent",
			checkFn: func(t *testing.T) {
				ctx := context.Background()
				ctctx := NewCompileTimeCallContext(ctx, false)
				notTail := ctctx.NotInTail()
				qt.Assert(t, notTail.inTail, qt.IsFalse)
			},
		},
		{
			name: "value semantics: modifications do not propagate",
			checkFn: func(t *testing.T) {
				ctx := context.Background()
				a := NewCompileTimeCallContext(ctx, true)
				b := a.NotInTail()
				qt.Assert(t, a.inTail, qt.IsTrue)
				qt.Assert(t, b.inTail, qt.IsFalse)
			},
		},
		{
			name: "zero value has false inTail and nil context",
			checkFn: func(t *testing.T) {
				var ctctx CompileTimeCallContext
				qt.Assert(t, ctctx.inTail, qt.IsFalse)
				qt.Assert(t, ctctx.Context(), qt.IsNil)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t)
		})
	}
}
