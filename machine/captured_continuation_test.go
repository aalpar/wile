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

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestCapturedContinuation(t *testing.T) {
	// Create a minimal composable continuation for test setup.
	cc := NewComposableContinuation(nil, nil, 0, nil)

	tcs := []struct {
		name    string
		checkFn func(t *testing.T)
	}{
		{
			name: "NewCapturedContinuation stores fields",
			checkFn: func(t *testing.T) {
				barrier := NewBarrierToken()
				capt := NewCapturedContinuation(cc, 42, barrier)
				qt.Assert(t, capt, qt.IsNotNil)
				qt.Assert(t, capt.cc, qt.Equals, cc)
				qt.Assert(t, capt.threadID, qt.Equals, uint64(42))
				qt.Assert(t, capt.barrierValid, qt.Equals, barrier)
			},
		},
		{
			name: "ComposableContinuation accessor",
			checkFn: func(t *testing.T) {
				capt := NewCapturedContinuation(cc, 0, nil)
				qt.Assert(t, capt.ComposableContinuation(), qt.Equals, cc)
			},
		},
		{
			name: "SchemeString",
			checkFn: func(t *testing.T) {
				capt := NewCapturedContinuation(cc, 0, nil)
				qt.Assert(t, capt.SchemeString(), qt.Equals, "#<continuation>")
			},
		},
		{
			name: "IsVoid false for non-nil",
			checkFn: func(t *testing.T) {
				capt := NewCapturedContinuation(cc, 0, nil)
				qt.Assert(t, capt.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "IsVoid true for nil",
			checkFn: func(t *testing.T) {
				var capt *CapturedContinuation
				qt.Assert(t, capt.IsVoid(), qt.IsTrue)
			},
		},
		{
			name: "EqualTo self is true (pointer identity)",
			checkFn: func(t *testing.T) {
				capt := NewCapturedContinuation(cc, 0, nil)
				qt.Assert(t, capt.EqualTo(capt), qt.IsTrue)
			},
		},
		{
			name: "EqualTo different instance is false",
			checkFn: func(t *testing.T) {
				a := NewCapturedContinuation(cc, 0, nil)
				b := NewCapturedContinuation(cc, 0, nil)
				qt.Assert(t, a.EqualTo(b), qt.IsFalse)
			},
		},
		{
			name: "EqualTo different type is false",
			checkFn: func(t *testing.T) {
				capt := NewCapturedContinuation(cc, 0, nil)
				qt.Assert(t, capt.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "AcceptsArity 1 is true",
			checkFn: func(t *testing.T) {
				capt := NewCapturedContinuation(cc, 0, nil)
				qt.Assert(t, capt.AcceptsArity(1), qt.IsTrue)
			},
		},
		{
			name: "AcceptsArity 0 is false",
			checkFn: func(t *testing.T) {
				capt := NewCapturedContinuation(cc, 0, nil)
				qt.Assert(t, capt.AcceptsArity(0), qt.IsFalse)
			},
		},
		{
			name: "AcceptsArity 2 is false",
			checkFn: func(t *testing.T) {
				capt := NewCapturedContinuation(cc, 0, nil)
				qt.Assert(t, capt.AcceptsArity(2), qt.IsFalse)
			},
		},
		{
			name: "satisfies values.Callable interface",
			checkFn: func(t *testing.T) {
				capt := NewCapturedContinuation(cc, 0, nil)
				var callable values.Callable = capt
				qt.Assert(t, callable, qt.IsNotNil)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t)
		})
	}
}
