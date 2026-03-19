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

func TestVmState(t *testing.T) {
	tcs := []struct {
		name    string
		checkFn func(t *testing.T)
	}{
		{
			name: "zero value has nil fields",
			checkFn: func(t *testing.T) {
				var s vmState
				qt.Assert(t, s.env, qt.IsNil)
				qt.Assert(t, s.template, qt.IsNil)
				qt.Assert(t, s.singleValue, qt.IsNil)
				qt.Assert(t, s.multiValues, qt.IsNil)
				qt.Assert(t, s.evals, qt.IsNil)
				qt.Assert(t, s.pc, qt.Equals, 0)
				qt.Assert(t, s.windingStack, qt.IsNil)
				qt.Assert(t, s.promptTag, qt.IsNil)
				qt.Assert(t, s.threadID, qt.Equals, uint64(0))
				qt.Assert(t, s.callDepth, qt.Equals, 0)
				qt.Assert(t, s.envPooled, qt.IsFalse)
				qt.Assert(t, s.marks, qt.IsNil)
			},
		},
		{
			name: "fields can be set directly",
			checkFn: func(t *testing.T) {
				s := vmState{
					pc:        42,
					threadID:  7,
					callDepth: 3,
					envPooled: true,
				}
				qt.Assert(t, s.pc, qt.Equals, 42)
				qt.Assert(t, s.threadID, qt.Equals, uint64(7))
				qt.Assert(t, s.callDepth, qt.Equals, 3)
				qt.Assert(t, s.envPooled, qt.IsTrue)
			},
		},
		{
			name: "split value register: singleValue and multiValues are independent",
			checkFn: func(t *testing.T) {
				s := vmState{
					singleValue: values.NewInteger(1),
				}
				qt.Assert(t, s.singleValue, qt.IsNotNil)
				qt.Assert(t, s.multiValues, qt.IsNil)

				s.multiValues = MultipleValues{values.NewInteger(2), values.NewInteger(3)}
				s.singleValue = nil
				qt.Assert(t, s.singleValue, qt.IsNil)
				qt.Assert(t, s.multiValues, qt.HasLen, 2)
			},
		},
		{
			name: "marks initially nil",
			checkFn: func(t *testing.T) {
				var s vmState
				qt.Assert(t, s.marks, qt.IsNil)
			},
		},
		{
			name: "windingStack can be set",
			checkFn: func(t *testing.T) {
				var s vmState
				f := NewDynamicWindFrame(nil, nil)
				s.windingStack = WindingStack{f}
				qt.Assert(t, s.windingStack.Depth(), qt.Equals, 1)
			},
		},
		{
			name: "promptTag can be set",
			checkFn: func(t *testing.T) {
				var s vmState
				tag := NewPromptTag("test")
				s.promptTag = tag
				qt.Assert(t, s.promptTag, qt.Equals, tag)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t)
		})
	}
}
