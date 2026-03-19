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

func TestBarrierToken(t *testing.T) {
	tcs := []struct {
		name    string
		checkFn func(t *testing.T)
	}{
		{
			name: "NewBarrierToken returns non-nil",
			checkFn: func(t *testing.T) {
				tok := NewBarrierToken()
				qt.Assert(t, tok, qt.IsNotNil)
			},
		},
		{
			name: "two tokens have different pointer identity",
			checkFn: func(t *testing.T) {
				a := NewBarrierToken()
				b := NewBarrierToken()
				qt.Assert(t, a != b, qt.IsTrue)
			},
		},
		{
			name: "same token equals itself",
			checkFn: func(t *testing.T) {
				tok := NewBarrierToken()
				other := tok
				qt.Assert(t, tok == other, qt.IsTrue)
			},
		},
		{
			name: "nil is valid (no active barrier)",
			checkFn: func(t *testing.T) {
				var tok *BarrierToken
				qt.Assert(t, tok == nil, qt.IsTrue)
			},
		},
		{
			name: "non-nil token does not equal nil",
			checkFn: func(t *testing.T) {
				tok := NewBarrierToken()
				qt.Assert(t, tok != nil, qt.IsTrue)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t)
		})
	}
}
