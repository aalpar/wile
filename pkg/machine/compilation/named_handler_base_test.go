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

package compilation

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestNamedHandlerBase(t *testing.T) {
	tcs := []struct {
		name    string
		handler namedHandlerBase
		checkFn func(t *testing.T, h *namedHandlerBase)
	}{
		{
			name: "Name returns handler name",
			handler: namedHandlerBase{
				name:   "define-syntax",
				prefix: "syntax-compiler",
			},
			checkFn: func(t *testing.T, h *namedHandlerBase) {
				qt.Assert(t, h.Name(), qt.Equals, "define-syntax")
			},
		},
		{
			name: "IsVoid returns false",
			handler: namedHandlerBase{
				name:   "if",
				prefix: "expander",
			},
			checkFn: func(t *testing.T, h *namedHandlerBase) {
				qt.Assert(t, h.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "SchemeString format with prefix and name",
			handler: namedHandlerBase{
				name:   "lambda",
				prefix: "primitive-expander",
			},
			checkFn: func(t *testing.T, h *namedHandlerBase) {
				qt.Assert(t, h.SchemeString(), qt.Equals, "#<primitive-expander:lambda>")
			},
		},
		{
			name: "SchemeString with empty name",
			handler: namedHandlerBase{
				name:   "",
				prefix: "compiler",
			},
			checkFn: func(t *testing.T, h *namedHandlerBase) {
				qt.Assert(t, h.SchemeString(), qt.Equals, "#<compiler:>")
			},
		},
		{
			name: "SchemeString with empty prefix",
			handler: namedHandlerBase{
				name:   "test",
				prefix: "",
			},
			checkFn: func(t *testing.T, h *namedHandlerBase) {
				qt.Assert(t, h.SchemeString(), qt.Equals, "#<:test>")
			},
		},
		{
			name:    "zero value handler",
			handler: namedHandlerBase{},
			checkFn: func(t *testing.T, h *namedHandlerBase) {
				qt.Assert(t, h.Name(), qt.Equals, "")
				qt.Assert(t, h.IsVoid(), qt.IsFalse)
				qt.Assert(t, h.SchemeString(), qt.Equals, "#<:>")
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t, &tc.handler)
		})
	}
}
