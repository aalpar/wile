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
	"strings"
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestPromptTag(t *testing.T) {
	tcs := []struct {
		name    string
		checkFn func(t *testing.T)
	}{
		{
			name: "NewPromptTag returns unique tags",
			checkFn: func(t *testing.T) {
				a := NewPromptTag("alpha")
				b := NewPromptTag("beta")
				qt.Assert(t, a.id != b.id, qt.IsTrue)
			},
		},
		{
			name: "NewPromptTag sequential IDs are monotonically increasing",
			checkFn: func(t *testing.T) {
				a := NewPromptTag("x")
				b := NewPromptTag("y")
				qt.Assert(t, b.id > a.id, qt.IsTrue)
			},
		},
		{
			name: "SchemeString with name",
			checkFn: func(t *testing.T) {
				tag := NewPromptTag("test-tag")
				qt.Assert(t, tag.SchemeString(), qt.Equals, "#<continuation-prompt-tag:test-tag>")
			},
		},
		{
			name: "SchemeString without name uses ID",
			checkFn: func(t *testing.T) {
				tag := NewPromptTag("")
				s := tag.SchemeString()
				qt.Assert(t, strings.HasPrefix(s, "#<continuation-prompt-tag:"), qt.IsTrue)
				qt.Assert(t, strings.HasSuffix(s, ">"), qt.IsTrue)
				// Should NOT contain the empty string between colon and >
				qt.Assert(t, s != "#<continuation-prompt-tag:>", qt.IsTrue)
			},
		},
		{
			name: "IsVoid returns false for non-nil tag",
			checkFn: func(t *testing.T) {
				tag := NewPromptTag("v")
				qt.Assert(t, tag.IsVoid(), qt.IsFalse)
			},
		},
		{
			name: "IsVoid returns true for nil tag",
			checkFn: func(t *testing.T) {
				var tag *PromptTag
				qt.Assert(t, tag.IsVoid(), qt.IsTrue)
			},
		},
		{
			name: "EqualTo self is true (pointer identity)",
			checkFn: func(t *testing.T) {
				tag := NewPromptTag("same")
				qt.Assert(t, tag.EqualTo(tag), qt.IsTrue)
			},
		},
		{
			name: "EqualTo different tag is false",
			checkFn: func(t *testing.T) {
				a := NewPromptTag("a")
				b := NewPromptTag("a") // same name, different pointer
				qt.Assert(t, a.EqualTo(b), qt.IsFalse)
			},
		},
		{
			name: "EqualTo different type is false",
			checkFn: func(t *testing.T) {
				tag := NewPromptTag("x")
				qt.Assert(t, tag.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
		{
			name: "DefaultPromptTag exists and is a PromptTag",
			checkFn: func(t *testing.T) {
				qt.Assert(t, DefaultPromptTag, qt.IsNotNil)
				qt.Assert(t, DefaultPromptTag.name, qt.Equals, "default")
				qt.Assert(t, DefaultPromptTag.SchemeString(), qt.Equals, "#<continuation-prompt-tag:default>")
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t)
		})
	}
}
