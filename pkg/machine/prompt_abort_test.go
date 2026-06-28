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
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestErrPromptAbort(t *testing.T) {
	tcs := []struct {
		name    string
		checkFn func(t *testing.T)
	}{
		{
			name: "Error message includes tag SchemeString",
			checkFn: func(t *testing.T) {
				tag := NewPromptTag("my-tag")
				err := &ErrPromptAbort{
					Tag:    tag,
					Values: nil,
				}
				qt.Assert(t, err.Error(), qt.Equals, "abort to prompt #<continuation-prompt-tag:my-tag>")
			},
		},
		{
			name: "Tag field accessible",
			checkFn: func(t *testing.T) {
				tag := NewPromptTag("t")
				err := &ErrPromptAbort{
					Tag: tag,
				}
				qt.Assert(t, err.Tag, qt.Equals, tag)
			},
		},
		{
			name: "Values field accessible with multiple values",
			checkFn: func(t *testing.T) {
				tag := NewPromptTag("v")
				vs := []values.Value{
					values.NewInteger(1),
					values.NewInteger(2),
					values.NewInteger(3),
				}
				err := &ErrPromptAbort{
					Tag:    tag,
					Values: vs,
				}
				qt.Assert(t, err.Values, qt.HasLen, 3)
			},
		},
		{
			name: "Values field nil when no values",
			checkFn: func(t *testing.T) {
				tag := NewPromptTag("n")
				err := &ErrPromptAbort{
					Tag:    tag,
					Values: nil,
				}
				qt.Assert(t, err.Values, qt.IsNil)
			},
		},
		{
			name: "satisfies error interface",
			checkFn: func(t *testing.T) {
				tag := NewPromptTag("e")
				err := &ErrPromptAbort{
					Tag: tag,
				}
				var e error = err
				qt.Assert(t, e.Error(), qt.Contains, "abort to prompt")
			},
		},
		{
			name: "errors.As matches ErrPromptAbort",
			checkFn: func(t *testing.T) {
				tag := NewPromptTag("as")
				orig := &ErrPromptAbort{
					Tag:    tag,
					Values: []values.Value{values.NewInteger(42)},
				}
				var target *ErrPromptAbort
				qt.Assert(t, errors.As(orig, &target), qt.IsTrue)
				qt.Assert(t, target.Tag, qt.Equals, tag)
				qt.Assert(t, target.Values, qt.HasLen, 1)
			},
		},
		{
			name: "errors.As does not match unrelated error",
			checkFn: func(t *testing.T) {
				other := errors.New("something else")
				var target *ErrPromptAbort
				qt.Assert(t, errors.As(other, &target), qt.IsFalse)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t)
		})
	}
}

func TestErrResumeContinuation(t *testing.T) {
	tcs := []struct {
		name    string
		checkFn func(t *testing.T)
	}{
		{
			name: "satisfies error interface with non-empty message",
			checkFn: func(t *testing.T) {
				var e error = &ErrResumeContinuation{Tag: DefaultPromptTag}
				qt.Assert(t, len(e.Error()) > 0, qt.IsTrue)
			},
		},
		{
			name: "errors.As matches ErrResumeContinuation",
			checkFn: func(t *testing.T) {
				var e error = &ErrResumeContinuation{Tag: DefaultPromptTag, Isolate: true}
				var target *ErrResumeContinuation
				qt.Assert(t, errors.As(e, &target), qt.IsTrue)
				qt.Assert(t, target.Tag, qt.Equals, DefaultPromptTag)
				qt.Assert(t, target.Isolate, qt.IsTrue)
			},
		},
		{
			name: "type-distinct from ErrPromptAbort",
			checkFn: func(t *testing.T) {
				var e error = &ErrResumeContinuation{Tag: DefaultPromptTag}
				var abortTarget *ErrPromptAbort
				qt.Assert(t, errors.As(e, &abortTarget), qt.IsFalse)
			},
		},
		{
			name: "Values field carries resume args",
			checkFn: func(t *testing.T) {
				err := &ErrResumeContinuation{
					Tag:    DefaultPromptTag,
					Values: []values.Value{values.NewInteger(7)},
				}
				qt.Assert(t, err.Values, qt.HasLen, 1)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tc.checkFn(t)
		})
	}
}
