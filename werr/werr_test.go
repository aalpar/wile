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

package werr_test

import (
	"errors"
	"fmt"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/werr"
)

func TestForeignError_EqualTo(t *testing.T) {
}

func TestForeignFileError_ErrorsAs(t *testing.T) {
	c := qt.New(t)
	err := werr.WrapForeignFileError(fmt.Errorf("no such file"), "open-input-file", "/tmp/missing")
	var fileErr *werr.ForeignFileError
	c.Assert(errors.As(err, &fileErr), qt.IsTrue)
	c.Assert(fileErr.Filename, qt.Equals, "/tmp/missing")
	c.Assert(fileErr.Op, qt.Equals, "open-input-file")
}

func TestForeignFileError_ErrorsAs_Wrapped(t *testing.T) {
	c := qt.New(t)
	inner := werr.WrapForeignFileError(fmt.Errorf("no such file"), "open-input-file", "/tmp/missing")
	outer := fmt.Errorf("wrapped: %w", inner)
	var fileErr *werr.ForeignFileError
	c.Assert(errors.As(outer, &fileErr), qt.IsTrue)
	c.Assert(fileErr.Filename, qt.Equals, "/tmp/missing")
}

func TestForeignFileError_NotDetectedAsReadError(t *testing.T) {
	c := qt.New(t)
	err := werr.WrapForeignFileError(fmt.Errorf("no such file"), "open-input-file", "/tmp/missing")
	var readErr *werr.ForeignReadError
	c.Assert(errors.As(err, &readErr), qt.IsFalse)
}

func TestForeignReadError_ErrorsAs(t *testing.T) {
	c := qt.New(t)
	err := werr.WrapForeignReadErrorf(fmt.Errorf("unexpected token"), "read error")
	var readErr *werr.ForeignReadError
	c.Assert(errors.As(err, &readErr), qt.IsTrue)
}

func TestForeignReadError_ErrorsAs_Wrapped(t *testing.T) {
	c := qt.New(t)
	inner := werr.WrapForeignReadErrorf(fmt.Errorf("unexpected token"), "read error")
	outer := fmt.Errorf("wrapped: %w", inner)
	var readErr *werr.ForeignReadError
	c.Assert(errors.As(outer, &readErr), qt.IsTrue)
}

func TestForeignReadError_NotDetectedAsFileError(t *testing.T) {
	c := qt.New(t)
	err := werr.WrapForeignReadErrorf(fmt.Errorf("unexpected token"), "read error")
	var fileErr *werr.ForeignFileError
	c.Assert(errors.As(err, &fileErr), qt.IsFalse)
}

func TestNewForeignReadErrorf(t *testing.T) {
	c := qt.New(t)
	err := werr.NewForeignReadErrorf("parse error at %d", 42)
	var readErr *werr.ForeignReadError
	c.Assert(errors.As(err, &readErr), qt.IsTrue)
	c.Assert(err.Error(), qt.Matches, ".*parse error at 42.*")
}

func TestForeignFileError_ErrorsIs_Cause(t *testing.T) {
	c := qt.New(t)
	cause := fmt.Errorf("permission denied")
	err := werr.WrapForeignFileError(cause, "open-input-file", "/etc/shadow")
	c.Assert(errors.Is(err, cause), qt.IsTrue)
}

func TestForeignReadError_ErrorsIs_Cause(t *testing.T) {
	c := qt.New(t)
	cause := fmt.Errorf("unexpected )")
	err := werr.WrapForeignReadErrorf(cause, "read error")
	c.Assert(errors.Is(err, cause), qt.IsTrue)
}

func TestForeignError_Is(t *testing.T) {
	c := qt.New(t)
	sentinel := werr.NewStaticError("test sentinel")
	cause := fmt.Errorf("root cause")

	tcs := []struct {
		name   string
		err    *werr.ForeignError
		target error
		want   bool
	}{
		{"sentinel match", werr.WrapForeignErrorf(sentinel, "msg"), sentinel, true},
		{"cause match", werr.WrapForeignErrorWithCause(sentinel, cause, "msg"), cause, true},
		{"sentinel via cause constructor", werr.WrapForeignErrorWithCause(sentinel, cause, "msg"), sentinel, true},
		{"no match", werr.WrapForeignErrorf(sentinel, "msg"), fmt.Errorf("other"), false},
		{"nil sentinel and cause", werr.NewForeignErrorf("msg"), sentinel, false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(errors.Is(tc.err, tc.target), qt.Equals, tc.want)
		})
	}
}

func TestForeignError_As(t *testing.T) {
	c := qt.New(t)

	// ForeignFileError detected through embedded ForeignError
	fileErr := werr.WrapForeignFileError(fmt.Errorf("no such file"), "open", "/tmp/x")
	wrapped := fmt.Errorf("outer: %w", fileErr)
	var target *werr.ForeignFileError
	c.Assert(errors.As(wrapped, &target), qt.IsTrue)
	c.Assert(target.Filename, qt.Equals, "/tmp/x")

	// ForeignReadError not detected through ForeignFileError
	var readTarget *werr.ForeignReadError
	c.Assert(errors.As(wrapped, &readTarget), qt.IsFalse)
}

func TestForeignError_Cause(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		err  *werr.ForeignError
		want error
	}{
		{"with cause", werr.WrapForeignErrorWithCause(werr.ErrNotANumber, fmt.Errorf("root"), "msg"), fmt.Errorf("root")},
		{"without cause", werr.WrapForeignErrorf(werr.ErrNotANumber, "msg"), nil},
		{"nil receiver", nil, nil},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := tc.err.Cause()
			if tc.want == nil {
				c.Assert(got, qt.IsNil)
			} else {
				c.Assert(got.Error(), qt.Equals, tc.want.Error())
			}
		})
	}
}

func TestWrapForeignErrorWithCause(t *testing.T) {
	c := qt.New(t)
	sentinel := werr.NewStaticError("test sentinel")
	cause := fmt.Errorf("disk full")

	err := werr.WrapForeignErrorWithCause(sentinel, cause, "write failed: %s", "/tmp/out")
	c.Assert(errors.Is(err, sentinel), qt.IsTrue)
	c.Assert(errors.Is(err, cause), qt.IsTrue)
	c.Assert(err.Cause(), qt.Equals, cause)
	c.Assert(err.Error(), qt.Matches, ".*write failed.*test sentinel.*")
}
