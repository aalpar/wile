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

package values

import (
	"errors"
	"fmt"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestForeignError_EqualTo(t *testing.T) {
}

func TestForeignFileError_ErrorsAs(t *testing.T) {
	c := qt.New(t)
	err := WrapForeignFileError(fmt.Errorf("no such file"), "open-input-file", "/tmp/missing")
	var fileErr *ForeignFileError
	c.Assert(errors.As(err, &fileErr), qt.IsTrue)
	c.Assert(fileErr.Filename, qt.Equals, "/tmp/missing")
	c.Assert(fileErr.Op, qt.Equals, "open-input-file")
}

func TestForeignFileError_ErrorsAs_Wrapped(t *testing.T) {
	c := qt.New(t)
	inner := WrapForeignFileError(fmt.Errorf("no such file"), "open-input-file", "/tmp/missing")
	outer := fmt.Errorf("wrapped: %w", inner)
	var fileErr *ForeignFileError
	c.Assert(errors.As(outer, &fileErr), qt.IsTrue)
	c.Assert(fileErr.Filename, qt.Equals, "/tmp/missing")
}

func TestForeignFileError_NotDetectedAsReadError(t *testing.T) {
	c := qt.New(t)
	err := WrapForeignFileError(fmt.Errorf("no such file"), "open-input-file", "/tmp/missing")
	var readErr *ForeignReadError
	c.Assert(errors.As(err, &readErr), qt.IsFalse)
}

func TestForeignReadError_ErrorsAs(t *testing.T) {
	c := qt.New(t)
	err := WrapForeignReadErrorf(fmt.Errorf("unexpected token"), "read error")
	var readErr *ForeignReadError
	c.Assert(errors.As(err, &readErr), qt.IsTrue)
}

func TestForeignReadError_ErrorsAs_Wrapped(t *testing.T) {
	c := qt.New(t)
	inner := WrapForeignReadErrorf(fmt.Errorf("unexpected token"), "read error")
	outer := fmt.Errorf("wrapped: %w", inner)
	var readErr *ForeignReadError
	c.Assert(errors.As(outer, &readErr), qt.IsTrue)
}

func TestForeignReadError_NotDetectedAsFileError(t *testing.T) {
	c := qt.New(t)
	err := WrapForeignReadErrorf(fmt.Errorf("unexpected token"), "read error")
	var fileErr *ForeignFileError
	c.Assert(errors.As(err, &fileErr), qt.IsFalse)
}

func TestNewForeignReadErrorf(t *testing.T) {
	c := qt.New(t)
	err := NewForeignReadErrorf("parse error at %d", 42)
	var readErr *ForeignReadError
	c.Assert(errors.As(err, &readErr), qt.IsTrue)
	c.Assert(err.Error(), qt.Matches, ".*parse error at 42.*")
}

func TestForeignFileError_Unwrap(t *testing.T) {
	c := qt.New(t)
	cause := fmt.Errorf("permission denied")
	err := WrapForeignFileError(cause, "open-input-file", "/etc/shadow")
	c.Assert(errors.Is(err, cause), qt.IsTrue)
}

func TestForeignReadError_Unwrap(t *testing.T) {
	c := qt.New(t)
	cause := fmt.Errorf("unexpected )")
	err := WrapForeignReadErrorf(cause, "read error")
	c.Assert(errors.Is(err, cause), qt.IsTrue)
}

func TestForeignFileError_Unwrap_Nil(t *testing.T) {
	c := qt.New(t)
	var err *ForeignFileError
	c.Assert(err.Unwrap(), qt.IsNil)
}

func TestForeignReadError_Unwrap_Nil(t *testing.T) {
	c := qt.New(t)
	var err *ForeignReadError
	c.Assert(err.Unwrap(), qt.IsNil)
}
