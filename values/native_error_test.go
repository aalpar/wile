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

package values_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func TestNativeError_EqualTo(t *testing.T) {
}

func TestNewFileError_SetsKindFile(t *testing.T) {
	c := qt.New(t)
	err := values.NewFileError("file not found", values.NewString("/tmp/foo"))
	c.Assert(err.Kind(), qt.Equals, values.NativeErrorKindFile)
	c.Assert(err.IsFileError(), qt.IsTrue)
	c.Assert(err.IsReadError(), qt.IsFalse)
}

func TestNewReadError_SetsKindRead(t *testing.T) {
	c := qt.New(t)
	err := values.NewReadError("unexpected token")
	c.Assert(err.Kind(), qt.Equals, values.NativeErrorKindRead)
	c.Assert(err.IsReadError(), qt.IsTrue)
	c.Assert(err.IsFileError(), qt.IsFalse)
}

func TestNewErrorObjectWithCauseAndKind(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name string
		kind values.NativeErrorKind
		file bool
		read bool
	}{
		{"generic", values.NativeErrorKindGeneric, false, false},
		{"file", values.NativeErrorKindFile, true, false},
		{"read", values.NativeErrorKindRead, false, true},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			cause := values.ExportNewForeignError("underlying error")
			err := values.NewErrorObjectWithCauseAndKind("msg", cause, tt.kind)
			c.Assert(err.Kind(), qt.Equals, tt.kind)
			c.Assert(err.IsFileError(), qt.Equals, tt.file)
			c.Assert(err.IsReadError(), qt.Equals, tt.read)
			c.Assert(err.Datum(), qt.Equals, cause)
		})
	}
}
