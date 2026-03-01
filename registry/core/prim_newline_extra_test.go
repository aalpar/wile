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

package core_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
)

// Newline Tests (R7RS §6.13.3)

func TestNewlineWithExplicitPort(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((p (open-output-string)))
			(newline p)
			(get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, str.Value, qt.Equals, "\n")
}

func TestMultipleNewlines(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((p (open-output-string)))
			(newline p)
			(newline p)
			(newline p)
			(get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, str.Value, qt.Equals, "\n\n\n")
}
