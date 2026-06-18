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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// Close Port Tests (R7RS §6.13.1)

func TestClosePortOnStringPorts(t *testing.T) {
	// close-port should work on string ports without error
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((p (open-input-string "test")))
			(close-port p)
			#t)
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)

	result, err = testhelpers.RunSchemeCode(t, `
		(let ((p (open-output-string)))
			(close-port p)
			#t)
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestClosePortOnBytevectorPorts(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((p (open-input-bytevector #u8(1 2 3))))
			(close-port p)
			#t)
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)

	result, err = testhelpers.RunSchemeCode(t, `
		(let ((p (open-output-bytevector)))
			(close-port p)
			#t)
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}
