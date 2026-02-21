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

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// Current Port Tests (R7RS §6.13.1)

func TestCurrentInputPortIsInputPort(t *testing.T) {
	result, err := runSchemeCode(t, `(input-port? (current-input-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestCurrentOutputPortIsOutputPort(t *testing.T) {
	result, err := runSchemeCode(t, `(output-port? (current-output-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestCurrentInputPortIsPort(t *testing.T) {
	result, err := runSchemeCode(t, `(port? (current-input-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestCurrentOutputPortIsPort(t *testing.T) {
	result, err := runSchemeCode(t, `(port? (current-output-port))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}
