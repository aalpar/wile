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

// External test package to allow importing validate and machine
// without creating an import cycle (forms is imported by both).
package forms_test

import (
	"testing"

	"github.com/aalpar/wile/internal/forms"

	// Blank imports trigger init() registrations.
	_ "github.com/aalpar/wile/internal/validate"
	_ "github.com/aalpar/wile/machine"

	qt "github.com/frankban/quicktest"
)

// TestFormRegistrationConsistency verifies that every registered special form
// has both a validator and a compiler (or is in the expand-time-only exception
// list). This catches the "forgot to add the compiler" class of bug when
// validators and compilers are registered from separate init() functions.
func TestFormRegistrationConsistency(t *testing.T) {
	c := qt.New(t)
	err := forms.Verify()
	c.Assert(err, qt.IsNil)
}
