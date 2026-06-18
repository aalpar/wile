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

// External test package to allow importing validate and machine/compilation
// without creating an import cycle (forms is imported by both).
package forms_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/machine/compilation"

	// Blank import triggers init() registration in validate.
	_ "github.com/aalpar/wile/pkg/internal/validate"

	qt "github.com/frankban/quicktest"
)

// TestFormRegistrationConsistency verifies that every registered special form
// has a validator (forms.Verify) and a compiler (compilation.VerifyCompilers),
// except for expand-time-only forms. This catches the "forgot to register"
// class of bug when validators and compilers are registered from separate
// init() functions in different packages.
func TestFormRegistrationConsistency(t *testing.T) {
	c := qt.New(t)
	c.Assert(forms.Verify(), qt.IsNil)
	c.Assert(compilation.VerifyCompilers(), qt.IsNil)
}
