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

package compilation

// Tests displaced from machine/ when operations moved to compilation/.

import (
	"testing"

	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestOperationSyntaxRulesTransform_Methods(t *testing.T) {
	op := NewOperationSyntaxRulesTransform()
	qt.Assert(t, op.String(), qt.Contains, "SyntaxRulesTransform")
	qt.Assert(t, op.SchemeString(), qt.Contains, "syntax-rules-transform")
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
	qt.Assert(t, op.EqualTo(op), qt.IsTrue)
	qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestOperationSyntaxRulesTransform_EqualTo(t *testing.T) {
	op := NewOperationSyntaxRulesTransform()
	qt.Assert(t, op.String(), qt.Equals, "SyntaxRulesTransform")
	qt.Assert(t, op.SchemeString(), qt.Contains, "syntax-rules")
	qt.Assert(t, op.IsVoid(), qt.IsFalse)
	qt.Assert(t, op.EqualTo(NewOperationSyntaxRulesTransform()), qt.IsTrue)
	qt.Assert(t, op.EqualTo(values.NewInteger(1)), qt.IsFalse)
}
