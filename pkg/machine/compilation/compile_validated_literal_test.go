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

import (
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// TestIsLiteralFalseSeesPassthroughForms guards the R7RS feature-detection
// idiom at its seam. A Tier-2 passthrough form (include, import, ...) is a
// *validate.ValidatedLiteral, the same Go type as a self-evaluating datum, told
// apart only by the FormName a compiler claims. isLiteralFalse must report such a
// form as not-a-literal, or the if constant-fold would discard it uncompiled.
//
// The end-to-end version of this guard used cond-expand, which no longer reaches
// the compiler (it splices at expand time), so it is built here from the
// validator's own types. Mutation: dropping BOTH the compilerFor check and the
// forms-registry check in isSelfEvaluatingLiteral turns the include row into
// (true, true); either alone is covered by the other.
func TestIsLiteralFalseSeesPassthroughForms(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	tpl := machine.NewNativeTemplate(0, 0, false)
	p := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())

	literal := func(formName string, v values.Value) *validate.ValidatedLiteral {
		lit := &validate.ValidatedLiteral{
			Value: syntax.NewSyntaxObject(v, syntax.NewZeroValueSourceContext()),
			Env:   env,
		}
		lit.SetFormName(formName)
		return lit
	}

	tcs := []struct {
		name        string
		expr        validate.ValidatedExpr
		wantLiteral bool
		wantFalse   bool
	}{
		{name: "bare #f folds", expr: literal("@literal", values.FalseValue), wantLiteral: true, wantFalse: true},
		{name: "bare truthy literal folds", expr: literal("@literal", values.NewInteger(0)), wantLiteral: true},
		{name: "an include form carrying #f is not a literal", expr: literal("include", values.FalseValue)},
		{name: "an import form carrying #f is not a literal", expr: literal("import", values.FalseValue)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			isLiteral, isFalse := p.isLiteralFalse(tc.expr)

			qt.Assert(t, isLiteral, qt.Equals, tc.wantLiteral)
			qt.Assert(t, isFalse, qt.Equals, tc.wantFalse)
		})
	}
}
