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

package machine

// body_passes.go contains per-body annotation extraction passes that run
// after validation and predeclaration but before compilation. Each pass
// inspects leading forms in the body, stores metadata on the template,
// and returns the remaining executable body.
//
// Ordering note: these passes run after predeclaration (letrec* forward
// references are already visible). This ordering is preserved intentionally —
// future passes (e.g., contract declarations) may need to reference
// predeclared bindings. Do not reorder without verifying that assumption.
//
// Pass signature: func([]validate.ValidatedExpr, *NativeTemplate) []validate.ValidatedExpr
//
// Each pass:
//   - Receives the current body (already trimmed by earlier passes)
//   - May strip leading forms that are metadata, not executable code
//   - Stores extracted metadata on the NativeTemplate
//   - Returns the remaining body

import (
	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/values"
)

// processBodyAnnotations runs all annotation extraction passes on the body.
// Each pass may strip leading forms and store metadata on the template.
// Returns the remaining executable body.
func processBodyAnnotations(body []validate.ValidatedExpr, tpl *NativeTemplate) []validate.ValidatedExpr {
	body = extractDocstring(body, tpl)
	return body
}

// extractDocstring checks whether the first body expression is a string
// literal (Guile-style docstring). If so, stores it on the template and
// returns the remaining body. The string must not be the only expression
// (a body of just "hello" is a return value, not documentation).
func extractDocstring(body []validate.ValidatedExpr, tpl *NativeTemplate) []validate.ValidatedExpr {
	if len(body) < 2 {
		return body
	}
	lit, ok := body[0].(*validate.ValidatedLiteral)
	if !ok {
		return body
	}
	str, ok := lit.Value.UnwrapAll().(*values.String)
	if !ok {
		return body
	}
	tpl.SetDoc(str.Value)
	return body[1:]
}
