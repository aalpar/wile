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
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// CompileBeginForSyntax handles (begin-for-syntax expr ...).
//
// Evaluates a sequence of expressions at compile time in the expand phase
// environment. Used for setting up compile-time state (hash tables, registries)
// that macros can access. No runtime effect — used for side effects only.
func (p *CompileTimeContinuation) CompileBeginForSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	err := p.ensureState("begin-for-syntax")
	if err != nil {
		return err
	}

	if syntax.IsSyntaxEmptyList(expr) {
		return nil
	}
	exprPair, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxPair, "begin-for-syntax: expected expressions"))
	}

	return p.executeFormsAtCompileTime(ctctx, "begin-for-syntax", exprPair)
}
