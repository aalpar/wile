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

// CompileBeginForSyntax handles (begin-for-syntax expr ...) for the compiler.
//
// The body is phase-(N+1) code with no runtime effect, and it has ALREADY RUN:
// expandBeginForSyntax ran it when the expander reached the form. So this only
// re-checks the shape and emits nothing. Running it here as well would repeat its
// side effects.
func (p *CompileTimeContinuation) CompileBeginForSyntax(_ CompileTimeCallContext, expr syntax.SyntaxValue) error {
	_, err := p.beginForSyntaxBody(expr)
	return err
}

// runBeginForSyntax evaluates the body of (begin-for-syntax expr ...) one phase
// up. Used for setting up compile-time state (hash tables, registries) that
// macros can access. Called by the expander; see expandBeginForSyntax.
func (p *CompileTimeContinuation) runBeginForSyntax(ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	body, err := p.beginForSyntaxBody(expr)
	if err != nil {
		return err
	}
	if body == nil {
		return nil
	}
	return p.executeFormsAtCompileTime(ctctx, "begin-for-syntax", body)
}

// beginForSyntaxBody returns the body of a begin-for-syntax form, or nil for an
// empty one.
func (p *CompileTimeContinuation) beginForSyntaxBody(expr syntax.SyntaxValue) (*syntax.SyntaxPair, error) {
	err := p.ensureState("begin-for-syntax")
	if err != nil {
		return nil, err
	}
	if syntax.IsSyntaxEmptyList(expr) {
		return nil, nil
	}
	q, ok := expr.(*syntax.SyntaxPair)
	if !ok {
		return nil, p.wrapCompilationError(werr.WrapForeignErrorf(werr.ErrNotASyntaxPair, "begin-for-syntax: expected expressions"))
	}
	return q, nil
}
