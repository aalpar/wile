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

package wile

import (
	"context"
	"strings"

	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/syntax"
)

// CheckProgram parses, expands, and compiles code as a single top-level program
// without executing it, and reports the first error with source location — the
// go build equivalent for a Scheme program. It returns nil when the program
// compiles clean. source labels the code in diagnostics; pass "" if none.
//
// It mirrors [Engine.EvalProgram] exactly, including the whole-program (begin
// form ...) splice that makes top-level defines mutually visible, and omits only
// the final execution step. What that buys is every diagnostic the compiler
// already produces — unbound names, malformed special forms, macro-expansion
// failures, call-site arity against a statically known callee — reported for
// code paths a test run would have to reach to discover, such as the body of a
// procedure that is never called.
//
// Two things it is NOT:
//
// Not side-effect-free. (import ...) executes the imported library's body during
// compilation of the importing form (see
// machine/compilation/library_loader.go, compileAndExecuteLibrary), so checking
// a program that imports a side-effecting library runs those effects. Only the
// checked program's own top level is guaranteed not to run.
//
// Not read-only with respect to the engine. Compiling a top-level define
// registers its binding — that registration is what makes forward references
// resolve — so a checked program leaves its top-level names defined in this
// engine's namespace. Consequences for a caller checking more than one program
// on one engine: later programs see earlier programs' definitions, and checking
// the same program twice reports a redefinition error against the first pass
// under the default immutable top level. Use a fresh engine per program when
// either matters.
func (p *Engine) CheckProgram(ctx context.Context, code string, source string) error {
	pr := parser.NewParserWithFile(p.env, true, strings.NewReader(code), source)
	pr.SetMaxDepth(p.maxParseDepth)

	var forms []syntax.SyntaxValue
	for {
		stx, err := pr.ReadSyntax(ctx)
		if err != nil {
			if isEOF(err) {
				break
			}
			return wrapCompilationError("parse error", err)
		}
		forms = append(forms, stx)
	}
	if len(forms) == 0 {
		// No forms (empty input, or only whitespace/comments) — like (begin).
		return nil
	}

	_, err := p.compileExpr(ctx, wrapInBegin(forms))
	return err
}
