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
	"context"

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

const (
	// TransformerSyntaxRules is the leading symbol for (syntax-rules ...) transformers.
	TransformerSyntaxRules = "syntax-rules"
	// TransformerERMacro is the leading symbol for (er-macro-transformer ...) transformers.
	TransformerERMacro = "er-macro-transformer"
	// FormDefineSyntax is the form name for (define-syntax ...) definitions.
	FormDefineSyntax = "define-syntax"
)

// compileTransformerValue evaluates a define-syntax / let-syntax / letrec-syntax
// right-hand side as an EXPRESSION one phase above env (R6RS §11.2.2).
//
// The value is stored bare: a procedure is a transformer, anything else is a
// compile-time value readable by syntax-local-value and an ErrNotAClosure in
// operator position (expandMacroInvocation). P0.4 lifted the admission switch
// that used to refuse a non-procedure here — Q4: there is no wrapper type and
// nothing to unwrap, so a let-syntax or define-syntax right-hand side is simply
// an expression whose value the binding holds.
//
// The head switch this replaces dispatched on the spelling of the right-hand
// side's car and never expanded it, so a macro that EXPANDS to a transformer was
// refused (design §5.2). syntax-rules and er-macro-transformer are now
// expression-level syntax compilers (compile_transformer_forms.go) that yield the
// same values the switch built.
//
// libraryScope rides into the right-hand-side unit on both the expander and the
// compiler: the syntax-rules producer stamps it on the free template identifiers
// it resolved at definition time (match/syntax_expand.go:457, in the globalBinding
// arm), and after P0.3 so does quote-syntax. It is not the only route into the
// library env — a library body is already stamped with its own scope before
// expansion (compile_time_continuation_include.go:181, reached from
// compileLibraryBegin) — so this thread is parity, not the load-bearing path.
func compileTransformerValue(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	rhs syntax.SyntaxValue,
	libraryScope *syntax.Scope,
	evaluator machine.MacroEvaluator,
) (values.Value, error) {
	expandEnv := env.NextPhase()
	tpl, err := expandAndCompileScoped(ctx, expandEnv, rhs, nil, DefaultInlineThreshold, DefaultMaxExpandDepth, libraryScope)
	if err != nil {
		return nil, wrapSourcedError(rhs.SourceContext(), werr.WrapForeignErrorf(err, "transformer"))
	}
	result, err := evaluator.EvalTemplate(ctx, tpl, expandEnv)
	if err != nil {
		return nil, wrapSourcedError(rhs.SourceContext(), werr.WrapForeignErrorf(err, "error evaluating transformer"))
	}
	return result, nil
}
