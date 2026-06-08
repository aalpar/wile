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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/werr"
)

// ExpandAndCompile runs the expand+compile pipeline on a single syntax value,
// returning a ready-to-execute template.
//
// A single MacroEvaluator is shared across both phases. If resolver is non-nil,
// it is set on the compiler for include/load file resolution. inlineThreshold
// controls procedure inlining (0 disables). maxExpandDepth bounds expander
// recursion to prevent a fatal Go stack overflow on deeply nested syntax
// (0 disables the bound; pass DefaultMaxExpandDepth for the standard limit).
// Errors are wrapped with phase context ("expansion" or "compilation"); callers
// may add site-specific context on top. Callers may call tpl.Optimize() on the
// returned template if desired.
func ExpandAndCompile(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue, resolver FileResolver, inlineThreshold int, maxExpandDepth int) (*machine.NativeTemplate, error) {
	evaluator := machine.NewVMMacroEvaluator()

	expander := NewExpanderTimeContinuation(ctx, env, evaluator)
	expander.SetMaxDepth(maxExpandDepth)
	expanded, err := expander.ExpandExpression(stx)
	if err != nil {
		return nil, wrapSourcedError(stx.SourceContext(), werr.WrapForeignErrorf(err, "expansion"))
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := NewCompileTimeCallContext(ctx, false)
	compiler := NewCompileTimeContinuation(tpl, env, evaluator)
	if resolver != nil {
		compiler.SetFileResolver(resolver)
	}
	compiler.SetInlineThreshold(inlineThreshold)
	err = compiler.CompileExpression(cctx, expanded)
	if err != nil {
		return nil, wrapSourcedError(stx.SourceContext(), werr.WrapForeignErrorf(err, "compilation"))
	}

	return tpl, nil
}
