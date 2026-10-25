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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// NamespaceMaxExpandDepth returns the engine's configured WithMaxExpandDepth
// bound as seen from env, else DefaultMaxExpandDepth. Nil-safe: frames with no
// namespace exist (test fixtures), and a namespace not built by an Engine reports
// unset. An explicit 0 (unlimited) is returned as 0. This is the reader for every
// site except the bootstrap pass (runBootstrapStx), which uses the default.
func NamespaceMaxExpandDepth(env *environment.EnvironmentFrame) int {
	ns := env.Namespace()
	if ns == nil {
		return DefaultMaxExpandDepth
	}
	n, ok := ns.MaxExpandDepth()
	if !ok {
		return DefaultMaxExpandDepth
	}
	return n
}

// NamespaceInlineThreshold is NamespaceMaxExpandDepth's counterpart for
// WithInlineThreshold, defaulting to DefaultInlineThreshold.
func NamespaceInlineThreshold(env *environment.EnvironmentFrame) int {
	ns := env.Namespace()
	if ns == nil {
		return DefaultInlineThreshold
	}
	n, ok := ns.InlineThreshold()
	if !ok {
		return DefaultInlineThreshold
	}
	return n
}

// ExpandAndCompile runs the expand+compile pipeline on a single syntax value,
// returning a ready-to-execute template.
//
// A single MacroEvaluator is shared across both phases. Configuration is read
// from env's namespace, as at every nested site: include/load resolve through
// env.FileResolver(), inlining follows NamespaceInlineThreshold (0 disables), and
// expander recursion is bounded by NamespaceMaxExpandDepth (0 disables), which
// prevents a fatal Go stack overflow on deeply nested syntax.
// Errors are wrapped with phase context ("expansion" or "compilation"); callers
// may add site-specific context on top.
//
// The returned template is NOT peephole-optimized; closure bodies inside it are.
// See ExpandAndCompileOptimized for when the top-level pass pays.
func ExpandAndCompile(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue) (*machine.NativeTemplate, error) {
	return expandAndCompileScoped(ctx, env, stx, nil, NamespaceInlineThreshold(env), NamespaceMaxExpandDepth(env), nil)
}

// ExpandAndCompileOptimized is ExpandAndCompile followed by the peephole pass on
// the top-level template. Closure bodies are already optimized as they compile,
// so the pass only buys the top-level template's own fusions. Callers: the
// Engine's compile funnel (shared by Eval and Compile, whose CompiledCode can be
// Run repeatedly) and the compile primitive, whose thunk runs the template on
// every call; bootstrap loading takes the same pass through runBootstrapStx. The
// eval and load primitives run each template once and discard it, so they use
// ExpandAndCompile and skip the pass's allocations.
func ExpandAndCompileOptimized(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue) (*machine.NativeTemplate, error) {
	tpl, err := ExpandAndCompile(ctx, env, stx)
	if err != nil {
		return nil, err
	}
	tpl.Optimize()
	return tpl, nil
}

// expandAndCompileScoped is ExpandAndCompile with a library scope set on both
// halves of the pipeline. A transformer right-hand side compiled inside a
// define-library body needs it: the expander threads it to nested define-syntax
// (expander_body.go) and the compiler to CompileSyntaxRulesExpr and, from P0.3,
// CompileQuoteSyntax. nil is the ordinary top-level case.
//
// resolver, inlineThreshold and maxExpandDepth exist for the bootstrap pass
// alone: it compiles core sources through an embed resolver of their own and at
// the package defaults, without touching env's namespace, which a library env
// shares with the running engine. A nil resolver leaves the compiler on
// env.FileResolver(); every other caller passes the namespace readers.
func expandAndCompileScoped(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue, resolver FileResolver, inlineThreshold int, maxExpandDepth int, libraryScope *syntax.Scope) (*machine.NativeTemplate, error) {
	evaluator := machine.NewVMMacroEvaluator()

	expander := NewExpanderTimeContinuation(ctx, env, evaluator)
	expander.libraryScope = libraryScope
	expander.SetMaxDepth(maxExpandDepth)
	expanded, err := expander.ExpandTopLevelExpression(stx)
	if err != nil {
		return nil, wrapSourcedError(stx.SourceContext(), werr.WrapForeignErrorf(err, "expansion"))
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := NewCompileTimeCallContext(ctx, false)
	compiler := NewCompileTimeContinuation(tpl, env, evaluator)
	compiler.libraryScope = libraryScope
	if resolver != nil {
		compiler.fileResolver = resolver
	}
	compiler.SetInlineThreshold(inlineThreshold)
	err = compiler.CompileExpression(cctx, expanded)
	if err != nil {
		return nil, wrapSourcedError(stx.SourceContext(), werr.WrapForeignErrorf(err, "compilation"))
	}

	return tpl, nil
}
