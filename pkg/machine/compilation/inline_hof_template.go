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
	"strings"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/werr"
)

// The inline templates live on inlineHOFSpecs (inline_hof.go), the single source
// of truth that keeps each template adjacent to its callback index and load path.
// This file owns the build pipeline that turns those source strings into validated
// lambdas stored per Namespace.

// inlineHOFTemplateRegistry is the per-Namespace store of validated templates,
// implementing environment.InlineHOFTemplateStore (returning the template as any,
// since environment/ cannot name *validate.ValidatedLambda).
type inlineHOFTemplateRegistry struct {
	templates map[string]*validate.ValidatedLambda
}

// InlineHOFTemplate returns the validated template lambda for a HOF name.
func (p *inlineHOFTemplateRegistry) InlineHOFTemplate(name string) (any, bool) {
	lam, ok := p.templates[name]
	if !ok {
		return nil, false
	}
	return lam, true
}

// BuildInlineHOFTemplates parses, expands, and validates each curated HOF loop
// template against env (the sealed base) and stores the registry on env's
// Namespace. Idempotent: a no-op when a store is already installed (the root
// bootstrap builds it; flat library envs sharing the Namespace skip). A failure
// aborts engine init — the templates are fixed source, so it is a build bug.
func BuildInlineHOFTemplates(ctx context.Context, env *environment.EnvironmentFrame) error {
	ns := env.Namespace()
	if ns.InlineHOFTemplates() != nil {
		return nil
	}
	reg := &inlineHOFTemplateRegistry{
		templates: make(map[string]*validate.ValidatedLambda, len(inlineHOFSpecs)),
	}
	for name, spec := range inlineHOFSpecs {
		lam, err := buildInlineHOFTemplate(ctx, env, spec.template)
		if err != nil {
			return werr.WrapForeignErrorf(err, "build inline-HOF template %q", name)
		}
		verr := validateInlineHOFCallbackIndex(name, spec.callbackParam, len(lam.Params().Required))
		if verr != nil {
			return verr
		}
		reg.templates[name] = lam
	}
	ns.SetInlineHOFTemplates(reg)
	return nil
}

// validateInlineHOFCallbackIndex checks that callbackParam names a real parameter
// of a template with requiredCount required parameters. tryInlineHOFCall stamps
// the callbackParam-th synthetic-let binding CaptureSafe; if that index fell
// outside the template's parameters, the stamp would land on the wrong argument
// (or panic), so BuildInlineHOFTemplates enforces it at engine init (fail closed).
// The live specs never trip it — it guards a future mis-authored spec — so it is
// exercised by a unit test rather than at runtime.
func validateInlineHOFCallbackIndex(name string, callbackParam, requiredCount int) error {
	if callbackParam < 0 || callbackParam >= requiredCount {
		return werr.WrapForeignErrorf(werr.ErrEngineInit,
			"inline-HOF template %q: callback index %d out of range for %d required parameters",
			name, callbackParam, requiredCount)
	}
	return nil
}

// buildInlineHOFTemplate runs the front half of the compile pipeline — parse,
// expand (for hygiene), validate — on a template source string, returning the
// validated lambda. It stops before compilation: the template is compiled later,
// inlined at each call site via the synthetic-let substrate.
func buildInlineHOFTemplate(ctx context.Context, env *environment.EnvironmentFrame, src string) (*validate.ValidatedLambda, error) {
	pr := parser.NewParser(env, true, strings.NewReader(src))
	stx, err := pr.ReadSyntax(ctx)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "parse")
	}
	evaluator := machine.NewVMMacroEvaluator()
	expander := NewExpanderTimeContinuation(ctx, env, evaluator)
	expanded, err := expander.ExpandExpression(stx)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "expand")
	}
	result := validate.ValidateExpression(ctx, env, expanded)
	if !result.Ok() {
		return nil, werr.WrapForeignErrorf(werr.ErrEngineInit, "validate: %v", result.Errors)
	}
	lam, ok := result.Expr.(*validate.ValidatedLambda)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrEngineInit, "template did not validate to a lambda (got %T)", result.Expr)
	}
	return lam, nil
}
