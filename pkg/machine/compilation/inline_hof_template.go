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

// inlineHOFTemplateSource maps each curated tail HOF to a Scheme template-lambda
// whose body is the HOF's single-list reclaiming loop, callback as the first
// parameter. Each is a transcription of the HOF's single-list clause from
// registry/core/bootstrap_procedures.scm (for-each ≈ line 122); the inline-vs-real
// equivalence test guards drift. v1 = for-each (P3); widened in P6.
//
// Built once per Namespace (BuildInlineHOFTemplates) through the real expander +
// validator, so the body's free identifiers (car/cdr/null?) carry definition-env
// hygiene and resolve to the sealed-base globals even when a call site shadows
// them locally. Inlined via the synthetic-let substrate (tryInlineHOFCall).
var inlineHOFTemplateSource = map[string]string{
	"for-each": `(lambda (f lst)
  (let loop ((lst lst))
    (if (null? lst) (if #f #f)
        (begin (f (car lst)) (loop (cdr lst))))))`,
}

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
		templates: make(map[string]*validate.ValidatedLambda, len(inlineHOFTemplateSource)),
	}
	for name, src := range inlineHOFTemplateSource {
		lam, err := buildInlineHOFTemplate(ctx, env, src)
		if err != nil {
			return werr.WrapForeignErrorf(err, "build inline-HOF template %q", name)
		}
		reg.templates[name] = lam
	}
	ns.SetInlineHOFTemplates(reg)
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
