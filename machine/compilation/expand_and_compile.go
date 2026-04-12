package compilation

import (
	"context"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/machine"
)

// ExpandAndCompile runs the expand+compile pipeline on a single syntax value,
// returning a ready-to-execute template.
//
// A single MacroEvaluator is shared across both phases. If resolver is non-nil,
// it is set on the compiler for include/load file resolution. inlineThreshold
// controls procedure inlining (0 disables). Callers own error wrapping and may
// call tpl.Optimize() on the returned template if desired.
func ExpandAndCompile(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue, resolver FileResolver, inlineThreshold int) (*machine.NativeTemplate, error) {
	evaluator := machine.NewVMMacroEvaluator()

	expanded, err := NewExpanderTimeContinuation(ctx, env, evaluator).ExpandExpression(stx)
	if err != nil {
		return nil, err
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
		return nil, err
	}

	return tpl, nil
}
