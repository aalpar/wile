package primitives

import (
	"context"
	"errors"

	"skeme/environment"
	"skeme/machine"
	"skeme/syntax"
	"skeme/utils"
	"skeme/values"
)

// PrimEval implements the (eval) primitive.
// Evaluates an expression in a given environment.
func PrimEval(ctx context.Context, mc *machine.MachineContext) error {
	expr := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
	envSpec := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value()

	// Get the environment frame from the SchemeEnvironment
	schemeEnv, ok := envSpec.(*values.SchemeEnvironment)
	if !ok {
		return values.NewForeignErrorf("eval: expected an environment specifier but got %T", envSpec)
	}

	env, ok := schemeEnv.Frame.(*environment.EnvironmentFrame)
	if !ok {
		return values.NewForeignError("eval: environment frame is invalid")
	}

	// Convert datum to syntax value
	sctx := syntax.NewZeroValueSourceContext()
	stx := utils.DatumToSyntaxValue(sctx, expr)

	// Expand the expression
	ectx := machine.NewExpandTimeCallContext()
	expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	if err != nil {
		return values.WrapForeignErrorf(err, "eval: expansion error")
	}

	// Compile the expression
	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	if err != nil {
		return values.WrapForeignErrorf(err, "eval: compilation error")
	}

	// Run the compiled code in a sub-context
	cont := machine.NewMachineContinuation(nil, tpl, env)
	sub := machine.NewMachineContext(cont)
	if err := sub.Run(ctx); err != nil {
		var escapeErr *machine.ErrContinuationEscape
		if errors.As(err, &escapeErr) {
			return err
		}
		if !errors.Is(err, machine.ErrMachineHalt) {
			return err
		}
	}

	mc.SetValue(sub.GetValue())
	return nil
}
