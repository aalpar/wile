package machine

import "wile/environment"

func NewForeignClosure(env *environment.EnvironmentFrame, pcnt int, vardiac bool, fn ForeignFunction) *MachineClosure {
	tpl := NewNativeTemplate(pcnt, 0, vardiac,
		NewOperationForeignFunctionCall(fn),
		NewOperationRestoreContinuation(),
	)
	lenv := environment.NewLocalEnvironment(pcnt)
	env = environment.NewEnvironmentFrameWithParent(lenv, env)
	q := NewClosureWithTemplate(tpl, env)
	return q
}

// NewForeignClosureV2 creates a closure from a ForeignFunctionV2 (trampolining model).
// This enables proper call/cc support by returning VMActions instead of directly modifying state.
func NewForeignClosureV2(env *environment.EnvironmentFrame, pcnt int, variadic bool, fn ForeignFunctionV2) *MachineClosure {
	tpl := NewNativeTemplate(pcnt, 0, variadic,
		NewOperationForeignFunctionCallV2(fn),
		NewOperationRestoreContinuation(),
	)
	lenv := environment.NewLocalEnvironment(pcnt)
	env = environment.NewEnvironmentFrameWithParent(lenv, env)
	return NewClosureWithTemplate(tpl, env)
}
