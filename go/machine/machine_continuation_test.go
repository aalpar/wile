package machine

import (
	"context"
	"skeme/environment"
	"skeme/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestMachine_Operations(t *testing.T) {
	genv := environment.NewTopLevelGlobalEnvironment()
	lenv := environment.NewLocalEnvironment(0)
	env := environment.NewEnvironmentFrame(lenv, genv)
	tpl := NewNativeTemplate(0, 0, false, NewOperationPush())
	tpl.MaybeAppendLiteral(values.NewSymbol("foo"))
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))
	ctx := context.Background()
	err := mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
}
