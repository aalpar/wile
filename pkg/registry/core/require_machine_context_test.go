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

package core_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/core"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// foreignCallContext is a machine.CallContext that is NOT a *machine.MachineContext.
// R8: primitives needing full VM internals must reject such a context with a
// wrapped werr.ErrNotAMachineContext (via machine.RequireMachineContext) instead
// of panicking on a bare `cc.(*machine.MachineContext)` type assertion. Arg never
// gets called by these primitives — the context check runs first — but it returns
// a safe value so the stub cannot panic incidentally.
type foreignCallContext struct{}

func (*foreignCallContext) Arg(int) values.Value                            { return values.Void }
func (*foreignCallContext) SetValue(values.Value)                           {}
func (*foreignCallContext) SetValues(...values.Value)                       {}
func (*foreignCallContext) Authorizer() security.Authorizer                 { return nil }
func (*foreignCallContext) Context() context.Context                        { return context.Background() }
func (*foreignCallContext) EnvironmentFrame() *environment.EnvironmentFrame { return nil }
func (*foreignCallContext) Thread() *values.Thread                          { return nil }

// Compile-time assertion: foreignCallContext satisfies CallContext without being
// the concrete *MachineContext the primitives need.
var _ machine.CallContext = (*foreignCallContext)(nil)

// TestPrimitivesRejectForeignCallContext exercises every core primitive that
// downcasts to *machine.MachineContext. Each must surface a wrapped
// ErrNotAMachineContext rather than panicking when handed a foreign context.
func TestPrimitivesRejectForeignCallContext(t *testing.T) {
	tcs := []struct {
		name string
		fn   func(machine.CallContext) error
	}{
		{"apply", core.PrimApply},
		{"call/cc", core.PrimCallCC},
		{"call-with-values", core.PrimCallWithValues},
		{"call-with-continuation-prompt", core.PrimCallWithContinuationPrompt},
		{"continuation-prompt-available?", core.PrimContinuationPromptAvailableQ},
		{"call-with-composable-continuation", core.PrimCallWithComposableContinuation},
		{"call-with-exit", core.PrimCallWithExit},
		{"current-continuation-marks", core.PrimCurrentContinuationMarks},
		{"call-with-immediate-continuation-mark", core.PrimCallWithImmediateContMark},
		{"make-parameter", core.PrimMakeParameter},
		{"%parameter-convert", core.PrimParameterConvert},
		{"raise", core.PrimRaise},
		{"raise-continuable", core.PrimRaiseContinuable},
		{"error", core.PrimError},
		{"with-timeout", core.PrimWithTimeout},
		{"call-with-continuation-barrier", core.PrimCallWithContinuationBarrier},
		{"current-error-context", core.PrimCurrentErrorContext},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := tc.fn(&foreignCallContext{})
			qt.Assert(t, err, qt.ErrorIs, werr.ErrNotAMachineContext)
		})
	}
}
