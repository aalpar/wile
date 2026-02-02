// Copyright 2025 Aaron Alpar
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

package machine

import (
	"context"
	"errors"
	"fmt"

	"github.com/aalpar/wile/go/values"
)

type OperationApply struct{}

func NewOperationApply() *OperationApply {
	return &OperationApply{}
}

func (p *OperationApply) SchemeString() string {
	return "#<operation-apply>"
}

func (p *OperationApply) IsVoid() bool {
	return p == nil
}

func (p *OperationApply) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationApply)
	return sameType(p, v, ok)
}

// FIXME: needs unit tests
func (p *OperationApply) Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error) {
	vs := mc.evals.PopAll()
	switch cls := mc.value[0].(type) {
	case *MachineClosure:
		return mc.Apply(cls, vs...)
	case *CaseLambdaClosure:
		return mc.ApplyCaseLambda(cls, vs...)
	case *Parameter:
		return applyParameter(ctx, mc, cls, vs)
	case *ComposableContinuation:
		return applyComposableContinuation(mc, cls, vs)
	default:
		err := mc.Error(fmt.Sprintf("expected a closure, got %s", mc.value[0].SchemeString()))
		return mc, err
	}
}

// applyParameter handles calling a parameter object.
// With 0 args: returns the current value.
// With 1 arg: sets the value (after applying converter if present).
// After setting the return value, we restore the saved continuation to return
// to the caller, just like a closure's RestoreContinuation would do.
func applyParameter(ctx context.Context, mc *MachineContext, param *Parameter, args []values.Value) (*MachineContext, error) {
	switch len(args) {
	case 0:
		// Get: return current value
		mc.SetValue(param.Value())
		// Restore continuation to return to caller (like RestoreContinuation)
		if mc.cont != nil {
			mc.Restore(mc.cont)
		} else {
			// Top-level with no saved continuation - just halt
			return mc, ErrMachineHalt
		}
		return mc, nil

	case 1:
		// Set: apply converter if present, then set value
		newVal := args[0]

		if param.HasConverter() {
			// Apply the converter using a sub-context
			converter := param.Converter()
			sub := mc.NewSubContext()
			_, err := sub.Apply(converter, newVal)
			if err != nil {
				wrapErr := mc.WrapError(err, "parameter: failed to apply converter")
				return mc, wrapErr
			}
			err = sub.Run()
			if err != nil {
				if !errors.Is(err, ErrMachineHalt) {
					wrapErr := mc.WrapError(err, "parameter: converter error")
					return mc, wrapErr
				}
			}
			newVal = sub.GetValue()
		}

		param.SetValue(newVal)
		mc.SetValue(values.Void)
		// Restore continuation to return to caller (like RestoreContinuation)
		if mc.cont != nil {
			mc.Restore(mc.cont)
		} else {
			// Top-level with no saved continuation - just halt
			return mc, ErrMachineHalt
		}
		return mc, nil

	default:
		err := mc.Error(fmt.Sprintf("parameter: expected 0 or 1 arguments, got %d", len(args)))
		return mc, err
	}
}

// applyComposableContinuation applies a composable continuation by splicing
// its captured frames onto the current continuation chain. The continuation
// is deep-copied for safe re-invocation.
//
// See: Flatt, Yu, Findler, Felleisen "Adding Delimited and Composable Control
// to a Production Programming Environment" (ICFP 2007).
func applyComposableContinuation(mc *MachineContext, cc *ComposableContinuation, args []values.Value) (*MachineContext, error) {
	if len(args) != 1 {
		err := mc.Error(fmt.Sprintf("composable continuation: expected 1 argument, got %d", len(args)))
		return mc, err
	}

	// Deep-copy the segment for safe re-invocation
	segment := cc.Cont().DeepCopy()

	// Graft the segment's bottom frame onto the current continuation chain
	GraftContinuation(segment, mc.cont)

	// Handle dynamic-wind: unwind current extents not in captured stack,
	// rewind captured extents not in current stack.
	err := mc.RestoreWithWindingFrom(nil, mc.windingStack, cc.WindingStack())
	if err != nil {
		return mc, err
	}

	// Restore from the top of the segment (resume captured computation)
	mc.Restore(segment)
	mc.SetValue(args[0])
	return mc, nil
}
