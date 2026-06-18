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

package machine

import (
	"context"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// MacroEvaluator abstracts VM execution for the compiler and expander so that
// compile-time evaluation and transformer invocation can be tested and wired
// without depending on the concrete MachineContext construction path.
type MacroEvaluator interface {
	// EvalTemplate evaluates a compiled template in the given environment
	// and returns the resulting value. Used by compileAndEvalLambdaTransformer
	// for define-syntax lambda transformers at compile time.
	EvalTemplate(ctx context.Context, tpl *NativeTemplate, env *environment.EnvironmentFrame) (values.Value, error)

	// InvokeTransformer calls a closure as a macro transformer.
	// expanderCtx is set on the VM context for auxiliary syntax
	// hygiene (R7RS Section 4.3.2); nil is valid when no hygiene context
	// is needed (e.g. ExpandOnce). args are passed to Apply — one arg for
	// syntax-rules/lambda transformers, three for ER macros (form, rename,
	// compare). On success, the caller receives the MachineContext with the
	// result in the value register and must call ReleaseSubContext when done.
	InvokeTransformer(ctx context.Context, cls Closure, expanderCtx ExpanderCtx, args ...values.Value) (*MachineContext, error)
}

// vmMacroEvaluator implements MacroEvaluator using the real VM execution path.
// Stateless — all call sites share a single instance via NewVMMacroEvaluator.
type vmMacroEvaluator struct{}

// sharedVMEvaluator is the singleton instance. vmMacroEvaluator is stateless,
// so a single allocation serves all callers.
var sharedVMEvaluator MacroEvaluator = &vmMacroEvaluator{}

// NewVMMacroEvaluator returns a MacroEvaluator backed by the real VM.
func NewVMMacroEvaluator() MacroEvaluator {
	return sharedVMEvaluator
}

func (p *vmMacroEvaluator) EvalTemplate(ctx context.Context, tpl *NativeTemplate, env *environment.EnvironmentFrame) (values.Value, error) {
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(ctx, cont)
	err := mc.Run()
	if err != nil {
		ReleaseSubContext(mc)
		return nil, err
	}
	q := mc.GetValue()
	ReleaseSubContext(mc)
	return q, nil
}

// InvokeTransformer delegates to invokeTransformerClosure.
func (p *vmMacroEvaluator) InvokeTransformer(ctx context.Context, cls Closure, expanderCtx ExpanderCtx, args ...values.Value) (*MachineContext, error) {
	return invokeTransformerClosure(ctx, cls, expanderCtx, args...)
}

// invokeTransformerClosure invokes a Closure (MachineClosure or ForeignClosure)
// as a macro transformer. If expanderCtx is non-nil, it is set on the context
// for auxiliary syntax hygiene (R7RS §4.3.2). args are passed to Apply — one
// arg for syntax-rules/lambda transformers, three for ER macros.
//
// On success, the caller receives the MachineContext with the result in the
// value register and must call ReleaseSubContext when done. On error, cleanup
// is handled internally.
func invokeTransformerClosure(ctx context.Context, cls Closure, expanderCtx ExpanderCtx, args ...values.Value) (*MachineContext, error) {
	var mc *MachineContext
	switch c := cls.(type) {
	case *MachineClosure:
		mc = acquireMacroContext(ctx, c)
		if expanderCtx != nil {
			mc.SetExpanderContext(expanderCtx)
		}
		_, err := mc.Apply(c, args...)
		if err != nil {
			ReleaseSubContext(mc)
			return nil, werr.WrapForeignErrorf(err, "failed to apply transformer")
		}
		err = mc.Run()
		if err != nil {
			ReleaseSubContext(mc)
			return nil, err
		}
	case *ForeignClosure:
		mc = acquireSubContext()
		mc.ctx = ctx
		mc.evals = acquireStack()
		if expanderCtx != nil {
			mc.SetExpanderContext(expanderCtx)
		}
		_, err := mc.applyForeign(c, args...)
		if err != nil {
			ReleaseSubContext(mc)
			return nil, werr.WrapForeignErrorf(err, "failed to apply transformer")
		}
	default:
		return nil, werr.WrapForeignErrorf(werr.ErrNotAClosure, "unexpected closure type: %T", cls)
	}
	return mc, nil
}
