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

package machine_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// --- machine.Parameter ---

func TestParameter_ValueAndSetValue(t *testing.T) {
	c := qt.New(t)
	p := machine.NewParameter(values.NewInteger(42), nil, machine.MutableBase)
	c.Assert(p.Value(), valuestest.SchemeEquals, values.NewInteger(42))

	p.SetValue(values.NewString("hello"))
	c.Assert(p.Value(), valuestest.SchemeEquals, values.NewString("hello"))
}

func TestParameter_Converter(t *testing.T) {
	c := qt.New(t)

	p := machine.NewParameter(values.NewInteger(1), nil, machine.MutableBase)
	c.Assert(p.HasConverter(), qt.IsFalse)
	c.Assert(p.Converter(), qt.IsNil)

	env := environment.NewNamespace().Runtime()
	conv := machine.NewForeignClosure(env, 1, false, func(mc machine.CallContext) error {
		mc.SetValue(values.Void)
		return nil
	})
	p2 := machine.NewParameter(values.NewInteger(1), conv, machine.MutableBase)
	c.Assert(p2.HasConverter(), qt.IsTrue)
	c.Assert(p2.Converter(), qt.Equals, conv)
}

func TestParameter_SchemeString(t *testing.T) {
	p := machine.NewParameter(values.NewInteger(1), nil, machine.MutableBase)
	qt.Assert(t, p.SchemeString(), qt.Equals, "#<parameter>")
}

func TestParameter_IsVoid(t *testing.T) {
	c := qt.New(t)
	p := machine.NewParameter(values.NewInteger(1), nil, machine.MutableBase)
	c.Assert(p.IsVoid(), qt.IsFalse)

	var nilParam *machine.Parameter
	c.Assert(nilParam.IsVoid(), qt.IsTrue)
}

func TestParameter_EqualTo(t *testing.T) {
	c := qt.New(t)
	p1 := machine.NewParameter(values.NewInteger(1), nil, machine.MutableBase)
	p2 := machine.NewParameter(values.NewInteger(1), nil, machine.MutableBase)

	c.Assert(p1.EqualTo(p1), qt.IsTrue)
	c.Assert(p1.EqualTo(p2), qt.IsFalse) // identity, not structural
	c.Assert(p1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// --- machine.PromptTag ---

func TestPromptTag_SchemeString(t *testing.T) {
	c := qt.New(t)
	named := machine.NewPromptTag("test")
	c.Assert(named.SchemeString(), qt.Matches, `#<continuation-prompt-tag:test>`)

	anon := machine.NewPromptTag("")
	c.Assert(anon.SchemeString(), qt.Matches, `#<continuation-prompt-tag:\d+>`)
}

func TestPromptTag_IsVoid(t *testing.T) {
	c := qt.New(t)
	tag := machine.NewPromptTag("x")
	c.Assert(tag.IsVoid(), qt.IsFalse)

	var nilTag *machine.PromptTag
	c.Assert(nilTag.IsVoid(), qt.IsTrue)
}

func TestPromptTag_EqualTo(t *testing.T) {
	c := qt.New(t)
	t1 := machine.NewPromptTag("a")
	t2 := machine.NewPromptTag("a")

	c.Assert(t1.EqualTo(t1), qt.IsTrue)
	c.Assert(t1.EqualTo(t2), qt.IsFalse) // identity, not name
	c.Assert(t1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// --- machine.ErrPromptAbort ---

func TestErrPromptAbort_Error(t *testing.T) {
	tag := machine.NewPromptTag("my-tag")
	err := &machine.ErrPromptAbort{Tag: tag, Values: nil}
	qt.Assert(t, err.Error(), qt.Equals, "abort to prompt #<continuation-prompt-tag:my-tag>")
}

// --- machine.ComposableContinuation ---

func TestComposableContinuation_Accessors(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ws := machine.WindingStack{machine.NewDynamicWindFrame(nil, nil)}

	cc := machine.NewComposableContinuation(cont, ws, 0, nil)
	c.Assert(cc.Cont(), qt.Equals, cont)
	c.Assert(cc.WindingStack(), qt.HasLen, 1)
}

func TestComposableContinuation_SchemeString(t *testing.T) {
	cc := machine.NewComposableContinuation(nil, nil, 0, nil)
	qt.Assert(t, cc.SchemeString(), qt.Equals, "#<composable-continuation>")
}

func TestComposableContinuation_IsVoid(t *testing.T) {
	c := qt.New(t)
	cc := machine.NewComposableContinuation(nil, nil, 0, nil)
	c.Assert(cc.IsVoid(), qt.IsFalse)

	var nilCC *machine.ComposableContinuation
	c.Assert(nilCC.IsVoid(), qt.IsTrue)
}

func TestComposableContinuation_EqualTo(t *testing.T) {
	c := qt.New(t)
	cc1 := machine.NewComposableContinuation(nil, nil, 0, nil)
	cc2 := machine.NewComposableContinuation(nil, nil, 0, nil)

	c.Assert(cc1.EqualTo(cc1), qt.IsTrue)
	c.Assert(cc1.EqualTo(cc2), qt.IsFalse)
	c.Assert(cc1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// --- PrimitiveExpander ---

func TestPrimitiveExpander_Name(t *testing.T) {
	pe := compilation.NewPrimitiveExpander("quote", nil)
	qt.Assert(t, pe.Name(), qt.Equals, "quote")
}

func TestPrimitiveExpander_SchemeString(t *testing.T) {
	pe := compilation.NewPrimitiveExpander("define", nil)
	qt.Assert(t, pe.SchemeString(), qt.Equals, "#<primitive-expander:define>")
}

func TestPrimitiveExpander_IsVoid(t *testing.T) {
	pe := compilation.NewPrimitiveExpander("x", nil)
	qt.Assert(t, pe.IsVoid(), qt.IsFalse)
}

func TestPrimitiveExpander_EqualTo(t *testing.T) {
	c := qt.New(t)
	pe1 := compilation.NewPrimitiveExpander("if", nil)
	pe2 := compilation.NewPrimitiveExpander("if", nil)
	pe3 := compilation.NewPrimitiveExpander("begin", nil)

	c.Assert(pe1.EqualTo(pe2), qt.IsTrue) // same name
	c.Assert(pe1.EqualTo(pe3), qt.IsFalse)
	c.Assert(pe1.EqualTo(values.NewInteger(1)), qt.IsFalse)
	c.Assert(pe1.EqualTo(nil), qt.IsFalse)
}

// --- OperationPushWind / OperationPopWind ---

func TestOperationPushWind_Boilerplate(t *testing.T) {
	c := qt.New(t)
	op := machine.NewOperationPushWind()
	c.Assert(op.SchemeString(), qt.Equals, "#<machine-operation-push-wind>")
	c.Assert(op.IsVoid(), qt.IsFalse)
	c.Assert(op.EqualTo(machine.NewOperationPushWind()), qt.IsTrue)
	c.Assert(op.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestOperationPopWind_Boilerplate(t *testing.T) {
	c := qt.New(t)
	op := machine.NewOperationPopWind()
	c.Assert(op.SchemeString(), qt.Equals, "#<machine-operation-pop-wind>")
	c.Assert(op.IsVoid(), qt.IsFalse)
	c.Assert(op.EqualTo(machine.NewOperationPopWind()), qt.IsTrue)
	c.Assert(op.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// --- machine.NativeTemplate ---

func TestNativeTemplate_SourceAt(t *testing.T) {
	tpl := machine.NewNativeTemplate(0, 0, false)
	qt.Assert(t, tpl.SourceAt(0), qt.IsNil)
}

// --- machine.Operations ---

func TestOperations_AsList(t *testing.T) {
	c := qt.New(t)

	// Empty operations
	var empty machine.Operations
	c.Assert(empty.AsList(), qt.IsNil)

	// Non-empty operations
	ops := machine.Operations{machine.NewOperationPush(), machine.NewOperationPop()}
	result := ops.AsList()
	c.Assert(result, qt.IsNotNil)
}

// --- machine.DynamicWindFrame ---

func TestWindingStack_Depth(t *testing.T) {
	c := qt.New(t)

	var empty machine.WindingStack
	c.Assert(empty.Depth(), qt.Equals, 0)

	ws := machine.WindingStack{machine.NewDynamicWindFrame(nil, nil)}
	c.Assert(ws.Depth(), qt.Equals, 1)

	ws = append(ws, machine.NewDynamicWindFrame(nil, nil))
	c.Assert(ws.Depth(), qt.Equals, 2)
}

// --- machine.MachineContext accessors ---

func TestMachineContext_ParentMC(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)

	c.Assert(mc.ParentMC(), qt.IsNil)
}

// TestNewThreadSubContext_SeversParentLink is the CI-safe regression guard for the
// SRFI-18 thread-terminate data race: a thread sub-context must NOT hold a live
// pointer to its concurrent parent (parentMC), or CaptureStackTrace /
// findParameterInMarks / the pool counter would read the parent's still-mutating
// VM fields across the goroutine boundary. (make ci does not run -race on the
// threads package, so this pins the fix without it.)
func TestNewThreadSubContext_SeversParentLink(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	parent := machine.NewMachineContext(context.Background(), cont)

	params := parent.CaptureSubContextParams()
	thread := values.NewThread(nil, "test")
	sub := machine.NewThreadSubContext(params, thread)

	c.Assert(sub.ParentMC(), qt.IsNil)
}

func TestMachineContext_EscapeCont(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)

	c.Assert(mc.EscapeCont(), qt.IsNil)

	mc.SetEscapeCont(cont)
	c.Assert(mc.EscapeCont(), qt.Equals, cont)
}

func TestMachineContext_SetPC(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)

	mc.SetPC(42)
	qt.Assert(t, mc.PC(), qt.Equals, 42)
}

func TestMachineContext_Context(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	ctx := context.Background()
	mc := machine.NewMachineContext(ctx, cont)

	c.Assert(mc.Context(), qt.Equals, ctx)

	ctx2, cancel := context.WithCancel(ctx)
	defer cancel()
	mc.SetContext(ctx2)
	c.Assert(mc.Context(), qt.Equals, ctx2)
}

func TestMachineContext_ExpanderContext(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)

	// expanderCtx is nil by default
	qt.Assert(t, mc.ExpanderContext(), qt.IsNil)
}

// (TestMachineContext_ExceptionHandler and _PushPopExceptionHandler removed in
// piece E: the exceptionHandler field and its Push/Pop/Get/Set methods are gone;
// handlers now ride the %exception-handlers parameter.)

func TestMachineContext_WindingStack(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)

	c.Assert(mc.WindingStack(), qt.HasLen, 0)

	mc.PushWindingFrame(machine.NewDynamicWindFrame(nil, nil))
	c.Assert(mc.WindingStack(), qt.HasLen, 1)
}

func TestMachineContext_PromptTag(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	cont := machine.NewMachineContinuation(nil, tpl, env)
	mc := machine.NewMachineContext(context.Background(), cont)

	c.Assert(mc.PromptTag(), qt.IsNil)

	tag := machine.NewPromptTag("test")
	mc.SetPromptTag(tag)
	c.Assert(mc.PromptTag(), qt.Equals, tag)
}

// --- machine.MachineContinuation prompt methods ---

func TestMachineContinuation_PromptMethods(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)

	cont := machine.NewMachineContinuation(nil, tpl, env)
	c.Assert(cont.PromptTag(), qt.IsNil)
	c.Assert(cont.PromptHandler(), qt.IsNil)

	tag := machine.NewPromptTag("p")
	cont.SetPromptTag(tag)
	c.Assert(cont.PromptTag(), qt.Equals, tag)

	handler := machine.NewForeignClosure(env, 0, false, func(mc machine.CallContext) error {
		return nil
	})
	cont.SetPromptHandler(handler)
	c.Assert(cont.PromptHandler(), qt.Equals, handler)
}

func TestNewMachineContinuationWithPrompt(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	tag := machine.NewPromptTag("p")
	handler := machine.NewForeignClosure(env, 0, false, func(mc machine.CallContext) error {
		return nil
	})

	cont := machine.NewMachineContinuationWithPrompt(nil, tpl, env, tag, handler)
	c.Assert(cont.PromptTag(), qt.Equals, tag)
	c.Assert(cont.PromptHandler(), qt.Equals, handler)
}
