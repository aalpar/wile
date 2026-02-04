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
	"testing"

	"github.com/aalpar/wile/go/environment"
	"github.com/aalpar/wile/go/values"

	qt "github.com/frankban/quicktest"
)

// --- Parameter ---

func TestParameter_ValueAndSetValue(t *testing.T) {
	c := qt.New(t)
	p := NewParameter(values.NewInteger(42), nil)
	c.Assert(p.Value(), values.SchemeEquals, values.NewInteger(42))

	p.SetValue(values.NewString("hello"))
	c.Assert(p.Value(), values.SchemeEquals, values.NewString("hello"))
}

func TestParameter_Converter(t *testing.T) {
	c := qt.New(t)

	p := NewParameter(values.NewInteger(1), nil)
	c.Assert(p.HasConverter(), qt.IsFalse)
	c.Assert(p.Converter(), qt.IsNil)

	env := environment.NewTopLevelEnvironment().Runtime()
	conv := NewForeignClosure(env, 1, false, func(_ context.Context, mc *MachineContext) error {
		mc.SetValue(values.Void)
		return nil
	})
	p2 := NewParameter(values.NewInteger(1), conv)
	c.Assert(p2.HasConverter(), qt.IsTrue)
	c.Assert(p2.Converter(), qt.Equals, conv)
}

func TestParameter_SchemeString(t *testing.T) {
	p := NewParameter(values.NewInteger(1), nil)
	qt.Assert(t, p.SchemeString(), qt.Equals, "#<parameter>")
}

func TestParameter_IsVoid(t *testing.T) {
	c := qt.New(t)
	p := NewParameter(values.NewInteger(1), nil)
	c.Assert(p.IsVoid(), qt.IsFalse)

	var nilParam *Parameter
	c.Assert(nilParam.IsVoid(), qt.IsTrue)
}

func TestParameter_EqualTo(t *testing.T) {
	c := qt.New(t)
	p1 := NewParameter(values.NewInteger(1), nil)
	p2 := NewParameter(values.NewInteger(1), nil)

	c.Assert(p1.EqualTo(p1), qt.IsTrue)
	c.Assert(p1.EqualTo(p2), qt.IsFalse) // identity, not structural
	c.Assert(p1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// --- PromptTag ---

func TestPromptTag_SchemeString(t *testing.T) {
	c := qt.New(t)
	named := NewPromptTag("test")
	c.Assert(named.SchemeString(), qt.Matches, `#<continuation-prompt-tag:test>`)

	anon := NewPromptTag("")
	c.Assert(anon.SchemeString(), qt.Matches, `#<continuation-prompt-tag:\d+>`)
}

func TestPromptTag_IsVoid(t *testing.T) {
	c := qt.New(t)
	tag := NewPromptTag("x")
	c.Assert(tag.IsVoid(), qt.IsFalse)

	var nilTag *PromptTag
	c.Assert(nilTag.IsVoid(), qt.IsTrue)
}

func TestPromptTag_EqualTo(t *testing.T) {
	c := qt.New(t)
	t1 := NewPromptTag("a")
	t2 := NewPromptTag("a")

	c.Assert(t1.EqualTo(t1), qt.IsTrue)
	c.Assert(t1.EqualTo(t2), qt.IsFalse) // identity, not name
	c.Assert(t1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// --- ErrPromptAbort ---

func TestErrPromptAbort_Error(t *testing.T) {
	tag := NewPromptTag("my-tag")
	err := &ErrPromptAbort{Tag: tag, Values: nil}
	qt.Assert(t, err.Error(), qt.Equals, "abort to prompt #<continuation-prompt-tag:my-tag>")
}

// --- ComposableContinuation ---

func TestComposableContinuation_Accessors(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	ws := WindingStack{NewDynamicWindFrame(nil, nil)}

	cc := NewComposableContinuation(cont, ws)
	c.Assert(cc.Cont(), qt.Equals, cont)
	c.Assert(cc.WindingStack(), qt.HasLen, 1)
}

func TestComposableContinuation_SchemeString(t *testing.T) {
	cc := NewComposableContinuation(nil, nil)
	qt.Assert(t, cc.SchemeString(), qt.Equals, "#<composable-continuation>")
}

func TestComposableContinuation_IsVoid(t *testing.T) {
	c := qt.New(t)
	cc := NewComposableContinuation(nil, nil)
	c.Assert(cc.IsVoid(), qt.IsFalse)

	var nilCC *ComposableContinuation
	c.Assert(nilCC.IsVoid(), qt.IsTrue)
}

func TestComposableContinuation_EqualTo(t *testing.T) {
	c := qt.New(t)
	cc1 := NewComposableContinuation(nil, nil)
	cc2 := NewComposableContinuation(nil, nil)

	c.Assert(cc1.EqualTo(cc1), qt.IsTrue)
	c.Assert(cc1.EqualTo(cc2), qt.IsFalse)
	c.Assert(cc1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// --- PrimitiveExpander ---

func TestPrimitiveExpander_Name(t *testing.T) {
	pe := NewPrimitiveExpander("quote", nil)
	qt.Assert(t, pe.Name(), qt.Equals, "quote")
}

func TestPrimitiveExpander_SchemeString(t *testing.T) {
	pe := NewPrimitiveExpander("define", nil)
	qt.Assert(t, pe.SchemeString(), qt.Equals, "#<primitive-expander:define>")
}

func TestPrimitiveExpander_IsVoid(t *testing.T) {
	pe := NewPrimitiveExpander("x", nil)
	qt.Assert(t, pe.IsVoid(), qt.IsFalse)
}

func TestPrimitiveExpander_EqualTo(t *testing.T) {
	c := qt.New(t)
	pe1 := NewPrimitiveExpander("if", nil)
	pe2 := NewPrimitiveExpander("if", nil)
	pe3 := NewPrimitiveExpander("begin", nil)

	c.Assert(pe1.EqualTo(pe2), qt.IsTrue) // same name
	c.Assert(pe1.EqualTo(pe3), qt.IsFalse)
	c.Assert(pe1.EqualTo(values.NewInteger(1)), qt.IsFalse)
	c.Assert(pe1.EqualTo(nil), qt.IsFalse)
}

// --- OperationPushWind / OperationPopWind ---

func TestOperationPushWind_Boilerplate(t *testing.T) {
	c := qt.New(t)
	op := NewOperationPushWind()
	c.Assert(op.SchemeString(), qt.Equals, "#<machine-operation-push-wind>")
	c.Assert(op.IsVoid(), qt.IsFalse)
	c.Assert(op.EqualTo(NewOperationPushWind()), qt.IsTrue)
	c.Assert(op.EqualTo(values.NewInteger(1)), qt.IsFalse)

	var nilOp *OperationPushWind
	c.Assert(nilOp.IsVoid(), qt.IsTrue)
}

func TestOperationPopWind_Boilerplate(t *testing.T) {
	c := qt.New(t)
	op := NewOperationPopWind()
	c.Assert(op.SchemeString(), qt.Equals, "#<machine-operation-pop-wind>")
	c.Assert(op.IsVoid(), qt.IsFalse)
	c.Assert(op.EqualTo(NewOperationPopWind()), qt.IsTrue)
	c.Assert(op.EqualTo(values.NewInteger(1)), qt.IsFalse)

	var nilOp *OperationPopWind
	c.Assert(nilOp.IsVoid(), qt.IsTrue)
}

// --- NativeTemplate ---

func TestNativeTemplate_SourceMap(t *testing.T) {
	tpl := NewNativeTemplate(0, 0, false)
	qt.Assert(t, tpl.SourceMap(), qt.IsNotNil)
}

// --- Operations ---

func TestOperations_AsList(t *testing.T) {
	c := qt.New(t)

	// Empty operations
	var empty Operations
	c.Assert(empty.AsList(), qt.IsNil)

	// Non-empty operations
	ops := Operations{NewOperationPush(), NewOperationPop()}
	result := ops.AsList()
	c.Assert(result, qt.IsNotNil)
}

// --- DynamicWindFrame ---

func TestWindingStack_Depth(t *testing.T) {
	c := qt.New(t)

	var empty WindingStack
	c.Assert(empty.Depth(), qt.Equals, 0)

	ws := WindingStack{NewDynamicWindFrame(nil, nil)}
	c.Assert(ws.Depth(), qt.Equals, 1)

	ws = append(ws, NewDynamicWindFrame(nil, nil))
	c.Assert(ws.Depth(), qt.Equals, 2)
}

// --- MachineContext accessors ---

func TestMachineContext_ParentMC(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	c.Assert(mc.ParentMC(), qt.IsNil)
}

func TestMachineContext_EscapeCont(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	c.Assert(mc.EscapeCont(), qt.IsNil)

	mc.SetEscapeCont(cont)
	c.Assert(mc.EscapeCont(), qt.Equals, cont)
}

func TestMachineContext_SetPC(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	mc.SetPC(42)
	qt.Assert(t, mc.PC(), qt.Equals, 42)
}

func TestMachineContext_Context(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	ctx := context.Background()
	mc := NewMachineContext(ctx, cont)

	c.Assert(mc.Context(), qt.Equals, ctx)

	ctx2, cancel := context.WithCancel(ctx)
	defer cancel()
	mc.SetContext(ctx2)
	c.Assert(mc.Context(), qt.Equals, ctx2)
}

func TestMachineContext_ExpanderContext(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	// expanderCtx is nil by default
	qt.Assert(t, mc.ExpanderContext(), qt.IsNil)
}

func TestMachineContext_ExceptionHandler(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	c.Assert(mc.ExceptionHandler(), qt.IsNil)

	h := NewExceptionHandler(values.NewString("handler"), nil)
	mc.SetExceptionHandler(h)
	c.Assert(mc.ExceptionHandler(), qt.Equals, h)
}

func TestMachineContext_PushPopExceptionHandler(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	// Pop from empty returns nil
	c.Assert(mc.PopExceptionHandler(), qt.IsNil)

	// Push two handlers
	mc.PushExceptionHandler(values.NewString("h1"))
	mc.PushExceptionHandler(values.NewString("h2"))

	// Pop returns most recent first
	h2 := mc.PopExceptionHandler()
	c.Assert(h2, qt.IsNotNil)
	c.Assert(h2.Handler(), values.SchemeEquals, values.NewString("h2"))

	h1 := mc.PopExceptionHandler()
	c.Assert(h1, qt.IsNotNil)
	c.Assert(h1.Handler(), values.SchemeEquals, values.NewString("h1"))

	c.Assert(mc.PopExceptionHandler(), qt.IsNil)
}

func TestMachineContext_WindingStack(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	c.Assert(mc.WindingStack(), qt.HasLen, 0)

	ws := WindingStack{NewDynamicWindFrame(nil, nil)}
	mc.SetWindingStack(ws)
	c.Assert(mc.WindingStack(), qt.HasLen, 1)
}

func TestMachineContext_PromptTag(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)
	mc := NewMachineContext(context.Background(), cont)

	c.Assert(mc.PromptTag(), qt.IsNil)

	tag := NewPromptTag("test")
	mc.SetPromptTag(tag)
	c.Assert(mc.PromptTag(), qt.Equals, tag)
}

// --- MachineContinuation prompt methods ---

func TestMachineContinuation_PromptMethods(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)

	cont := NewMachineContinuation(nil, tpl, env)
	c.Assert(cont.PromptTag(), qt.IsNil)
	c.Assert(cont.PromptHandler(), qt.IsNil)

	tag := NewPromptTag("p")
	cont.SetPromptTag(tag)
	c.Assert(cont.PromptTag(), qt.Equals, tag)

	handler := NewForeignClosure(env, 0, false, func(_ context.Context, mc *MachineContext) error {
		return nil
	})
	cont.SetPromptHandler(handler)
	c.Assert(cont.PromptHandler(), qt.Equals, handler)
}

func TestNewMachineContinuationWithPrompt(t *testing.T) {
	c := qt.New(t)
	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	tag := NewPromptTag("p")
	handler := NewForeignClosure(env, 0, false, func(_ context.Context, mc *MachineContext) error {
		return nil
	})

	cont := NewMachineContinuationWithPrompt(nil, tpl, env, tag, handler)
	c.Assert(cont.PromptTag(), qt.Equals, tag)
	c.Assert(cont.PromptHandler(), qt.Equals, handler)
}
