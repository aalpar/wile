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
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// exceptionHandlerParam is the canonical parameter that carries the current
// exception-handler stack (an immutable Scheme list, innermost handler first).
//
// It is a process-wide singleton used purely as a continuation-mark KEY (by
// pointer identity). The actual handler stacks live in per-MachineContext marks,
// so sharing one key object across Engines is safe: each Engine's handlers ride
// its own continuation chain and resolve only within it. The base value (empty
// list) is immutable. This mirrors the process-wide DefaultPromptTag.
//
// Scheme reaches it through the global %exception-handlers binding (bound to this
// object by the core registry's %exception-handler-parameter primitive); Go
// reaches it directly. with-exception-handler is (parameterize ((p (cons H (p))))
// …), which sets a continuation mark keyed by this object — so call/cc copies the
// handler with the continuation, and re-entry restores it. That is the whole fix:
// the handler rides the captured continuation instead of an off-chain Go field.
var exceptionHandlerParam = NewParameter(values.EmptyList, nil)

// ExceptionHandlerParam returns the canonical exception-handler parameter so the core
// registry can bind it to the Scheme global %exception-handlers. The return type is
// values.Value, not *Parameter: callers only need to bind the object, and narrowing
// keeps the mutating Parameter.SetValue out of reach so the shared singleton's base
// value (the immutability the cross-Engine safety argument rests on) can't be
// rewritten through this accessor. RaiseInPlace uses the package var directly.
func ExceptionHandlerParam() values.Value {
	return exceptionHandlerParam
}

// RaiseInPlace invokes the current exception handler on cond, in the dynamic
// extent of the raise (R7RS §6.11), with the parent handler installed as current.
// It is the single implementation shared by raise, raise-continuable, error, and
// the Go-error bridge (applyCallableError).
//
//   - continuable=false (raise / error / Go errors): the handler is expected to
//     escape (e.g. via guard's captured continuation); if it returns, a secondary
//     non-continuable exception escalates to the parent handler. The bridge only
//     ever calls with continuable=false, so it never needs an in-place resume.
//   - continuable=true (raise-continuable): the handler's return value becomes the
//     value of the raise-continuable expression, resumed inline on mc.
//
// When the handler stack is empty the condition is uncaught: a slim
// *ErrExceptionEscape carrier bubbles to RunWithEscapeHandling and surfaces to the
// embedder (engine.wrapRuntimeError) with the condition, source, and stack trace.
func RaiseInPlace(mc *MachineContext, cond values.Value, continuable bool) error {
	handlers := mc.ResolveParameterValue(exceptionHandlerParam)
	return mc.raiseToHandlers(cond, continuable, handlers)
}

// raiseToHandlers runs the FIRST handler of `handlers` on cond, escalating to the
// rest (its cdr) on a non-continuable normal return. handlers is passed explicitly so
// the non-continuable escalation targets the parent list directly rather than
// re-resolving from marks (the handler ran with the parent mark, but the finalizer
// frame that escalates runs after the handler's marks are gone).
func (mc *MachineContext) raiseToHandlers(cond values.Value, continuable bool, handlers values.Value) error {
	source := mc.CurrentSource()
	trace := mc.CaptureStackTrace(defaultBacktraceDepth)
	enrichNativeError(cond, source, trace)

	pair, ok := handlers.(*values.Pair)
	if !ok {
		// Empty handler stack: uncaught exception. Carry it to the embedder.
		return &ErrExceptionEscape{Condition: cond, Source: source, StackTrace: trace}
	}
	handler := pair.Car()
	parent := pair.Cdr()

	// Install the parent handler as current and the raise-site diagnostics on the LIVE
	// activation (SetMark updates the marks in place; the Go equivalent of
	// (parameterize ((exc-param parent)) …)). This makes a re-raise inside the handler
	// escalate, and rides the marks into a continuation captured inside the handler.
	mc.SetMark(exceptionHandlerParam, parent)
	mc.SetMark(ErrorContextKey(), NewErrorContext(source, trace, nil))

	// Run the handler INLINE on mc — NOT in a sub-context. This is load-bearing for
	// nested guard under the resume flip: guard's handler captures handler-k via
	// call/cc, and that continuation must span the live chain INCLUDING the
	// chain-resident exit/prompt frames ABOVE the raise. A sub-context handler captures
	// only the sub's chain, so a reinstated handler-k would not restore the outer
	// boundaries and an escalating re-raise's abort to an outer exit tag would find "no
	// prompt found for tag exit". Inline, handler-k spans mc.cont, so the boundaries
	// travel in the captured segment and the escalation resolves them.
	if continuable {
		// raise-continuable: the handler's return value(s) become the value(s) of the
		// raise-continuable expression (R7RS §6.11), delivered inline at the raise site
		// by the transparent frame (returnTemplate passes the value register through).
		frame := NewMachineContinuation(mc.cont, returnTemplate, mc.env)
		_, err := mc.RunBodyUnderFrame(frame, handler, cond)
		return err
	}

	// Non-continuable (raise / error / Go-error bridge): the handler is expected to
	// escape (guard escapes via guard-k). If it RETURNS normally, the finalizer frame
	// escalates a secondary non-continuable exception to the parent handlers — directly
	// (not re-resolved from marks). Each escalation reads a shorter handler list,
	// bottoming out at the empty-stack uncaught carrier above. The frame's
	// applyToValuesCode template applies the escalator to (and discards) the handler's
	// return value(s).
	//
	// EXCEPTION: a guard whose clauses miss re-raises via raise-continuable, which
	// RESUMES this handler's captured handler-k continuation — and that continuation
	// now spans this finalizer frame (the handler runs inline so handler-k captures
	// the live chain, which is what makes nested guard work). So the continuable
	// re-raise's result flows back THROUGH this frame. Detect that by isolatedMarks
	// (set only while a call/cc continuation is being resumed) and FORWARD the value
	// instead of escalating: the handler did not return naturally, it was resumed with
	// a continuable value.
	escalateFn := func(finCC CallContext) error {
		finMC, err := RequireMachineContext(finCC, "raise")
		if err != nil {
			return err
		}
		if finMC.isolatedMarks {
			var vals []values.Value
			current := finMC.Arg(0)
			for !values.IsEmptyList(current) {
				tuple, ok := current.(values.Tuple)
				if !ok {
					return werr.WrapForeignErrorf(werr.ErrNotAList,
						"raise: improper handler-result list")
				}
				vals = append(vals, tuple.Car())
				current = tuple.Cdr()
			}
			finMC.SetValues(vals...)
			return nil
		}
		secondary := values.NewErrorObjectWithCause(
			"exception handler returned from non-continuable exception",
			werr.ErrNonContinuableException,
		)
		return finMC.raiseToHandlers(secondary, false, parent)
	}
	// Use a runtime env reachable from here, falling back to the live frame: when the
	// raise fires inside a reified call-with-values producer the live procedure frame
	// is detached (nil namespace), so MutableRuntime() would panic. The escalator is a
	// pure-Go closure; any valid frame serves for its apply-time InitApplyFrame.
	escalatorEnv := mc.EnvironmentFrame().MutableRuntimeOrNil()
	if escalatorEnv == nil {
		escalatorEnv = mc.EnvironmentFrame()
	}
	escalator := NewForeignClosure(escalatorEnv, 1, true, escalateFn)
	escalateTpl := &NativeTemplate{
		code:     applyToValuesCode,
		literals: MultipleValues{escalator},
	}
	frame := NewMachineContinuation(mc.cont, escalateTpl, mc.env)
	_, err := mc.RunBodyUnderFrame(frame, handler, cond)
	return err
}

// enrichNativeError stamps a freshly-raised NativeError with the raise-site source
// location and stack trace, so error-object-source / error-object-stack-trace can
// report them. It enriches only once — a re-raise preserves the original raise site
// (guarded by SourceLocation() == "").
func enrichNativeError(cond values.Value, source *syntax.SourceContext, trace StackTrace) {
	ne, ok := cond.(*values.NativeError)
	if !ok || ne.SourceLocation() != "" {
		return
	}
	ne.SetSourceLocation(source.Location())
	ne.SetStackTraceValue(StackTraceToSchemeList(trace))
}

// StackTraceToSchemeList converts a StackTrace to a Scheme list of alists, one per
// frame. Each alist always carries name; file, line, and column are present only
// when source information is available. Lives here (not in the registry) because it
// converts machine types and is needed by both RaiseInPlace and the error-context
// primitives.
func StackTraceToSchemeList(st StackTrace) values.Tuple {
	if len(st) == 0 {
		return values.EmptyList
	}
	frames := make([]values.Value, len(st))
	for i, frame := range st {
		frames[i] = stackFrameToAlist(frame)
	}
	return values.List(frames...)
}

// stackFrameToAlist converts a single StackFrame to a Scheme alist with keys
// name, file, line, column. Example:
// ((name . "f") (file . "test.scm") (line . 10) (column . 5))
func stackFrameToAlist(frame StackFrame) values.Tuple {
	name := frame.FunctionName
	if name == "" {
		name = "<anonymous>"
	}

	// Prefer CurrentLoc over CallSite for source info.
	src := frame.CurrentLoc
	if src == nil {
		src = frame.CallSite
	}

	nameEntry := values.NewCons(values.NewSymbol("name"), values.NewString(name))
	if src == nil {
		return values.List(nameEntry)
	}

	fileEntry := values.NewCons(values.NewSymbol("file"), values.NewString(src.File))
	lineEntry := values.NewCons(values.NewSymbol("line"), values.NewInteger(int64(src.Start.Line())))
	colEntry := values.NewCons(values.NewSymbol("column"), values.NewInteger(int64(src.Start.Column())))

	return values.List(nameEntry, fileEntry, lineEntry, colEntry)
}
