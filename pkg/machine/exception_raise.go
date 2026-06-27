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
	source := mc.CurrentSource()
	trace := mc.CaptureStackTrace(defaultBacktraceDepth)
	enrichNativeError(cond, source, trace)

	handlers := mc.ResolveParameterValue(exceptionHandlerParam)
	pair, ok := handlers.(*values.Pair)
	if !ok {
		// Empty handler stack: uncaught exception. Carry it to the embedder.
		return &ErrExceptionEscape{Condition: cond, Source: source, StackTrace: trace}
	}
	handler := pair.Car()
	parent := pair.Cdr()

	// Run the handler in a sub-context with the parent handler installed as the
	// current one (so a re-raise inside the handler escalates) and the raise-site
	// diagnostics attached (so current-error-context can read them). Setting the
	// parent mark is the Go equivalent of (parameterize ((exc-param parent)) …).
	errCtx := NewErrorContext(source, trace, nil)
	sub := mc.NewSubContext()
	defer ReleaseSubContext(sub)
	sub.SetMark(exceptionHandlerParam, parent)
	sub.SetMark(ErrorContextKey(), errCtx)

	_, err := sub.ApplyCallable(handler, cond)
	if err != nil {
		return err
	}
	err = sub.Run()
	if err != nil {
		// The handler escaped (guard's prompt abort, a captured continuation, a
		// nested raise's uncaught carrier, …). Let it bubble unchanged.
		return err
	}

	if continuable {
		// raise-continuable: the handler returned; ALL its values become the values of
		// the raise-continuable expression (R7RS §6.11 — "the values returned by the
		// handler"). Use SetValues/GetValues so a multi-value handler return is not
		// collapsed to its first value.
		mc.SetValues(sub.GetValues()...)
		return nil
	}

	// Non-continuable handler returned: raise a secondary exception in the handler's
	// dynamic extent (sub has exc-param = parent), so it escalates to the parent.
	// Terminates: each level reads a shorter handler list, bottoming out at the
	// empty-stack uncaught carrier above.
	secondary := values.NewErrorObjectWithCause(
		"exception handler returned from non-continuable exception",
		werr.ErrNonContinuableException,
	)
	return RaiseInPlace(sub, secondary, false)
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
