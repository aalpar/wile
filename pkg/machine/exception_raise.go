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
// list) is immutable -- ImmutableBase is what enforces that, and the whole
// cross-Engine safety argument rests on it. Before that flag existed, Scheme in
// any Engine could apply the object to one argument (it is bound to the global
// %exception-handlers) and rewrite the shared base, so a Tiny engine could
// harvest conditions that escaped another Engine's handler stack and race its
// writes. This mirrors the process-wide DefaultPromptTag.
//
// Scheme reaches it through the global %exception-handlers binding (bound to this
// object by the core registry's %exception-handler-parameter primitive); Go
// reaches it directly. with-exception-handler is (parameterize ((p (cons H (p))))
// …), which sets a continuation mark keyed by this object — so call/cc copies the
// handler with the continuation, and re-entry restores it. That is the whole fix:
// the handler rides the captured continuation instead of an off-chain Go field.
var exceptionHandlerParam = NewParameter(values.EmptyList, nil, ImmutableBase)

// ExceptionHandlerParam returns the canonical exception-handler parameter so the core
// registry can bind it to the Scheme global %exception-handlers. The return type is
// values.Value, not *Parameter: callers only need to bind the object, and narrowing
// keeps the mutating Parameter.SetValue out of reach so the shared singleton's base
// value (the immutability the cross-Engine safety argument rests on) can't be
// rewritten through this accessor. RaiseInPlace uses the package var directly.
func ExceptionHandlerParam() values.Value {
	return exceptionHandlerParam
}

// escalatorArm marks one non-continuable handler invocation's finalizer frame.
// cond is the condition the arm's raise raised: a raise-continuable of the same
// object while the frame is live is that raise's guard re-raise
// (reRaisesNonContinuable).
type escalatorArm struct {
	cond values.Value
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
	return mc.raiseToHandlers(cond, continuable, handlers, handlerReturnedMessage)
}

// handlerReturnedMessage is the secondary exception's message when a handler
// returns from a non-continuable raise (R7RS §6.11).
const handlerReturnedMessage = "exception handler returned from non-continuable exception"

// raiseToHandlers runs the FIRST handler of `handlers` on cond, escalating to the
// rest (its cdr) on a non-continuable normal return. handlers is passed explicitly so
// the non-continuable escalation targets the parent list directly rather than
// re-resolving from marks (the handler ran with the parent mark, but the finalizer
// frame that escalates runs after the handler's marks are gone). returned is the
// message of that escalation's secondary exception; the escalation itself raises
// with handlerReturnedMessage.
func (mc *MachineContext) raiseToHandlers(cond values.Value, continuable bool, handlers values.Value, returned string) error {
	source := mc.CurrentSource()
	trace := mc.CaptureStackTrace(DefaultBacktraceDepth)
	enrichNativeError(cond, source, trace)

	// A raise that has left an expansion-time root's own handlers is recorded on
	// the root, as the escape it would have been with no caller's handlers below.
	root := mc.inheritingRoot(handlers)
	pair, ok := handlers.(*values.Pair)
	if !ok {
		escape := mc.uncaughtEscape(cond, continuable, source, trace)
		if root != nil {
			root.raisedToCaller = escape
		}
		return escape
	}
	if root != nil {
		root.raisedToCaller = mc.uncaughtEscape(cond, continuable, source, trace)
	}
	handler := pair.Car()
	parent := pair.Cdr()

	// Install the parent handler as current and the raise-site diagnostics on the LIVE
	// activation (SetMark updates the marks in place; the Go equivalent of
	// (parameterize ((exc-param parent)) …)). This makes a re-raise inside the handler
	// escalate, and rides the marks into a continuation captured inside the handler.
	mc.SetMark(exceptionHandlerParam, parent)
	// Snapshot the marks EAGERLY at the raise site (the third leg of the diagnostic
	// triple alongside source + trace). It must be taken here: the handler runs with
	// its own marks, and chain frames get pooled/reclaimed once we leave the raise,
	// so a lazy "capture the pointer now, materialize later" scheme would race the
	// continuation machinery (MarkChainShared). Bounded to DefaultPromptTag, and
	// CollectContinuationMarks skips mark-less frames, so sparse-mark code pays little.
	marks := mc.CollectContinuationMarks(DefaultPromptTag)
	mc.SetMark(ErrorContextKey(), NewErrorContext(source, trace, marks))

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
	// escape (guard escapes via guard-k). If it RETURNS, the finalizer frame raises a
	// secondary non-continuable exception to the parent handlers, the handler's
	// dynamic environment (R7RS §6.11), directly rather than re-resolved from marks.
	// Each escalation reads a shorter handler list, bottoming out at the empty-stack
	// uncaught carrier above. The frame's applyToValuesCode template applies the
	// escalator to (and discards) the handler's return value(s).
	//
	// Every return reaches the escalator, however the handler's continuation was
	// reached: a guard whose clauses miss re-raises with raise-continuable and
	// resumes handler-k, so the outer handler's value returns from this handler and
	// raises the secondary; so does re-entering the handler after the guard exited.
	// Chez does the same.
	escalateFn := func(finCC CallContext) error {
		finMC, err := RequireMachineContext(finCC, "raise")
		if err != nil {
			return err
		}
		secondary := values.NewErrorObjectWithCause(returned, werr.ErrNonContinuableException)
		return finMC.raiseToHandlers(secondary, false, parent, handlerReturnedMessage)
	}
	// Use a runtime env reachable from here, falling back to the live frame: when the
	// raise fires inside a reified call-with-values producer the live procedure frame
	// is detached (nil namespace), so MutableRuntime() would panic. The escalator is a
	// pure-Go closure; any valid frame serves for its apply-time InitApplyFrame.
	escalatorEnv := mc.ClosureEnv()
	escalator := NewForeignClosure(escalatorEnv, 1, true, escalateFn)
	escalateTpl := &NativeTemplate{
		code:     applyToValuesCode,
		literals: MultipleValues{escalator},
	}
	frame := NewMachineContinuation(mc.cont, escalateTpl, mc.env)
	frame.escalatorArm = &escalatorArm{cond: cond}
	_, err := mc.RunBodyUnderFrame(frame, handler, cond)
	return err
}

// uncaughtEscape is the carrier of cond raised on mc with no handler: it goes to
// the embedder with the location and trace of the condition's first raise when it
// is an error object raised before (a guard's re-raise, the bridge's re-raise of
// an expansion-time root's escape), so the two name the same raise.
func (mc *MachineContext) uncaughtEscape(cond values.Value, continuable bool, source *syntax.SourceContext, trace StackTrace) *ErrExceptionEscape {
	origin := firstRaiseOf(cond)
	if origin != nil {
		source, trace = origin.source, origin.trace
	}
	continuable = continuable && !reRaisesNonContinuable(mc, cond)
	return &ErrExceptionEscape{Condition: cond, Source: source, StackTrace: trace, Continuable: continuable}
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
	ne.SetRaiseOrigin(&raiseOrigin{source: source, trace: trace})
}

// reRaisesNonContinuable reports whether a raise of cond on mc is made in the
// dynamic environment of a non-continuable raise of the same object: a live
// escalator frame for it is on mc's chain or a parent's. A guard whose clauses
// miss re-raises that way with raise-continuable (R7RS 4.2.7), so its uncaught
// re-raise is the program's raise, and is as continuable as that was. The walk
// runs only for an uncaught raise-continuable.
func reRaisesNonContinuable(mc *MachineContext, cond values.Value) bool {
	for c := mc; c != nil; c = c.parentMC {
		for frame := c.cont; frame != nil; frame = frame.parent {
			if frame.escalatorArm != nil && values.Eqv(frame.escalatorArm.cond, cond) {
				return true
			}
		}
	}
	return false
}

// raiseOrigin is the first raise of an error object, kept on it
// (NativeError.RaiseOrigin) beside the Scheme-visible location and trace that
// enrichNativeError stamps from the same raise.
type raiseOrigin struct {
	source *syntax.SourceContext
	trace  StackTrace
}

// firstRaiseOf returns cond's first raise, or nil for a condition that is not
// an error object or whose location was stamped without one (a compile error
// carries its location from the compiler, not from a raise).
func firstRaiseOf(cond values.Value) *raiseOrigin {
	ne, ok := cond.(*values.NativeError)
	if !ok {
		return nil
	}
	q, _ := ne.RaiseOrigin().(*raiseOrigin)
	return q
}

// StackTraceToSchemeList converts a StackTrace to a Scheme list of alists, one per
// frame. Each alist always carries name; file, line, and column are present only
// when the frame has a source LOCATION, and file is #f when that location has a
// position but no filename. Lives here (not in the registry) because it
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

	// Prefer CurrentLoc over CallSite, and ask each one the same question
	// StackFrame.String asks: does it have a LOCATION, not merely is it
	// non-nil. A nil test admits a context that EXISTS and carries no
	// position — which is what eval hands every frame of a runtime-built
	// datum, since PrimEval mints a zero-value SourceContext for it. (A
	// frame with genuinely nil contexts was already rendered correctly; the
	// zero-value one was not, and the two states are indistinguishable to a
	// reader of the result.)
	//
	// The two renderings used to disagree about the same frame. The textual
	// trace printed a bare "at <anonymous>"; the alist fabricated
	// (file . "") (line . 0) (column . 0), which is a plausible-looking
	// line 0 indistinguishable from a real one. Worse, HEAD already emitted
	// the correct bare ((name . "<anonymous>")) form for frames whose two
	// contexts were both nil, so a single result list interleaved both
	// shapes. Location() is nil-safe, so it subsumes the nil check.
	src := frame.CurrentLoc
	if src.Location() == "" {
		src = frame.CallSite
	}

	nameEntry := values.NewCons(values.NewSymbol("name"), values.NewString(name))
	if src.Location() == "" {
		return values.List(nameEntry)
	}

	// StringOrFalse, not NewString: a located but unnamed program — the
	// stdin CLI mode, and any embedder calling EvalMultiple without a name —
	// has a real line and column and an empty File. Four sibling accessors
	// already encode that absence as #f (error-object-source,
	// error-context-source, syntax-source twice); "" is not a filename, and
	// per the project's own rule an unknown value gets a named absence
	// rather than a zero value that reads as data.
	fileEntry := values.NewCons(values.NewSymbol("file"), values.StringOrFalse(src.File))
	lineEntry := values.NewCons(values.NewSymbol("line"), values.NewInteger(int64(src.Start.Line())))
	colEntry := values.NewCons(values.NewSymbol("column"), values.NewInteger(int64(src.Start.Column())))

	return values.List(nameEntry, fileEntry, lineEntry, colEntry)
}
