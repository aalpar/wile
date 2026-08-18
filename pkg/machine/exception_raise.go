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

// escalatorArm is the per-arm revival flag for one non-continuable handler
// invocation. One is minted each time raiseToHandlers arms an escalator; the
// pointer lives BOTH on the finalizer frame and in the escalator closure, so it
// survives the frame copies SliceContinuationAt / Copy / DeepCopy make — the
// closure and any copy of the frame all observe the same flag.
//
// revived means a resume re-entered this frame's extent FROM OUTSIDE: the resumed
// segment carried the frame while the chain live AT THE (k v) SITE did not. That is
// the guard miss-path, and only that path may forward the handler's value. The
// invoking context is the one that answers, because this frame may sit on a
// sub-context's chain the reinstating driver never held.
type escalatorArm struct {
	revived bool
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
	trace := mc.CaptureStackTrace(DefaultBacktraceDepth)
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
	// re-raise's result flows back THROUGH this frame; FORWARD it instead of escalating.
	//
	// The discriminator is per-arm REVIVAL, not "did any resume happen". Both the guard
	// miss-path and an ordinary call/cc escape taken inside the handler capture a
	// segment CONTAINING this finalizer frame, so segment membership alone is vacuous.
	// They differ in the LIVE chain AT THE (k v) SITE: guard-k's call-with-exit abort had
	// already discarded the frame, so reinstating handler-k re-enters this frame's extent
	// FROM OUTSIDE and the resume sets arm.revived. A jump that never left the extent
	// finds the frame still live, leaves the arm unrevived, and the §6.11 secondary still
	// fires — which is the whole point of the arm.
	//
	// "At the (k v) site", not "at reinstatement": this frame lands on whatever chain the
	// raise was handled on, and a raise inside a dynamic-wind thunk (or any other
	// sub-context) is handled on the SUB's chain while the call/cc trampoline reinstates
	// on the top driver's. Judging there reads the frame as absent — a revival — for a
	// jump that never left it, which swallows the secondary again. pendingEscalatorRevivals
	// is therefore called by the invoker, not by ReinstallSegment.
	//
	// (A context-global isolatedMarks gate was WRONG here: it stays true after ANY prior
	// resume on the driver, so it swallowed the mandatory secondary exception for every
	// later non-continuable handler return — see TestNonContinuableHandlerReturnErrors.)
	arm := &escalatorArm{}
	escalateFn := func(finCC CallContext) error {
		finMC, err := RequireMachineContext(finCC, "raise")
		if err != nil {
			return err
		}
		if arm.revived {
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
	frame.escalatorArm = arm
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
