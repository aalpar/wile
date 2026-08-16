package machine

import (
	"errors"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// ForeignFunction is the signature for Go-implemented Scheme primitives.
// The CallContext provides access to arguments, the value register,
// and the cancellation context (via mc.Context()).
//
// Most implementations only need CallContext methods (Arg, SetValue,
// SetValues, Authorizer, Context, EnvironmentFrame, Thread).
// Implementations that need full VM access (sub-context creation,
// continuation manipulation) should type-assert to *MachineContext.
type ForeignFunction func(mc CallContext) error

// ConditionFromError converts a Go error into a Scheme condition object (a
// NativeError). It detects ForeignFileError and ForeignReadError to set the
// appropriate NativeError kind per R7RS §6.11. RaiseInPlace later stamps the
// raise-site source location and stack trace onto it.
//
// This is THE converter, and it is exported so that it stays the only one: the
// compile funnel (wile.wrapCompilationError, filling CompilationError.Condition)
// and the runtime bridge (applyCallableError) both go through it, so a Go failure
// has one Scheme-visible representation no matter which verb was running when it
// happened. A second conversion written next to a caller is the defect this
// signature exists to prevent.
//
// A primitive that RETURNS a *values.NativeError is already holding a condition,
// so it is forwarded rather than rebuilt: rebuilding reads only err.Error(), which
// drops the irritants and the kind. CODING_STYLE.md has prescribed that return
// form since 2026-06-27 and it did not work until wave 4 item 4.
//
// The assertion is deliberately DIRECT, never errors.As. errors.As would also
// match a *NativeError reached through a werr wrap, and forwarding that inner
// value would discard the wrap's own message — one losslessness bug traded for
// another. A wrapped condition is a Go error something has added context to, and
// it belongs on the rebuild path. Pinned by TestWrappedNativeErrorIsNotForwarded
// (pkg/wile/native_error_passthrough_test.go).
//
// A rebuilt condition is pre-stamped when the chain carries a compile-time
// location. Without that, a compile failure returned by load, eval or compile
// gets the location of the CALL from enrichNativeError, because the raise
// happens at the frame that invoked the compiler — so error-object-source
// contradicted the message text, which held the real site. See
// innermostCompileLocation.
func ConditionFromError(err error) values.Value {
	nativeErr, ok := err.(*values.NativeError)
	if ok {
		return nativeErr
	}
	kind := values.NativeErrorKindGeneric
	var fileErr *werr.ForeignFileError
	var readErr *werr.ForeignReadError
	if errors.As(err, &fileErr) {
		kind = values.NativeErrorKindFile
	} else if errors.As(err, &readErr) {
		kind = values.NativeErrorKindRead
	}
	msg, irritants := conditionMessageAndIrritants(err)
	q := values.NewErrorObjectWithCauseAndKind(msg, err, kind, irritants...)
	loc := innermostCompileLocation(err)
	if loc != "" {
		// enrichNativeError's once-guard keys on an empty location, so stamping
		// here is idempotent for free: the compile site wins and the raise site
		// is never written over it.
		q.SetSourceLocation(loc)
	}
	return q
}

// conditionError is a compile-time failure that named both halves of R7RS §6.11's
// pair itself — today only (syntax-error message irritant ...), which §4.3.3
// defines by delegation to error, so its operands ARE that pair and nothing has to
// be recovered from rendered text.
//
// Same edge and same reason as sourcedError: the concrete type lives in
// compilation, which imports this package.
//
// ErrorMessage and ErrorIrritants rather than Message and Irritants:
// *values.NativeError has both of the shorter names, so an interface keyed on them
// would match a condition buried in the chain and harvest ITS message and
// irritants onto the one being built — the same hazard SourceContext avoids for
// the location.
type conditionError interface {
	error
	ErrorMessage() string
	ErrorIrritants() []values.Value
}

// conditionMessageAndIrritants projects err onto the two facts R7RS §6.11
// specifies. They are returned together because one lookup answers both whenever
// the raise site declared them, and splitting the pair would walk the chain twice
// with two different notions of which link won.
//
// The message is NOT err.Error(). A rendered chain fuses the route to the failure,
// the failure, and the sentinel's category; failureChain drops the first and
// werr.FailureMessage drops the third. The whole chain stays on the condition's
// wrapped error, so nothing an embedder or a rendering could reach before is
// unreachable now — see values.NativeError.Unwrap and wile.CompilationError.Error.
//
// Irritants are nil in the ordinary case, which is why
// NewErrorObjectWithCauseAndKind is called variadically rather than through two
// branches.
func conditionMessageAndIrritants(err error) (string, []values.Value) {
	var ce conditionError
	if errors.As(err, &ce) {
		return ce.ErrorMessage(), ce.ErrorIrritants()
	}
	msg := werr.FailureMessage(failureChain(err))
	if msg == "" {
		// The chain is nothing but a category sentinel, so its own text is all
		// there is. Widest fallback on purpose: this is the one shape where
		// nothing better exists.
		msg = err.Error()
	}
	return msg, nil
}

// failureChain returns the part of err that describes the failure rather than the
// compiler's route to it: everything below the innermost sourcedError, which is
// where provenance was stamped and therefore where "what failed" ends and "how the
// compiler reached it" begins. A runtime failure carries no sourcedError and comes
// back unchanged.
//
// Cutting on the stamp rather than reducing to the innermost wrap is the load-bearing
// choice. Every wrap message is supposed to name where and what, so at a primitive
// the outer ones are substance — "file-exists?: argument 0" is the operation, and
// "/: division error" is the primitive that failed. Only the compile funnel stacks
// wraps that describe its own traversal ("compilation", "expansion", "expand: failed
// to expand list expression", "load: in f.scm"), and every one of them sits above
// the stamp. The file name in that last one is not lost either: it is the head of
// the location innermostCompileLocation puts on the condition.
//
// Same walk as innermostCompileLocation, different reduction — deepest, versus
// deepest with a non-empty location.
func failureChain(err error) error {
	q := err
	var se sourcedError
	for e := err; errors.As(e, &se); e = errors.Unwrap(se) {
		inner := errors.Unwrap(se)
		if inner != nil {
			q = inner
		}
	}
	return q
}

// sourcedError is compilation.SourcedError seen from below. The compiler stamps a
// source context onto the errors it raises, but compilation imports this package,
// so the concrete type cannot be named here and an errors.As target has to be an
// interface.
//
// SourceContext, not a location string, is the discriminating method on purpose:
// *values.NativeError already has SourceLocation, and an interface keyed on that
// name would match a condition buried in the chain and stamp its location onto
// the one being built.
type sourcedError interface {
	error
	SourceContext() *syntax.SourceContext
}

// innermostCompileLocation returns the deepest non-empty location on err's
// compiler chain, or "" when it carries none — which is the ordinary case for a
// runtime failure, and the reason stamping is conditional.
//
// Deepest wins because it names the sub-expression that failed rather than the
// form enclosing it. Same rule and same walk as wile.wrapCompilationError, which
// is what an embedder catching the compile error as a Go error already sees; this
// is the Scheme-visible half agreeing with it.
func innermostCompileLocation(err error) string {
	q := ""
	var se sourcedError
	for e := err; errors.As(e, &se); e = errors.Unwrap(se) {
		loc := se.SourceContext().Location()
		if loc != "" {
			q = loc
		}
	}
	return q
}

// isControlSignal reports whether err is Scheme-level control flow rather than a
// failure: a prompt abort (a handler escaping), the uncaught-exception carrier (a
// deeper RaiseInPlace found no handler), a timer interrupt, a debugger break
// interrupt, or the trampoline's continuation-resume bounce. Every one of them is
// addressed to a driver's dispatch loop, so a site that treats it as an ordinary
// error either converts it into a catchable Scheme condition or hands it to the
// embedder — both of which lose the transfer.
//
// This is the single classifier. Do not open another errors.As chain against
// these five types; call this instead, so a new control signal is added in one
// place. It is deliberately a POSITIVE test for the known control types rather
// than a negative test for "ordinary error": Wave 1 §4.3 converts two VM-internal
// panics (machine_context.go's DrainN, and the fused promoted ops' Stack.Pop2)
// into raised conditions that travel this same chain, and those must keep
// reaching their error handling rather than being absorbed as control flow.
func isControlSignal(err error) bool {
	var abortErr *ErrPromptAbort
	if errors.As(err, &abortErr) {
		return true
	}
	var excErr *ErrExceptionEscape
	if errors.As(err, &excErr) {
		return true
	}
	var timerErr *ErrTimerInterrupt
	if errors.As(err, &timerErr) {
		return true
	}
	var breakErr *ErrBreakInterrupt
	if errors.As(err, &breakErr) {
		return true
	}
	var resumeErr *ErrResumeContinuation
	return errors.As(err, &resumeErr)
}

// applyCallableError converts a Go error from a primitive into a Scheme exception
// by invoking the current handler in place (RaiseInPlace) — the single bridge
// between Go-level failures and the mark-based handler chain, so they are catchable
// by guard and with-exception-handler.
//
// Two classes bypass that bridge.
//
// Control signals pass through unchanged; see isControlSignal.
//
// An authorizer denial passes through as a *SchemeError, because a security
// denial is not a Scheme condition: it is the sandbox's answer, not the
// program's business. Converting it made every runtime gate site absorbable by
// an enclosing guard or with-exception-handler, so a sandboxed program could
// neutralise its own refusals. It now terminates the evaluation and reaches the
// embedder with errors.Is(err, security.ErrAccessDenied) intact.
//
// Scope: this covers the RUNTIME gate sites only. The three compile-time load
// gates (resolver's FS and OS resolvers, library_loader) never reach this
// function — they surface as *CompilationError and were already unswallowable.
func applyCallableError(mc *MachineContext, err error) error {
	if isControlSignal(err) {
		return err
	}
	if errors.Is(err, security.ErrAccessDenied) {
		// errors.Is, not errors.As: ErrAccessDenied is a static sentinel and
		// CheckWithAuthorizer wraps every denial with action/resource/target, so
		// both the bare and the contextualised form must match. This is NOT folded
		// into isControlSignal — that function is a positive test for control
		// TRANSFER, and a denial is a failure.
		var stamped *SchemeError
		if errors.As(err, &stamped) {
			// Already carried through one primitive frame. Re-stamping would
			// prepend a second location and a second trace, which is exactly what
			// the SRFI-18 join path does: raiseJoinCondition returns the error
			// unchanged for a carrier joinConditionFor does not recognise, and it
			// re-enters here.
			return err
		}
		// WrapError, not a bare pass-through: it stamps the raise-site location
		// and the VM trace onto the error, which returning err raw would drop.
		// *SchemeError is deliberately not *ErrExceptionEscape — that is the
		// uncaught-CONDITION carrier, which the SRFI-18 join turns back into a
		// catchable values.UncaughtException. The embedder side of that choice is
		// Engine.wrapDenial, which unpacks this carrier into RuntimeError.Source
		// and .StackTrace; without it the stamp survives only as message text.
		return mc.WrapError(err, "")
	}
	return RaiseInPlace(mc, ConditionFromError(err), false)
}

var _ Closure = (*ForeignClosure)(nil)

// ForeignClosure wraps a Go function as a directly-callable Scheme procedure.
// Unlike MachineClosure, it holds the ForeignFunction directly and bypasses
// the bytecode VM — no template, no opcodes, no VM loop iteration.
type ForeignClosure struct {
	fn         ForeignFunction
	validate   ForeignFunction // nil = no validation; set via SetValidator
	env        *environment.EnvironmentFrame
	identity   *PrimitiveIdentity // nil = none; set via SetIdentity
	paramCount int
	isVariadic bool
	name       string
	doc        string
}

func (p *ForeignClosure) closureMarker() {
}

func (p *ForeignClosure) Fn() ForeignFunction {
	return p.fn
}

func (p *ForeignClosure) Env() *environment.EnvironmentFrame {
	return p.env
}

func (p *ForeignClosure) ParameterCount() int {
	return p.paramCount
}

func (p *ForeignClosure) IsVariadic() bool {
	return p.isVariadic
}

func (p *ForeignClosure) Name() string {
	return p.name
}

func (p *ForeignClosure) SetName(name string) {
	p.name = name
}

func (p *ForeignClosure) Doc() string {
	return p.doc
}

// Identity returns the registered primitive this closure was built from, or nil
// if its spec declared none. See PrimitiveIdentity for why a pointer compare on
// the closure itself is not the same question.
func (p *ForeignClosure) Identity() *PrimitiveIdentity {
	return p.identity
}

// SetIdentity records the registered primitive this closure was built from. It is
// called by the registry at registration; every closure the registry mints for one
// spec — in the sealed base and in each library environment — gets the same token.
func (p *ForeignClosure) SetIdentity(identity *PrimitiveIdentity) {
	p.identity = identity
}

func (p *ForeignClosure) SetDoc(doc string) {
	p.doc = doc
}

// SetValidator installs a contract validation function that runs before
// the implementation. Called during registration when contract enforcement
// is enabled.
func (p *ForeignClosure) SetValidator(v ForeignFunction) {
	p.validate = v
}

// Validator returns the installed contract validator, or nil if none.
func (p *ForeignClosure) Validator() ForeignFunction {
	return p.validate
}

func (p *ForeignClosure) IsVoid() bool {
	return p == nil
}

func (p *ForeignClosure) SchemeString() string {
	return "#<foreign-closure>"
}

// AcceptsArity reports whether this closure can be called with n arguments.
func (p *ForeignClosure) AcceptsArity(n int) bool {
	if p.isVariadic {
		return n >= p.paramCount-1
	}
	return n == p.paramCount
}

// EqualTo uses identity semantics — two foreign closures are equal only
// if they are the same pointer.
func (p *ForeignClosure) EqualTo(o values.Value) bool {
	v, ok := o.(*ForeignClosure)
	if !ok {
		return false
	}
	return p == v
}
