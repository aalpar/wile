package machine

import (
	"errors"

	"github.com/aalpar/wile/pkg/environment"
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

// goErrorToCondition converts a Go error into a Scheme condition object (a
// NativeError). It detects ForeignFileError and ForeignReadError to set the
// appropriate NativeError kind per R7RS §6.11. RaiseInPlace later stamps the
// raise-site source location and stack trace onto it.
func goErrorToCondition(err error) values.Value {
	kind := values.NativeErrorKindGeneric
	var fileErr *werr.ForeignFileError
	var readErr *werr.ForeignReadError
	if errors.As(err, &fileErr) {
		kind = values.NativeErrorKindFile
	} else if errors.As(err, &readErr) {
		kind = values.NativeErrorKindRead
	}
	return values.NewErrorObjectWithCauseAndKind(err.Error(), err, kind)
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
// by guard and with-exception-handler. Errors that are already Scheme-level control
// flow pass through unchanged; see isControlSignal.
func applyCallableError(mc *MachineContext, err error) error {
	if isControlSignal(err) {
		return err
	}
	return RaiseInPlace(mc, goErrorToCondition(err), false)
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
