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

// applyCallableError converts a Go error from a primitive into a Scheme exception
// by invoking the current handler in place (RaiseInPlace) — the single bridge
// between Go-level failures and the mark-based handler chain, so they are catchable
// by guard and with-exception-handler. Errors that are already Scheme-level control
// flow pass through unchanged: prompt aborts (a handler escaping), the uncaught
// exception carrier (a deeper RaiseInPlace found no handler), and timer interrupts.
func applyCallableError(mc *MachineContext, err error) error {
	var abortErr *ErrPromptAbort
	if errors.As(err, &abortErr) {
		return err
	}
	var excErr *ErrExceptionEscape
	if errors.As(err, &excErr) {
		return err
	}
	var timerErr *ErrTimerInterrupt
	if errors.As(err, &timerErr) {
		return err
	}
	// A continuation-resume control signal (the trampoline bounce) is Scheme-level
	// control flow, not a Go failure: pass it through so it reaches the nearest
	// DefaultPromptTag driver. Without this, RaiseInPlace below would convert it
	// into a catchable Scheme condition.
	var resumeErr *ErrResumeContinuation
	if errors.As(err, &resumeErr) {
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
