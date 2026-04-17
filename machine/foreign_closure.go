package machine

import (
	"errors"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
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

// goErrorToSchemeException converts a Go error to a Scheme exception escape.
// It detects ForeignFileError and ForeignReadError to set the appropriate
// NativeError kind per R7RS §6.11. The MachineContext is used to capture
// the source location and stack trace at the point where the error occurred.
func goErrorToSchemeException(mc *MachineContext, err error) error {
	kind := values.NativeErrorKindGeneric
	var fileErr *werr.ForeignFileError
	var readErr *werr.ForeignReadError
	if errors.As(err, &fileErr) {
		kind = values.NativeErrorKindFile
	} else if errors.As(err, &readErr) {
		kind = values.NativeErrorKindRead
	}
	errObj := values.NewErrorObjectWithCauseAndKind(err.Error(), err, kind)
	return &ErrExceptionEscape{
		Condition:   errObj,
		Continuable: false,
		Handled:     false,
		Source:      mc.CurrentSource(),
		StackTrace:  mc.CaptureStackTrace(20),
	}
}

// applyCallableError converts errors from ApplyCallable into Scheme
// exceptions so they are catchable by guard and with-exception-handler.
// Errors that are already Scheme-level control flow (exception escapes,
// prompt aborts) pass through unchanged.
func applyCallableError(mc *MachineContext, err error) error {
	var excErr *ErrExceptionEscape
	if errors.As(err, &excErr) {
		return err
	}
	var abortErr *ErrPromptAbort
	if errors.As(err, &abortErr) {
		return err
	}
	var timerErr *ErrTimerInterrupt
	if errors.As(err, &timerErr) {
		return err
	}
	return goErrorToSchemeException(mc, err)
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
