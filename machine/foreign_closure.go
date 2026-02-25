package machine

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
)

var _ Closure = (*ForeignClosure)(nil)

// ForeignClosure wraps a Go function as a directly-callable Scheme procedure.
// Unlike MachineClosure, it holds the ForeignFunction directly and bypasses
// the bytecode VM — no template, no opcodes, no VM loop iteration.
type ForeignClosure struct {
	fn         ForeignFunction
	env        *environment.EnvironmentFrame
	paramCount int
	isVariadic bool
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
