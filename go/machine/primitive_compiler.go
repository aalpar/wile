package machine

import (
	"wile/syntax"
	"wile/values"
)

// PrimitiveCompilerFunc is the type for compile-time special form handlers.
// These functions handle syntax-directed compilation of primitive forms like
// `define`, `lambda`, `if`, `quote`, etc.
//
// Parameters:
//   - ctc: The compile-time continuation (compiler state)
//   - ccnt: The compile-time call context (tail position info, etc.)
//   - expr: The expression arguments (everything after the keyword)
//
// The function should emit operations to ctc.template and return nil on success.
type PrimitiveCompilerFunc func(ctc *CompileTimeContinuation, ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error

// PrimitiveCompiler wraps a PrimitiveCompilerFunc as a values.Value so it can
// be stored in the environment.
type PrimitiveCompiler struct {
	name string
	fn   PrimitiveCompilerFunc
}

// NewPrimitiveCompiler creates a new primitive compiler.
func NewPrimitiveCompiler(name string, fn PrimitiveCompilerFunc) *PrimitiveCompiler {
	return &PrimitiveCompiler{name: name, fn: fn}
}

// Name returns the name of this primitive compiler.
func (p *PrimitiveCompiler) Name() string {
	return p.name
}

// Compile invokes the primitive compiler function.
func (p *PrimitiveCompiler) Compile(ctc *CompileTimeContinuation, ccnt CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return p.fn(ctc, ccnt, expr)
}

// SchemeString implements values.Value interface.
func (p *PrimitiveCompiler) SchemeString() string {
	return "#<primitive-compiler:" + p.name + ">"
}

// IsVoid implements values.Value interface.
func (p *PrimitiveCompiler) IsVoid() bool {
	return false
}

// EqualTo implements values.Value interface.
func (p *PrimitiveCompiler) EqualTo(other values.Value) bool {
	if other == nil {
		return false
	}
	otherPC, ok := other.(*PrimitiveCompiler)
	if !ok {
		return false
	}
	return p.name == otherPC.name
}
