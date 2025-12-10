package machine

import (
	"context"
	"wile/environment"
)

// CompileTimeCallContext carries contextual information through the compilation process.
// It tracks properties that affect code generation, particularly for tail call optimization
// and distinguishing definition contexts from expression contexts.
//
// This structure is passed by value (not pointer) through the compiler, allowing each
// compilation step to create modified copies without affecting the caller's context.
//
// # Tail Call Optimization
//
// The inTail flag tracks whether an expression is in tail position. An expression is in
// tail position if its value will be the final result of the enclosing procedure. When
// inTail is true, the compiler can generate a tail call that reuses the current stack
// frame instead of creating a new one, preventing stack overflow in recursive procedures.
//
// Per R7RS Section 3.5, these positions are tail positions:
//   - The body of a lambda expression
//   - The last expression in a begin sequence (if the begin is in tail position)
//   - The consequent and alternative of an if expression (if the if is in tail position)
//   - The body of a let/let*/letrec (if the let is in tail position)
//
// These are NOT tail positions (use NotInTail()):
//   - Function arguments: (f (g x)) - the call to g is not in tail position
//   - Condition of if: (if (pred x) ...) - pred is not in tail position
//   - Definitions: (define x (expr)) - expr is not in tail position
//   - Non-final expressions in begin: (begin (a) (b) (c)) - only c is in tail position
//
// # Definition vs Expression Context
//
// The inExpression flag distinguishes between definition contexts (top-level or internal
// definitions) and expression contexts. This affects how certain forms are compiled:
//   - In definition context: define creates bindings
//   - In expression context: define may be an error or treated differently
//
// Use NotInExpression() when entering a definition-only context (e.g., library body).
type CompileTimeCallContext struct {
	ctx          context.Context
	env          *environment.EnvironmentFrame
	inTail       bool
	inExpression bool
}

// NewCompileTimeCallContext creates a new compile-time context with the given flags.
// Parameters:
//   - inTail: true if compiling an expression in tail position
//   - inExpression: true if compiling an expression (vs. a definition)
//   - env: the environment frame for variable resolution during compilation
func NewCompileTimeCallContext(inTail, inExpression bool, env *environment.EnvironmentFrame) CompileTimeCallContext {
	return CompileTimeCallContext{
		env:          env,
		inTail:       inTail,
		inExpression: inExpression,
	}
}

// NotInTail returns a copy of the context with inTail set to false.
// Use this when compiling sub-expressions that are not in tail position:
//   - Function arguments
//   - Condition expressions in if
//   - Initial values in define/let bindings
//   - Non-final expressions in begin
//
// Example:
//
//	// Compiling (f (g x)) - the call to g is not in tail position
//	err := p.CompileExpression(ccnt.NotInTail(), argExpr)
func (p CompileTimeCallContext) NotInTail() CompileTimeCallContext {
	return CompileTimeCallContext{
		env:          p.env,
		inTail:       false,
		inExpression: p.inExpression,
	}
}

// NotInExpression returns a copy of the context with inExpression set to false.
// Use this when entering a definition-only context where expressions are not allowed
// at the current level (e.g., library declaration bodies before begin).
func (p CompileTimeCallContext) NotInExpression() CompileTimeCallContext {
	return CompileTimeCallContext{
		env:          p.env,
		inTail:       p.inTail,
		inExpression: false,
	}
}
