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

package compilation

import (
	"context"
)

// CompileTimeCallContext carries contextual information through the compilation process.
// It tracks whether an expression is in tail position, which controls whether the
// compiler emits SaveContinuation (non-tail) or reuses the current frame (tail).
//
// This structure is passed by value (not pointer) through the compiler, allowing each
// compilation step to create modified copies without affecting the caller's context.
//
// # Tail Call Optimization
//
// Tail call optimization (Steele 1977, R7RS §3.5): tail calls reuse the
// caller's continuation frame instead of allocating a new one, making
// recursive procedures in tail position run in constant stack space.
// See BIBLIOGRAPHY.md "Tail Call Optimization".
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
type CompileTimeCallContext struct {
	ctx    context.Context
	inTail bool
	// selfTail, when non-nil, marks that the current position is in the body of a
	// closure proven self-tail-reusable (validate.BodyIsSelfTailReusable, plus
	// Stable for a top-level self). It carries the self name and arity so a tail
	// call matching them can be emitted as OpSelfTailCall (in-place frame reuse).
	// It is preserved through tail-transparent forms (if branches, begin/body
	// tails), dropped by NotInTail(), and cleared on entry to a frame-pushing form
	// (let) — so a set selfTail at a call site means a DEPTH-0 tail self call.
	selfTail *selfTailInfo
}

// selfTailInfo identifies the enclosing self-tail-reusable closure: its bound
// name (symbol Key) and required-parameter count.
type selfTailInfo struct {
	name  string
	arity int
}

// NewCompileTimeCallContext creates a new compile-time context.
// Parameters:
//   - inTail: true if compiling an expression in tail position
func NewCompileTimeCallContext(ctx context.Context, inTail bool) CompileTimeCallContext {
	return CompileTimeCallContext{
		ctx:    ctx,
		inTail: inTail,
	}
}

// Context returns the context associated with this compile-time call context.
func (p CompileTimeCallContext) Context() context.Context {
	return p.ctx
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
//	err := p.CompileExpression(ctctx.NotInTail(), argExpr)
func (p CompileTimeCallContext) NotInTail() CompileTimeCallContext {
	return CompileTimeCallContext{
		ctx:    p.ctx,
		inTail: false,
		// selfTail intentionally dropped: a non-tail position can never be a
		// rewritable self-tail call.
	}
}

// WithSelfTail returns a copy marking the current (tail) position as the body of
// a self-tail-reusable closure named name with arity required parameters.
// Preserves inTail and ctx.
func (p CompileTimeCallContext) WithSelfTail(name string, arity int) CompileTimeCallContext {
	q := p
	q.selfTail = &selfTailInfo{name: name, arity: arity}
	return q
}

// WithoutSelfTail returns a copy with the self-tail context cleared. Used when
// descending into a frame-pushing form (let): its body runs in a pushed frame, so
// a self call there is no longer at the parameter-frame depth and must not be
// rewritten to the in-place OpSelfTailCall.
func (p CompileTimeCallContext) WithoutSelfTail() CompileTimeCallContext {
	q := p
	q.selfTail = nil
	return q
}
