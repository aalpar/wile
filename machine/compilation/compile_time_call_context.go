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
	// frameReuse is the single frame-reuse disposition of the current position —
	// at most ONE mode, never a contradiction. It is set on a closure body proven
	// safe, preserved through tail-transparent forms (if branches, begin/body
	// tails), dropped by NotInTail(), and cleared on entry to a frame-pushing form
	// (let) via WithoutFrameReuse — so a non-none frameReuse at a call site means a
	// DEPTH-0 tail call. The emit site discriminates on frameReuse.kind.
	frameReuse frameReuse
}

// frameReuseKind discriminates how a depth-0 tail call may reuse the parameter
// frame. A closure is armed with exactly one kind (self-tail takes precedence
// over release), so the previously-representable contradiction "self-tail AND
// releasable at once" is now unrepresentable.
type frameReuseKind uint8

const (
	// frameReuseNone — the frame must not be reused here.
	frameReuseNone frameReuseKind = iota
	// frameReuseSelfTail — a depth-0 tail call to name/arity rebinds the frame in
	// place (OpSelfTailCall); requires a Stable self binding.
	frameReuseSelfTail
	// frameReuseRelease — a depth-0 general tail call releases the dead frame to
	// the pool (OpReleaseEnvFrame) before applying.
	frameReuseRelease
)

// frameReuse describes the parameter frame's reuse disposition. name and arity
// are meaningful only for frameReuseSelfTail; the zero value is frameReuseNone.
type frameReuse struct {
	kind  frameReuseKind
	name  string
	arity int
}

// noFrameReuse is the frameReuseNone disposition (the zero value), named for use
// at sites that explicitly arm no reuse (anonymous lambdas, case-lambda clauses, a
// define that qualifies for neither mode).
func noFrameReuse() frameReuse {
	return frameReuse{kind: frameReuseNone}
}

// selfTailReuse arms the self-tail (in-place rebind) mode for name with arity.
func selfTailReuse(name string, arity int) frameReuse {
	return frameReuse{kind: frameReuseSelfTail, name: name, arity: arity}
}

// releaseReuse arms the frame-release mode.
func releaseReuse() frameReuse {
	return frameReuse{kind: frameReuseRelease}
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
		// frameReuse intentionally dropped (zero value = frameReuseNone): a non-tail
		// position can never be a rewritable self-tail call or a frame-release site.
	}
}

// WithFrameReuse returns a copy armed with the given frame-reuse disposition.
// Preserves inTail and ctx.
func (p CompileTimeCallContext) WithFrameReuse(fr frameReuse) CompileTimeCallContext {
	q := p
	q.frameReuse = fr
	return q
}

// WithoutFrameReuse returns a copy with the frame-reuse context cleared. Used when
// descending into a frame-pushing form (let): its body runs in a pushed frame, so
// a call there is no longer at the parameter-frame depth — neither the in-place
// OpSelfTailCall nor OpReleaseEnvFrame (both act on the parameter frame) may fire.
func (p CompileTimeCallContext) WithoutFrameReuse() CompileTimeCallContext {
	q := p
	q.frameReuse = noFrameReuse()
	return q
}
