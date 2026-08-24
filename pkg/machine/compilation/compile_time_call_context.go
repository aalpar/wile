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

	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/syntax"
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
	// tails), dropped by NotInTail(), and cleared on entry to a form that PUSHES a
	// frame (see UnderLetFrame) — so a non-none frameReuse at a call site means a
	// DEPTH-0 tail call, where depth counts RUNTIME frames. A merged let pushes
	// none, so its body is still depth 0 and keeps the disposition. The emit site
	// discriminates on frameReuse.kind.
	frameReuse frameReuse
	// enclosingDefines is the body sequence whose internal defines form the
	// letrec* group being compiled, carried so frameReuseForDefine can prove a
	// member releasable against its siblings. Unlike frameReuse it is NOT a
	// depth signal: it is set where defines are predeclared and overridden on
	// entry to a nested body, never merely dropped. nil means "no group known",
	// which refuses.
	enclosingDefines []validate.ValidatedExpr
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

// frameReuse describes the parameter frame's reuse disposition. selfSym, arity and
// letDepth are meaningful only for frameReuseSelfTail; the zero value is
// frameReuseNone.
type frameReuse struct {
	kind frameReuseKind
	// letDepth is how many `let` frames lexically separate the call site from the
	// parameter frame — the operand OpSelfTailCall pops before rebinding.
	//
	// It is counted HERE and not by the validate-side walk, and that is the whole
	// coupling argument. compileValidatedLet emits exactly one OpPushEnv and makes
	// exactly one UnderLetFrame() call, so the counter equals the runtime frame
	// count by construction rather than by two walks agreeing. The or-shaped let
	// (compileOrShapedLet) emits no frame and passes ctctx through untouched, so it
	// contributes nothing to either side automatically — where a second counter in
	// validate would have had to remember not to count it.
	letDepth int
	// selfSym is the armed closure's own binder IDENTIFIER, not its spelling. The
	// emit gate resolves it against the call site's env and compares BINDINGS, so a
	// tail call to a same-spelled DIFFERENT binding — a macro template's escape to a
	// user global of the loop's name — is not rewritten into a jump to pc=0.
	//
	// A symbol and not a resolved *environment.Binding: a local binding is
	// &frame.local.bindings[i], a pointer into a slice EnsureLocalBinding can
	// reallocate, so it is not a stable identity to carry across compilation steps.
	// Resolving both sides inside one call has no such window.
	selfSym *syntax.SyntaxSymbol
	arity   int
}

// noFrameReuse is the frameReuseNone disposition (the zero value), named for use
// at sites that explicitly arm no reuse (anonymous lambdas, case-lambda clauses, a
// define that qualifies for neither mode).
func noFrameReuse() frameReuse {
	return frameReuse{kind: frameReuseNone}
}

// selfTailReuse arms the self-tail (in-place rebind) mode for the closure bound by
// selfSym, at arity. letDepth starts at 0 — the closure body itself runs in the
// parameter frame.
func selfTailReuse(selfSym *syntax.SyntaxSymbol, arity int) frameReuse {
	return frameReuse{kind: frameReuseSelfTail, selfSym: selfSym, arity: arity}
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
		//
		// enclosingDefines intentionally PRESERVED, and the asymmetry is the point:
		// it is lexical group membership, not a statement about depth or tail
		// position. An internal define is a non-final body expression, so it is
		// always compiled through here — dropping the group would leave every
		// internal define unable to name its own siblings, which is precisely the
		// bug this preserves against.
		enclosingDefines: p.enclosingDefines,
	}
}

// WithFrameReuse returns a copy armed with the given frame-reuse disposition.
// Preserves inTail and ctx.
func (p CompileTimeCallContext) WithFrameReuse(fr frameReuse) CompileTimeCallContext {
	q := p
	q.frameReuse = fr
	return q
}

// WithEnclosingDefines returns a copy carrying the body sequence whose internal
// defines form the letrec* group currently being compiled — the group
// InternalDefineFrameReleasable needs to prove one member releasable, since a
// define alone cannot name its own siblings.
//
// It must be set wherever internal defines are PREDECLARED (the three
// predeclareDefineFromValidatedRecursive / predeclareBodyDefines sites), and reset
// rather than inherited on the way into a nested body: a lambda body starts a
// fresh context, and a let body overrides with its own. Inheriting a stale body
// would be a correctness bug, not just imprecision, which is why the predicate
// independently verifies the define is a member of what it is handed.
func (p CompileTimeCallContext) WithEnclosingDefines(body []validate.ValidatedExpr) CompileTimeCallContext {
	q := p
	q.enclosingDefines = body
	return q
}

// UnderLetFrame returns a copy describing the body of a let. It is the ONLY site
// that may say so, because it is paired 1:1 with compileValidatedLet's single
// descent into a body.
//
// pushesFrame says whether that let emitted an OpPushEnv. A merged let does not
// (compilation/merged_slots.go): its slots live in the enclosing lambda's frame,
// so the body runs in the SAME runtime frame the head did.
//
// The two dispositions answer differently, and the asymmetry is the phase:
//
//   - SELF-TAIL survives, with letDepth incremented ONLY for a pushing let.
//     OpSelfTailCall pops that many frames before rebinding, so a self call under
//     a let is rewritable after all (frame-reclaim Phase C). Before Phase C this
//     cleared, and the call leaked one parameter frame per iteration. A merged let
//     contributes no frame, so incrementing would unwind the parameter frame's
//     PARENT — the count has to equal the runtime frame count by construction,
//     which is the whole reason it is counted here rather than by a second walk.
//
//   - RELEASE survives a MERGED let and is still cleared by a pushing one. For a
//     pushing let the reason is that OpReleaseEnvFrame releases mc.env, which
//     inside the body is the LET frame — not pool-owned, so the release would be
//     a no-op at best. A merged let removes that reason exactly: mc.env inside
//     its body IS the enclosing pooled parameter frame, and the merged slots the
//     body reads are slots OF that frame, so the enclosing body's releasability
//     proof covers them unchanged.
//
//     Merging on its own recovers only the RETURN-path release (no OpPushEnv
//     means no envPooled clear, so RestoreAndRelease recycles). A frame
//     abandoned at a general TAIL call still needs the explicit
//     OpReleaseEnvFrame, and 92% of let frames are in tail position, so this
//     clause is what reaches them. Measured on the Gabriel corpus at the default
//     immutable top level: nqueens 2,264 -> 868,868 releases, pool hit
//     78.6% -> 88.8%, wall −3.5%. It does NOT explain schelog zebra's 66.9% —
//     that harness needs a mutable top level, under which no global callee is
//     Stable and frame reclaim arms nowhere at all. See
//     memory/flat-closure-baseline.local.md §6.
//
//     The peephole hazard this used to name is real but belongs to the peephole:
//     a fusion that hoisted a PushLocal callee across the release would resolve
//     it out of the frame just handed to the pool. The interior scans refuse
//     that by callee kind (peephole.go, releaseSafeCallee) rather than by
//     refusing to emit the release.
func (p CompileTimeCallContext) UnderLetFrame(pushesFrame bool) CompileTimeCallContext {
	q := p
	switch p.frameReuse.kind {
	case frameReuseSelfTail:
		if pushesFrame {
			q.frameReuse.letDepth++
		}
	case frameReuseRelease:
		if pushesFrame {
			q.frameReuse = noFrameReuse()
		}
	default:
		q.frameReuse = noFrameReuse()
	}
	return q
}
