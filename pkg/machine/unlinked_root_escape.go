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

package machine

import (
	"context"
	"errors"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// unlinkedRootEscape marks an uncaught raise out of a root that ran
// synchronously for the caller but is not linked to its chain, so it searched
// none of the caller's handlers: an expansion-time root that did not inherit
// them (inheritCallerHandlers; one that did settles its escape instead,
// settleRootEscape), which runs a macro transformer, a phase-1 expression or a
// library body, or the root of an engine run a host function started
// (Engine.Call, Engine.EvalMultiple). Its
// *ErrExceptionEscape means "uncaught on that root", not "uncaught": the first
// foreign-call bridge it reaches raises the condition in the caller's handlers
// (applyCallableError), the mechanism thread-join! uses for a joinee's outcome.
//
// err is the full chain, escape included, so errors.As still finds the
// *ErrExceptionEscape and the embedder still sees an uncaught raise when no
// handler takes it.
//
// A nil escape is a settled mark: a bridge with no handlers wrapped the chain it
// received, and the raise is uncaught from there out. errors.As finds the
// outermost mark, so the settled one hides the live one inside it.
type unlinkedRootEscape struct {
	err    error
	escape *ErrExceptionEscape
}

// MarkUnlinkedRootEscape marks err as an unlinked root's uncaught raise when it
// carries one, and returns it unchanged otherwise. A chain already marked is
// marked again: the outer root's raise is uncaught only on that root too.
func MarkUnlinkedRootEscape(err error) error {
	var escape *ErrExceptionEscape
	if !errors.As(err, &escape) {
		return err
	}
	return &unlinkedRootEscape{err: err, escape: escape}
}

// CancelledBy reports whether err is ctx ending: ctx has ended and err is its
// Err or its Cause. A primitive's own context.DeadlineExceeded in a live ctx, or
// a failure that lands after ctx ended but before the VM noticed, is not.
func CancelledBy(ctx context.Context, err error) bool {
	if ctx.Err() == nil {
		return false
	}
	return errors.Is(err, ctx.Err()) || errors.Is(err, context.Cause(ctx))
}

// Error implements the error interface.
func (p *unlinkedRootEscape) Error() string {
	return p.err.Error()
}

// Unwrap returns the marked chain.
func (p *unlinkedRootEscape) Unwrap() error {
	return p.err
}

// raiseIn raises the escape's condition in mc's handlers. With none, the raise is
// uncaught here too, and err, the chain the bridge received, goes on under a
// settled mark: an outer bridge, such as one below a handler that is running now
// and so is not in mc's handlers, must not raise it a second time. Not
// RaiseInPlace, which would replace that chain with a fresh carrier.
//
// The raise is non-continuable whatever the root raised: the root has ended, so
// a raise-continuable has nothing to resume. A handler that returns from one is
// told so, and where it was raised, rather than that the program raised a
// non-continuable exception.
func (p *unlinkedRootEscape) raiseIn(mc *MachineContext, err error) error {
	handlers := mc.ResolveParameterValue(exceptionHandlerParam)
	_, ok := handlers.(*values.Pair)
	if !ok {
		return &unlinkedRootEscape{err: err}
	}
	return mc.raiseToHandlers(p.escape.Condition, false, handlers, endedRootReturnedMessage(p.escape))
}

// endedRootReturnedMessage is the secondary exception's message when a handler
// returns from escape, a raise out of an expansion-time root raised again after
// the root has ended: for a raise-continuable, that it cannot be resumed, and
// where it was raised, rather than that the program raised a non-continuable
// exception.
func endedRootReturnedMessage(escape *ErrExceptionEscape) string {
	if !escape.Continuable {
		return handlerReturnedMessage
	}
	q := "exception handler returned from raise-continuable"
	loc := escape.Location()
	if loc != "" {
		q += " at " + loc
	}
	return q + ", in a transformer or library body that has ended, so it cannot be resumed"
}

// ReRaiseOutOfEndedRoot is a guard's re-raise of cond when its clauses all miss
// and k, the continuation of its handler, was captured under an expansion-time
// root that has ended: the raise came out of the root to a guard of the caller's,
// and the guard's escape to its clauses ended the root. R7RS 4.2.7 re-raises in
// the dynamic environment of the original raise, by re-entering k; the root
// cannot be re-entered, so the raise is made here, in the guard's continuation,
// to the same handlers (those outside the guard). A handler that returns is told
// so, as for an uncaught raise out of an ended root (unlinkedRootEscape.raiseIn):
// a raise-continuable has nothing to resume. raised is false when k's root is
// live, or not an expansion-time root, and the guard re-enters k.
func ReRaiseOutOfEndedRoot(mc *MachineContext, k values.Value, cond values.Value) (raised bool, err error) {
	capt, ok := k.(*CapturedContinuation)
	if !ok || capt.cc.root == nil || !capt.cc.root.ended() {
		return false, nil
	}
	returned := handlerReturnedMessage
	escape := capt.cc.root.raisedToCaller
	if escape != nil && values.Eqv(escape.Condition, cond) {
		returned = endedRootReturnedMessage(escape)
	}
	return true, mc.raiseToHandlers(cond, false, mc.ResolveParameterValue(exceptionHandlerParam), returned)
}

// callerHandlersKey carries, on the ctx an expansion runs under, the
// expansionCaller of the runtime call that started it
// (ContextWithCallerHandlers). A nil *expansionCaller under the key hides an
// outer one (inheritCallerHandlers).
type callerHandlersKey struct{}

// expansionCaller is what an expansion-time root takes from the runtime call
// that started its expansion: that call's dynamic environment (its reachable
// marks: the handler stack and every parameterization), the handler stack among
// them, and its thread, on whose goroutine the root runs, so that a handler
// escaping from the root to the caller's continuation is not refused as a
// cross-thread escape.
//
// The environment is read from the caller when a root first needs it (a raise,
// a parameter read), not when the expansion starts: an eval that reads no mark
// walks none of the caller's chain, whatever its depth. That is sound because
// the caller is parked in the primitive for the whole synchronous expansion, so
// its chain and marks are as they were; the snapshot is then kept, so a read
// after the caller has moved on (none is known) sees the same environment.
// from and cont are the caller and its chain when the expansion started, to
// check that.
type expansionCaller struct {
	from     *MachineContext
	cont     *MachineContinuation
	marks    []markEntry
	handlers values.Value
	read     bool
	thread   *values.Thread
	threadID uint64
}

// environment returns the caller's reachable marks and its handler stack,
// reading them from the caller on the first call.
func (p *expansionCaller) environment() ([]markEntry, values.Value) {
	if p.read {
		return p.marks, p.handlers
	}
	if p.from.cont != p.cont || p.from.ctx == nil {
		panic(werr.WrapForeignErrorf(werr.ErrInvariantViolation,
			"expansion root: its caller moved on before the root first read the caller's dynamic environment"))
	}
	p.marks = p.from.collectReachableMarks()
	p.handlers = lookupMark(p.marks, exceptionHandlerParam)
	if p.handlers == nil {
		p.handlers = exceptionHandlerParam.Value()
	}
	p.read = true
	p.from = nil
	return p.marks, p.handlers
}

// callerMarks returns p's caller's dynamic environment when p is an
// expansion-time root that inherited one, nil otherwise.
func (p *MachineContext) callerMarks() []markEntry {
	if p.caller == nil {
		return nil
	}
	q, _ := p.caller.environment()
	return q
}

// callerHandlers returns the handler stack p, an expansion-time root, inherited
// from its caller, nil when it inherited none.
func (p *MachineContext) callerHandlers() values.Value {
	if p.caller == nil {
		return nil
	}
	_, q := p.caller.environment()
	return q
}

// ContextWithCallerHandlers returns the ctx for an expansion a primitive
// running on p starts (eval, load, environment, compile, expand, ...): p's ctx,
// carrying p's dynamic environment. Each expansion-time root the expansion runs
// inherits it below its own (inheritCallerHandlers), so a raise its own handlers
// do not take is handled in its dynamic extent, while its dynamic-winds are
// live, and the handler runs with the caller's parameterization, as when the
// expansion shares the caller's continuation (R7RS 6.11; Chez, Racket). Without
// it, a root raises in no handlers and the caller's are consulted only after the
// root has ended (MarkUnlinkedRootEscape).
func (p *MachineContext) ContextWithCallerHandlers() context.Context {
	return context.WithValue(p.ctx, callerHandlersKey{}, &expansionCaller{
		from:     p,
		cont:     p.cont,
		thread:   p.thread,
		threadID: p.threadID,
	})
}

// inheritCallerHandlers gives p, an expansion-time root, the dynamic
// environment and thread identity ctx carries (ContextWithCallerHandlers), if
// any, and reports whether it did. It hides them from p's own ctx: an expansion
// a primitive on p starts must inherit p's live handlers, which a primitive
// passes with ContextWithCallerHandlers; one that passes p's ctx as it is gets
// none, and the after-the-root bridge, rather than the outer caller's.
func (p *MachineContext) inheritCallerHandlers(ctx context.Context) bool {
	caller, _ := ctx.Value(callerHandlersKey{}).(*expansionCaller)
	if caller == nil {
		return false
	}
	p.caller = caller
	p.thread = caller.thread
	p.threadID = caller.threadID
	p.ctx = context.WithValue(p.ctx, callerHandlersKey{}, (*expansionCaller)(nil))
	return true
}

// inheritingRoot returns the expansion-time root p runs under when handlers is
// the stack that root inherited from its caller, so a raise to handlers has
// left the root's own; nil otherwise. The root is rootContext's: an
// expansion-time root has no parent and is not driven by RunResumable, so the
// walk ends there whenever it reaches it; one that stops earlier, at a context
// RunResumable drives, is at a root that inherited nothing.
func (p *MachineContext) inheritingRoot(handlers values.Value) *MachineContext {
	root := p.rootContext()
	inherited := root.callerHandlers()
	if inherited == nil || !values.Eqv(inherited, handlers) {
		return nil
	}
	return root
}

// ownHandlers is p's handler stack less the part its expansion-time root
// inherited from its caller: the handlers a raise of the caller's own ctx
// ending is given (applyCallableError). The caller's handlers do not see that
// ending raised inside an expansion: it is the caller's, and it ends the
// caller's run. A stack that does not end in the inherited one (rebound
// wholesale) is returned whole.
func (p *MachineContext) ownHandlers() values.Value {
	handlers := p.ResolveParameterValue(exceptionHandlerParam)
	inherited := p.rootContext().callerHandlers()
	if inherited == nil {
		return handlers
	}
	var own []values.Value
	for h := handlers; !values.Eqv(h, inherited); {
		pair, ok := h.(*values.Pair)
		if !ok {
			return handlers
		}
		own = append(own, pair.Car())
		h = pair.Cdr()
	}
	return values.List(own...)
}

// settleRootEscape is the mark of an uncaught raise out of a root that
// inherited its caller's handlers (inheritCallerHandlers): they have run on it
// already, so it is uncaught from here out, and no bridge raises it again.
func settleRootEscape(err error) error {
	var escape *ErrExceptionEscape
	if !errors.As(err, &escape) {
		return err
	}
	return &unlinkedRootEscape{err: err}
}

// raisedToCallerError is an expansion-time root's failure with the last raise
// its own handlers did not take (boundaryToken.raisedToCaller).
type raisedToCallerError struct {
	err   error
	raise *ErrExceptionEscape
}

// Error implements the error interface.
func (p *raisedToCallerError) Error() string {
	return p.err.Error()
}

// Unwrap returns the root's failure.
func (p *raisedToCallerError) Unwrap() error {
	return p.err
}

// RaisedToCaller returns the last raise an expansion-time root's own handlers
// did not take, if err, the root's failure (EvalTemplate), records one, marked
// for the next caller's handlers (MarkUnlinkedRootEscape); nil otherwise. A
// library body ended by a raise its importer's handler escaped from fails with
// that escape, a control signal, which later importers must not receive
// (memoizableInstantiateError): they get the raise.
func RaisedToCaller(err error) error {
	var raised *raisedToCallerError
	if !errors.As(err, &raised) {
		return nil
	}
	return MarkUnlinkedRootEscape(raised.raise)
}
