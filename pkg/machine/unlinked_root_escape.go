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
)

// unlinkedRootEscape marks an uncaught raise out of a root that ran
// synchronously for the caller but is not linked to its chain, so it searched
// none of the caller's handlers: an expansion-time root, which runs a macro
// transformer, a phase-1 expression or a library body. Its
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
func (p *unlinkedRootEscape) raiseIn(mc *MachineContext, err error) error {
	handlers := mc.ResolveParameterValue(exceptionHandlerParam)
	_, ok := handlers.(*values.Pair)
	if !ok {
		return &unlinkedRootEscape{err: err}
	}
	return mc.raiseToHandlers(p.escape.Condition, false, handlers)
}
