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
	"errors"

	"github.com/aalpar/wile/pkg/values"
)

// ErrUnlinkedRootEscape marks an uncaught raise out of a root that ran
// synchronously for the caller but is not linked to its chain, so it searched
// none of the caller's handlers: a library body being instantiated. Its
// *ErrExceptionEscape means "uncaught on that root", not "uncaught": the first
// foreign-call bridge it reaches raises the condition in the caller's handlers
// (applyCallableError), as thread-join! does for a joinee's uncaught condition.
//
// Err is the full chain, escape included, so errors.As still finds the
// *ErrExceptionEscape and the embedder still sees an uncaught raise when no
// handler takes it.
type ErrUnlinkedRootEscape struct {
	Err    error
	escape *ErrExceptionEscape
}

// MarkUnlinkedRootEscape wraps err in an ErrUnlinkedRootEscape when it carries
// an uncaught raise, and returns it unchanged otherwise.
func MarkUnlinkedRootEscape(err error) error {
	var escape *ErrExceptionEscape
	if !errors.As(err, &escape) {
		return err
	}
	return &ErrUnlinkedRootEscape{Err: err, escape: escape}
}

// Error implements the error interface.
func (p *ErrUnlinkedRootEscape) Error() string {
	return p.Err.Error()
}

// Unwrap returns the marked chain.
func (p *ErrUnlinkedRootEscape) Unwrap() error {
	return p.Err
}

// raiseIn raises the escape's condition in mc's handlers. With none, the raise is
// uncaught here too, and the chain goes on WITHOUT the mark: an outer bridge,
// such as one below a handler that is running now and so is not in mc's handlers,
// must not raise it a second time.
func (p *ErrUnlinkedRootEscape) raiseIn(mc *MachineContext) error {
	handlers := mc.ResolveParameterValue(exceptionHandlerParam)
	_, ok := handlers.(*values.Pair)
	if !ok {
		return p.Err
	}
	return mc.raiseToHandlers(p.escape.Condition, false, handlers)
}
