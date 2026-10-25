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

	"github.com/aalpar/wile/pkg/werr"
)

// CancelledRun is the error a run reports when it stops because ctx ended
// outside any wait: ErrOperationCancelled, the class a cancelled SRFI-18 wait
// raises, carrying ctx's ending (werr.ContextEnding, with via, the failure that
// reported it, or nil). site names the check that noticed, "operation: what
// failed" style. The error carries p's current source and trace, which locate
// the stop only while p is running: the VM poll and apply's argument spread are
// located, and the Engine's end-of-run check, run after p finished, is not.
func (p *MachineContext) CancelledRun(ctx context.Context, site string, via error) *SchemeError {
	// "" so the message is the full chain's text: SchemeError.Error renders its
	// Message but not its Cause, and the cause is what tells a deadline from a cancel.
	return p.WrapError(werr.WrapForeignErrorWithCause(werr.ErrOperationCancelled, werr.ContextEnding(ctx, via), "%s", site), "")
}

// cancelledFailure is the failure the foreign-call bridge dispatches for err when
// err is p's ctx ending reported bare (a list walker's ctx.Err(), as length
// returns it): ErrOperationCancelled with the ending, the class a parked wait
// raises, so a handler and the embedder see one shape whichever step noticed.
// The bridge's raise locates it. Any other err, including a wait's own
// ErrOperationCancelled, is returned unchanged.
func (p *MachineContext) cancelledFailure(err error) error {
	if errors.Is(err, werr.ErrOperationCancelled) || !CancelledBy(p.ctx, err) {
		return err
	}
	return werr.WrapForeignErrorWithCause(werr.ErrOperationCancelled, werr.ContextEnding(p.ctx, err),
		"cancelled before the primitive returned")
}
