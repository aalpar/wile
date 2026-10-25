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
// raises, carrying ctx's ending (ContextEnding) and located at p's current
// instruction. site names the check that noticed, "operation: what failed" style.
// The VM poll is one such check; the Engine's end-of-run check is the other.
func (p *MachineContext) CancelledRun(ctx context.Context, site string) *SchemeError {
	// "" so the message is the full chain's text: SchemeError.Error renders its
	// Message but not its Cause, and the cause is what tells a deadline from a cancel.
	return p.WrapError(werr.WrapForeignErrorWithCause(werr.ErrOperationCancelled, ContextEnding(ctx), "%s", site), "")
}

// ContextEnding is how an ended ctx says it ended: its cause, with ctx.Err() kept
// matchable alongside when the cause does not already match it. A
// CancelCauseFunc given a host error makes the two differ (Err is
// context.Canceled, the cause is the host's), and an embedder may test either.
// Nil while ctx is live.
func ContextEnding(ctx context.Context) error {
	err := ctx.Err()
	if err == nil {
		return nil
	}
	cause := context.Cause(ctx)
	if errors.Is(cause, err) {
		return cause
	}
	return &contextEnding{cause: cause, err: err}
}

// contextEnding renders as the cause alone, which says more than "context
// canceled", and matches both.
type contextEnding struct {
	cause error
	err   error
}

func (p *contextEnding) Error() string {
	return p.cause.Error()
}

func (p *contextEnding) Unwrap() []error {
	return []error{p.cause, p.err}
}
