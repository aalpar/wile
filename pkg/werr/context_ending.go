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

package werr

import (
	"context"
	"errors"
)

// ContextEnding is how an ended ctx says it ended, as the cause of an
// ErrOperationCancelled: its cause, with ctx.Err() kept matchable alongside when
// the cause does not already match it. A CancelCauseFunc given a host error makes
// the two differ (Err is context.Canceled, the cause is the host's), and an
// embedder may test either. via is the failure that reported the ending (a list
// walker's bare ctx.Err(), a host primitive's own wrap of it), kept reachable by
// errors.Is/As and rendered when its text says more than the cause's; nil when
// the caller noticed the ending itself. Nil while ctx is live.
//
// It lives here, the lowest package every reporter imports: the VM poll and
// bridge (pkg/machine), the SRFI-18 waits (pkg/values, extensions/threads) and
// the expander.
func ContextEnding(ctx context.Context, via error) error {
	err := ctx.Err()
	if err == nil {
		return nil
	}
	q := &contextEnding{cause: context.Cause(ctx)}
	if !errors.Is(q.cause, err) {
		q.err = err
	}
	if via != nil {
		q.via = via
		text := via.Error()
		q.viaShown = text != q.cause.Error() && text != err.Error()
	}
	if q.err == nil && q.via == nil {
		return q.cause
	}
	return q
}

// WaitCancelled is the failure a blocking wait reports when ctx ended before the
// wait completed: ErrOperationCancelled carrying ctx's ending, with site, the
// waiting primitive, leading the message. The SRFI-18 waits (extensions/threads)
// and process-wait (extensions/process) raise it, so one cancelled wait reads
// the same whichever primitive it was.
func WaitCancelled(ctx context.Context, site string) error {
	return WrapForeignErrorWithCause(ErrOperationCancelled, ContextEnding(ctx, nil),
		"%s: cancelled before the wait completed", site)
}

// contextEnding matches the cause, ctx.Err() when it differs from the cause, and
// via. It renders as the cause, which says more than "context canceled", led by
// via's text when via says something of its own (viaShown): a host primitive's
// "fetch http://x failed: context canceled" is the failure's whole story, and
// dropping it would leave only the class. A via that already matches the cause
// renders alone, since its text ends in the cause's.
type contextEnding struct {
	cause    error
	err      error // ctx.Err(), or nil when the cause matches it
	via      error
	viaShown bool
}

func (p *contextEnding) Error() string {
	if !p.viaShown {
		return p.cause.Error()
	}
	if errors.Is(p.via, p.cause) {
		return p.via.Error()
	}
	return p.via.Error() + ": " + p.cause.Error()
}

func (p *contextEnding) Unwrap() []error {
	q := []error{p.cause}
	if p.err != nil {
		q = append(q, p.err)
	}
	if p.via != nil {
		q = append(q, p.via)
	}
	return q
}

// messageParts is FailureMessage's projection of p: the parts of what Error
// renders, each projected in turn.
func (p *contextEnding) messageParts(acc []string) []string {
	if !p.viaShown {
		return messageParts(p.cause, acc)
	}
	acc = messageParts(p.via, acc)
	if errors.Is(p.via, p.cause) {
		return acc
	}
	return messageParts(p.cause, acc)
}
