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
// walker's bare ctx.Err()), kept reachable by errors.Is/As, or nil when the
// caller noticed the ending itself. Nil while ctx is live.
//
// It lives here, the lowest package every reporter imports: the VM poll and
// bridge (pkg/machine), the SRFI-18 waits (pkg/values, extensions/threads) and
// the expander.
func ContextEnding(ctx context.Context, via error) error {
	err := ctx.Err()
	if err == nil {
		return nil
	}
	cause := context.Cause(ctx)
	var also []error
	if !errors.Is(cause, err) {
		also = append(also, err)
	}
	if via != nil {
		also = append(also, via)
	}
	if len(also) == 0 {
		return cause
	}
	return &contextEnding{cause: cause, also: also}
}

// contextEnding renders as the cause alone, which says more than "context
// canceled", and matches the cause and everything in also.
type contextEnding struct {
	cause error
	also  []error
}

func (p *contextEnding) Error() string {
	return p.cause.Error()
}

func (p *contextEnding) Unwrap() []error {
	return append([]error{p.cause}, p.also...)
}
