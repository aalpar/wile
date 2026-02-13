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
	"fmt"
	"strings"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

// ErrExceptionEscape signals an exception being raised through the call stack.
// It is used by raise and raise-continuable to propagate exceptions to handlers.
type ErrExceptionEscape struct {
	Condition    values.Value          // The raised condition/object
	Continuable  bool                  // Whether handler can return
	Continuation *MachineContinuation  // Return point for continuable exceptions
	Handled      bool                  // Set true after handler processes it
	WindingStack WindingStack          // Winding stack at raise point (for proper unwinding)
	Source       *syntax.SourceContext // Source location where exception was raised
	StackTrace   StackTrace            // VM stack trace at raise point
}

// Error implements the error interface.
//
// When Source is present and the condition is a NativeError, produces clean
// human-readable output like "file:5:3: error: division by zero".
// For non-NativeError conditions with Source, produces "file:5:3: exception: foo".
// When no Source is set, falls back to the original format for backward compat.
func (p *ErrExceptionEscape) Error() string {
	var b strings.Builder

	// Format the source prefix if available
	hasSource := p.Source != nil && p.Source.File != ""
	if hasSource {
		fmt.Fprintf(&b, "%s:%d:%d: ",
			p.Source.File,
			p.Source.Start.Line(),
			p.Source.Start.Column())
	}

	// Format the condition
	if p.Condition == nil {
		b.WriteString("exception: <nil>")
	} else if ne, ok := p.Condition.(*values.NativeError); ok && hasSource {
		// NativeError with source: use clean "error: message" format
		b.WriteString("error: ")
		b.WriteString(ne.Error())
	} else {
		// Non-NativeError or no source: use SchemeString for backward compat
		b.WriteString("exception: ")
		b.WriteString(p.Condition.SchemeString())
	}

	// Append stack trace if present
	if len(p.StackTrace) > 0 {
		b.WriteString("\n")
		b.WriteString(p.StackTrace.String())
	}

	return b.String()
}

// Unwrap returns the underlying error when the condition implements the error
// interface (e.g., *NativeError). This enables errors.Is/errors.As to traverse
// through ErrExceptionEscape into the wrapped error chain, supporting sentinel
// matching like errors.Is(err, values.ErrDivisionByZero) from Go callers.
func (p *ErrExceptionEscape) Unwrap() error {
	e, ok := p.Condition.(error)
	if ok {
		return e
	}
	return nil
}
