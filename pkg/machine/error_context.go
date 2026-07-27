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
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

var _ values.Value = (*ErrorContext)(nil)

// ErrorContext carries diagnostic information captured at a raise site.
// It is attached as a continuation mark during exception handler dispatch,
// enabling Scheme code to inspect source location, stack trace, and
// continuation marks at the point where the exception was raised.
type ErrorContext struct {
	source     *syntax.SourceContext
	stackTrace StackTrace
	marks      *ContinuationMarkSet
}

// NewErrorContext creates an ErrorContext with the given diagnostics.
func NewErrorContext(
	source *syntax.SourceContext,
	stackTrace StackTrace,
	marks *ContinuationMarkSet,
) *ErrorContext {
	q := &ErrorContext{
		source:     source,
		stackTrace: stackTrace,
		marks:      marks,
	}
	return q
}

// Source returns the source location at the raise site.
func (p *ErrorContext) Source() *syntax.SourceContext {
	if p == nil {
		return nil
	}
	return p.source
}

// SourceLocation returns the formatted source location string ("file:line:col"),
// or "" if unavailable.
func (p *ErrorContext) SourceLocation() string {
	if p == nil || p.source == nil {
		return ""
	}
	return p.source.Location()
}

// StackTraceFrames returns the stack trace captured at the raise site.
func (p *ErrorContext) StackTraceFrames() StackTrace {
	if p == nil {
		return nil
	}
	return p.stackTrace
}

// Marks returns the continuation mark set snapshot from the raise site,
// or nil if not captured.
func (p *ErrorContext) Marks() *ContinuationMarkSet {
	if p == nil {
		return nil
	}
	return p.marks
}

// SchemeString returns the Scheme representation of the error context.
func (p *ErrorContext) SchemeString() string {
	loc := p.SourceLocation()
	if loc != "" {
		return "#<error-context " + loc + ">"
	}
	return "#<error-context>"
}

// IsVoid returns true if the receiver is nil.
func (p *ErrorContext) IsVoid() bool {
	return p == nil
}

// EqualTo uses pointer identity. ErrorContext values are not structurally
// comparable — each raise site produces a unique context.
func (p *ErrorContext) EqualTo(other values.Value) bool {
	o, ok := other.(*ErrorContext)
	if !ok {
		return false
	}
	return p == o
}

// errorContextKeyType is a private sentinel type used as the continuation mark
// key for error context. Because it is unexported, Scheme code cannot forge it:
// identity is type identity under values.EqIdentity, not string comparison.
type errorContextKeyType struct{}

func (errorContextKeyType) SchemeString() string {
	return "#<error-context-key>"
}

func (errorContextKeyType) IsVoid() bool {
	return false
}

func (errorContextKeyType) EqualTo(other values.Value) bool {
	_, ok := other.(errorContextKeyType)
	return ok
}

// errorContextKey is the singleton mark key. Compared by type identity —
// no Scheme code can construct an errorContextKeyType.
var errorContextKey errorContextKeyType

// ErrorContextKey returns the continuation mark key for error context.
// Exposed for use by registry primitives that read the mark.
func ErrorContextKey() values.Value {
	return errorContextKey
}
