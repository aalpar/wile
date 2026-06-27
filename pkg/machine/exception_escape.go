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
	"strings"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// ErrExceptionEscape carries an UNCAUGHT Scheme exception out to the embedder.
// It is produced by RaiseInPlace when the exception-handler stack is empty (no
// handler caught the condition) and bubbles to RunWithEscapeHandling, where the
// public Engine turns it into a wile.RuntimeError (engine.wrapRuntimeError reads
// Condition / Source / StackTrace). Handler dispatch itself no longer flows through
// this type — handlers ride the %exception-handlers parameter and are invoked in
// place by RaiseInPlace (see exception_raise.go), so the old handler-machinery
// fields (Continuable / Continuation / Handled / WindingStack) are gone.
type ErrExceptionEscape struct {
	Condition  values.Value          // The raised condition/object
	Source     *syntax.SourceContext // Source location where exception was raised
	StackTrace StackTrace            // VM stack trace at raise point
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
	loc := p.Source.Location()
	hasSource := loc != ""
	if hasSource {
		b.WriteString(loc)
		b.WriteString(": ")
	}

	// Format the condition
	if p.Condition == nil {
		b.WriteString("exception: <nil>")
	} else {
		ne, ok := p.Condition.(*values.NativeError)
		if ok && hasSource {
			// NativeError with source: use clean "error: message" format
			b.WriteString("error: ")
			b.WriteString(ne.Error())
		} else {
			// Non-NativeError or no source: use SchemeString for backward compat
			b.WriteString("exception: ")
			b.WriteString(p.Condition.SchemeString())
		}
	}

	// Append stack trace if present
	if len(p.StackTrace) > 0 {
		b.WriteString("\n")
		b.WriteString(p.StackTrace.String())
	}

	return b.String()
}

// ConditionText returns the raised condition's human-readable text alone —
// without the source-location prefix, the "error:"/"exception:" category word,
// or the stack trace that Error() adds. Callers that render their own prefix
// (the REPL's "Exception:", the CLI's "Error:", wile.RuntimeError's structured
// Source/StackTrace fields) use this to surface the message exactly once
// instead of embedding the whole Error() string and duplicating location and
// trace. A *NativeError yields its plain message; any other condition yields
// its SchemeString; a nil condition yields "<nil>".
//
// Deliberately NOT shared with Error(): Error() gates a *NativeError's plain
// message on hasSource (a *NativeError with no source falls back to SchemeString
// there, for backward compatibility), whereas ConditionText always uses the
// plain message since it never pairs with a source prefix. The two ladders stay
// separate on purpose — do not "unify" them without preserving that divergence.
func (p *ErrExceptionEscape) ConditionText() string {
	if p.Condition == nil {
		return "<nil>"
	}
	ne, ok := p.Condition.(*values.NativeError)
	if ok {
		return ne.Error()
	}
	return p.Condition.SchemeString()
}

// Unwrap returns the underlying error when the condition implements the error
// interface (e.g., *NativeError). This enables errors.Is/errors.As to traverse
// through ErrExceptionEscape into the wrapped error chain, supporting sentinel
// matching like errors.Is(err, werr.ErrDivisionByZero) from Go callers.
func (p *ErrExceptionEscape) Unwrap() error {
	e, ok := p.Condition.(error)
	if ok {
		return e
	}
	return nil
}
