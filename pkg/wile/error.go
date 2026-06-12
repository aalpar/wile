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

package wile

import (
	"errors"
	"io"
	"strings"
)

// CompilationError wraps errors from parsing, expanding, or compiling Scheme code.
//
// # Source
//
// Source provides the source location ("file:line:col") where the error occurred,
// when available. Compilation errors from the core compiler include source locations;
// parse errors and some edge cases may have an empty Source.
type CompilationError struct {
	Message string
	Source  string // formatted source location ("file:line:col"), empty if unavailable
	Cause   error
}

func (p *CompilationError) Error() string {
	var b strings.Builder
	if p.Source != "" {
		b.WriteString(p.Source)
		b.WriteString(": ")
	}
	b.WriteString(p.Message)
	if p.Cause != nil {
		b.WriteString(": ")
		b.WriteString(p.Cause.Error())
	}
	return b.String()
}

func (p *CompilationError) Unwrap() error {
	return p.Cause
}

// RuntimeError wraps errors from executing Scheme code.
//
// # Condition
//
// When the error originated from a Scheme raise or raise-continuable,
// Condition holds the raised value and [RuntimeError.IsSchemeException]
// returns true. When the error originated from Go code (VM errors,
// primitive failures, type mismatches), Condition is nil.
//
// # Source and Stack Trace
//
// Source and StackTrace provide the source location and VM stack trace
// at the point of the error. Both are empty strings when per-operation
// source tracking is unavailable.
//
// # Cause
//
// Cause may contain internal machine types. Callers should treat it as
// an opaque error suitable for logging and [errors.Is]/[errors.As]
// matching, not for direct type inspection.
type RuntimeError struct {
	Message    string
	Cause      error
	Condition  Value  // non-nil when Scheme raise produced the error; nil for VM/primitive errors
	Source     string // formatted source location ("file:line:col"), empty if unavailable
	StackTrace string // formatted VM stack trace, empty if unavailable
}

func (p *RuntimeError) Error() string {
	var b strings.Builder

	if p.Source != "" {
		b.WriteString(p.Source)
		b.WriteString(": ")
	}

	b.WriteString(p.Message)

	if p.Cause != nil {
		b.WriteString(": ")
		b.WriteString(p.Cause.Error())
	}

	if p.StackTrace != "" {
		b.WriteString("\n")
		b.WriteString(p.StackTrace)
	}

	return b.String()
}

func (p *RuntimeError) Unwrap() error {
	return p.Cause
}

// IsSchemeException reports whether this error originated from a Scheme
// raise or raise-continuable expression. When true, Condition holds
// the raised value.
func (p *RuntimeError) IsSchemeException() bool {
	return p != nil && p.Condition != nil
}

// newRuntimeError creates a RuntimeError with only a message.
func newRuntimeError(msg string) *RuntimeError {
	return &RuntimeError{Message: msg}
}

// newRuntimeErrorWithCause creates a RuntimeError with a message and an underlying cause.
func newRuntimeErrorWithCause(msg string, cause error) *RuntimeError {
	return &RuntimeError{Message: msg, Cause: cause}
}

// IsIncompleteInput reports whether a parse error indicates the input
// is a valid prefix of an expression that needs more input to complete.
// This is useful for REPL implementations that accumulate multi-line input.
//
// Detection uses errors.Is where possible (wrapped io.EOF for truncated
// input). String matching is used only for tokenizer/parser errors whose
// types are internal and cannot be matched structurally from public code.
// Returns false for nil and bare io.EOF.
func IsIncompleteInput(err error) bool {
	if err == nil {
		return false
	}
	// Bare io.EOF means "stream ended normally" — not incomplete.
	// Wrapped io.EOF (e.g. CompilationError{Cause: io.EOF}) means
	// the parser ran out of input mid-expression.
	if err != io.EOF && errors.Is(err, io.EOF) {
		return true
	}
	// The tokenizer and parser live in internal packages, so their error
	// types cannot be matched with errors.Is/As from public code. Fall
	// back to string matching for these specific patterns:
	//   - "unterminated" — tokenizer: unterminated string/symbol
	//   - "unclosed"     — tokenizer: unclosed block comment
	//   - "unknown token type" — parser: partial token from premature EOF
	errStr := err.Error()
	return strings.Contains(errStr, "unterminated") ||
		strings.Contains(errStr, "unclosed") ||
		strings.Contains(errStr, "unknown token type")
}

// isEOF checks if an error represents end of input.
func isEOF(err error) bool {
	return errors.Is(err, io.EOF)
}
