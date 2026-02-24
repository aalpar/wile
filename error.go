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
type CompilationError struct {
	Message string
	Cause   error
}

func (p *CompilationError) Error() string {
	if p.Cause != nil {
		return p.Message + ": " + p.Cause.Error()
	}
	return p.Message
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

// isEOF checks if an error represents end of input.
func isEOF(err error) bool {
	return errors.Is(err, io.EOF)
}
