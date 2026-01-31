// Copyright 2025 Aaron Alpar
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

	"wile/syntax"
	"wile/values"
)

// SchemeError is a runtime error with Scheme-level stack trace.
type SchemeError struct {
	Message    string
	Source     *syntax.SourceContext // Where error occurred
	StackTrace string                // Formatted stack trace
	Cause      error                 // Underlying error (if any)
}

func NewSchemeError(msg string, source *syntax.SourceContext, stackTrace string) *SchemeError {
	return &SchemeError{
		Message:    msg,
		Source:     source,
		StackTrace: stackTrace,
	}
}

func NewSchemeErrorWithCause(msg string, source *syntax.SourceContext, stackTrace string, cause error) *SchemeError {
	return &SchemeError{
		Message:    msg,
		Source:     source,
		StackTrace: stackTrace,
		Cause:      cause,
	}
}

func (p *SchemeError) Error() string {
	var b strings.Builder

	// Location prefix
	if p.Source != nil {
		fmt.Fprintf(&b, "%s:%d:%d: ",
			p.Source.File,
			p.Source.Start.Line(),
			p.Source.Start.Column())
	}

	// Message
	b.WriteString(p.Message)

	// Stack trace
	if p.StackTrace != "" {
		b.WriteString("\n")
		b.WriteString(p.StackTrace)
	}

	return b.String()
}

func (p *SchemeError) Unwrap() error {
	return p.Cause
}

// SchemeString returns the Scheme representation.
func (p *SchemeError) SchemeString() string {
	return fmt.Sprintf("#<error: %s>", p.Message)
}

// IsVoid returns false (errors are not void).
func (p *SchemeError) IsVoid() bool {
	return false
}

// EqualTo compares for equality.
func (p *SchemeError) EqualTo(o values.Value) bool {
	other, ok := o.(*SchemeError)
	if !ok {
		return false
	}
	return p.Message == other.Message
}
