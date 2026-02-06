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

import "io"

// Error represents a Wile engine error, including initialization failures.
type Error struct {
	Message string
	Cause   error
}

func (p *Error) Error() string {
	if p.Cause != nil {
		return p.Message + ": " + p.Cause.Error()
	}
	return p.Message
}

func (p *Error) Unwrap() error {
	return p.Cause
}

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
// If the error originated from a Scheme raise/raise-continuable,
// Condition holds the raised value.
type RuntimeError struct {
	Message   string
	Cause     error
	Condition Value // non-nil when Scheme raise produced the error
}

func (p *RuntimeError) Error() string {
	if p.Cause != nil {
		return p.Message + ": " + p.Cause.Error()
	}
	return p.Message
}

func (p *RuntimeError) Unwrap() error {
	return p.Cause
}

// isEOF checks if an error represents end of input.
func isEOF(err error) bool {
	return err == io.EOF
}
