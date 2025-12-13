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
	"wile/syntax"
	"wile/values"
)

// SyntaxCompilerFunc is the type for compile-time special form handlers.
// These functions handle syntax-directed compilation of special forms like
// `define`, `lambda`, `if`, `quote`, etc.
//
// Parameters:
//   - ctc: The compile-time continuation (compiler state)
//   - ctctx: The compile-time call context (tail position info, etc.)
//   - expr: The expression arguments (everything after the keyword)
//
// The function should emit operations to ctc.template and return nil on success.
type SyntaxCompilerFunc func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error

// SyntaxCompiler wraps a SyntaxCompilerFunc as a values.Value so it can
// be stored in the environment.
type SyntaxCompiler struct {
	name string
	fn   SyntaxCompilerFunc
}

// NewSyntaxCompiler creates a new syntax compiler.
func NewSyntaxCompiler(name string, fn SyntaxCompilerFunc) *SyntaxCompiler {
	return &SyntaxCompiler{name: name, fn: fn}
}

// Name returns the name of this syntax compiler.
func (p *SyntaxCompiler) Name() string {
	return p.name
}

// Compile invokes the syntax compiler function.
func (p *SyntaxCompiler) Compile(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error {
	return p.fn(ctc, ctctx, expr)
}

// SchemeString implements values.Value interface.
func (p *SyntaxCompiler) SchemeString() string {
	return "#<syntax-compiler:" + p.name + ">"
}

// IsVoid implements values.Value interface.
func (p *SyntaxCompiler) IsVoid() bool {
	return false
}

// EqualTo implements values.Value interface.
func (p *SyntaxCompiler) EqualTo(other values.Value) bool {
	if other == nil {
		return false
	}
	otherPC, ok := other.(*SyntaxCompiler)
	if !ok {
		return false
	}
	return p.name == otherPC.name
}
