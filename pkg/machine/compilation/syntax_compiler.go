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

package compilation

import (
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// SyntaxCompilerFunc is the type for compile-time special form handlers.
// These functions handle syntax-directed compilation of extension forms like
// `syntax-case`, `import`, `define-syntax`, `include`, etc.
//
// Parameters:
//   - ctc: The compile-time continuation (compiler state)
//   - ctctx: The compile-time call context (tail position info, etc.)
//   - expr: The expression arguments (everything after the keyword)
//
// The function should emit operations via ctc.AppendOperations and return nil on success.
type SyntaxCompilerFunc func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, expr syntax.SyntaxValue) error

// SyntaxCompiler wraps a SyntaxCompilerFunc as a values.Value so it can
// be stored in the environment.
type SyntaxCompiler struct {
	namedHandlerBase
	fn SyntaxCompilerFunc
}

// NewSyntaxCompiler creates a new syntax compiler.
func NewSyntaxCompiler(name string, fn SyntaxCompilerFunc) *SyntaxCompiler {
	return &SyntaxCompiler{
		name: name, prefix: "syntax-compiler",
		fn: fn,
	}
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
