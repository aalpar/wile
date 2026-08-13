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
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestEvalWhen_MultiExpressionBody is the must-fail-first gate for the OpPop
// underflow in evalWhenCompileForRuntime.
//
// The compiler discarded each non-final body result with OpPop, but
// CompileExpression leaves its result in the VALUE REGISTER, not on the eval
// stack. The Pop therefore drained a stack entry nobody had pushed. At
// 48a52a3c the second row below printed 1 and then panicked with
// "Stack.Pop: stack is empty".
//
// Only the eval situation was affected: the compile situation does not run the
// body's instructions through the VM, so it printed 12 the whole time. A gate
// written on (eval-when (compile) ...) alone would have stayed green.
func TestEvalWhen_MultiExpressionBody(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "single-expression body is the value",
			src:  "(eval-when (eval) 7)",
			want: "7",
		},
		{
			// 48a52a3c: panic "Stack.Pop: stack is empty"
			name: "multi-expression body yields the last value",
			src:  "(eval-when (eval) 1 2 7)",
			want: "7",
		},
		{
			// 48a52a3c: panic, after the first display had already run
			name: "multi-expression body with effects",
			src:  "(eval-when (eval) (define ew-a 1) (define ew-b 2) (+ ew-a ew-b))",
			want: "3",
		},
		{
			name: "compile situation was never affected",
			src:  "(eval-when (compile) 1 2 7)",
			want: "#<void>",
		},
	}

	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			c.Assert(evalString(t, tt.src), qt.Equals, tt.want)
		})
	}
}
