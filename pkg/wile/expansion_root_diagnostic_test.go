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

package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestClosureTransformerFailureIsNotLabelledSyntaxRules: every closure
// transformer (a lambda, a syntax-case or a Scheme-layer ER transformer) is
// invoked through one wrap, which named syntax-rules whatever the transformer
// was.
func TestClosureTransformerFailureIsNotLabelledSyntaxRules(t *testing.T) {
	t.Parallel()

	for _, tc := range []struct {
		name, src string
	}{
		{"lambda transformer", `(import (for-syntax (scheme base))) (define-syntax m (lambda (s) (car 5))) (m)`},
		{"syntax-case transformer", `(import (for-syntax (scheme base))) (define-syntax m (lambda (s) (syntax-case s () ((_) (car 5))))) (m)`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			_, err := newExpansionRootProbeEngine(t).EvalMultiple(context.Background(), tc.src)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, err.Error(), qt.Contains, "transformer invocation failed")
			qt.Assert(t, err.Error(), qt.Not(qt.Contains), "syntax-rules")
		})
	}
}
