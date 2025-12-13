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
	"context"
	"testing"

	"wile/environment"

	qt "github.com/frankban/quicktest"
)

// Tests moved from coverage_additional_test.go
// TestExtractLiteralsFromSyntaxRules tests extractLiterals function via syntax-rules
func TestExtractLiteralsFromSyntaxRules(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Macro with literal identifiers
	macroCode := `(define-syntax when-test
		(syntax-rules (then)
			((_ cond then body) (if cond body #f))))`
	sv := parseSchemeExpr(t, env, macroCode)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}
