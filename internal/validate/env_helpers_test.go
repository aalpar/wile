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

package validate

import (
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"

	qt "github.com/frankban/quicktest"
)

// TestCreateChildEnvWithSymbols_EmptyAlwaysFrames pins the invariant that
// createChildEnvWithSymbols always returns a fresh child frame, even when
// the symbol slice is empty. This is the property that distinguishes it
// from extendEnvWithSymbols (which returns env unchanged on empty input)
// and is the reason createLambdaValidationEnv uses the primitive directly.
// If a future refactor were to "simplify" lambda by switching to
// extendEnvWithSymbols, this test would fail and surface the regression.
func TestCreateChildEnvWithSymbols_EmptyAlwaysFrames(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	child := createChildEnvWithSymbols(env, nil)
	qt.Assert(t, child != env, qt.IsTrue,
		qt.Commentf("createChildEnvWithSymbols must allocate a fresh child frame even for empty syms"))
}

// TestExtendEnvWithSymbols_EmptyReturnsEnv pins the let-family short-circuit:
// extendEnvWithSymbols with empty input returns env unchanged (avoiding
// the empty frame that a zero-binding let/let*/letrec doesn't need).
func TestExtendEnvWithSymbols_EmptyReturnsEnv(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	got := extendEnvWithSymbols(env, nil)
	qt.Assert(t, got == env, qt.IsTrue,
		qt.Commentf("extendEnvWithSymbols must return env unchanged for empty syms"))
	got = extendEnvWithSymbols(env, []*syntax.SyntaxSymbol{})
	qt.Assert(t, got == env, qt.IsTrue,
		qt.Commentf("extendEnvWithSymbols must return env unchanged for empty-non-nil syms"))
}

// TestCreateLambdaValidationEnv_ZeroParamsCreatesFrame pins the contract
// stated in createLambdaValidationEnv's doc comment: '(lambda () body)' must
// receive a fresh child frame, not the outer env. This matches the per-
// lambda frame discipline the expander and compiler observe.
func TestCreateLambdaValidationEnv_ZeroParamsCreatesFrame(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	got := createLambdaValidationEnv(env, &ValidatedParams{})
	qt.Assert(t, got != env, qt.IsTrue,
		qt.Commentf("(lambda () body) must allocate a fresh child frame"))
}
