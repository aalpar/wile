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

package testutil_test

import (
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/machine/testutil"
)

func TestSetupEngineTest_SmokePlainArithmetic(t *testing.T) {
	env := testutil.SetupEngineTest(t, nil)
	result := testutil.EvalSchemeInEnv(t, env, "(+ 1 2)")
	qt.Assert(t, result.SchemeString(), qt.Equals, "3")
}

func TestSetupEngineTest_LibraryImport(t *testing.T) {
	fs := fstest.MapFS{
		"test/greet.sld": &fstest.MapFile{
			Data: []byte(`(define-library (test greet)
  (export greeting)
  (import (scheme base))
  (begin (define greeting "hello")))`),
		},
	}
	env := testutil.SetupEngineTest(t, fs)
	result := testutil.EvalSchemeInEnv(t, env, `(import (test greet)) greeting`)
	qt.Assert(t, result.SchemeString(), qt.Equals, `"hello"`)
}

func TestEvalSchemeInEnvMayFail_ReturnsError(t *testing.T) {
	env := testutil.SetupEngineTest(t, nil)
	_, err := testutil.EvalSchemeInEnvMayFail(t, env, "(car 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestEvalSchemeInEnvMayFail_Success(t *testing.T) {
	env := testutil.SetupEngineTest(t, nil)
	result, err := testutil.EvalSchemeInEnvMayFail(t, env, "(+ 1 2)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "3")
}
