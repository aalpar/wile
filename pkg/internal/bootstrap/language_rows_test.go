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

package bootstrap_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/registry/testhelpers"
)

// The bootstrap sequence installs the same language rows as the engine's, so a
// phase-2 transformer body reaches the macro vocabulary in every owner it builds.
// Pinned here as well as in pkg/wile because most Go tests build their
// environments through this sequence and would not see it diverge.
const phaseTwoProgram = `
(define-syntax second-of
  (er-macro-transformer
    (lambda (form rename compare)
      (let-syntax ((second
                     (er-macro-transformer
                       (lambda (f r c) (list 'car (list 'cdr (car (cdr f))))))))
        (second form)))))`

func TestBootstrapOwnersHaveTheMacroVocabularyAtPhaseTwo(t *testing.T) {
	ctx := context.Background()
	env, err := bootstrap.NewNamespaceFrame(ctx)
	qt.Assert(t, err, qt.IsNil)

	_, err = testhelpers.RunSchemeCodeWithEnv(t, env, phaseTwoProgram)
	qt.Assert(t, err, qt.IsNil)
	v, err := testhelpers.RunSchemeCodeWithEnv(t, env, "(second-of 5 6)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "5")
}
