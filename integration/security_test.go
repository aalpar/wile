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

package integration_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	exteval "github.com/aalpar/wile/extensions/eval"
	extfiles "github.com/aalpar/wile/extensions/files"
	extsystem "github.com/aalpar/wile/extensions/system"
	extio "github.com/aalpar/wile/internal/extensions/io"
	"github.com/aalpar/wile/pkg/wile"
	"github.com/aalpar/wile/security"
)

// TestPrivilegedExtensionsDenyUnderDenyAll pins R12: every privileged extension
// (files, eval, system) must surface errors.Is(ErrAccessDenied) when its gate
// denies an operation. Previously only the process extension had a denial test,
// and even that used a weaker error-not-nil assertion — so a silently dropped
// gate in files/eval/system would regress unnoticed.
func TestPrivilegedExtensionsDenyUnderDenyAll(t *testing.T) {
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(extfiles.Extension),
		wile.WithExtension(exteval.Extension),
		wile.WithExtension(extsystem.Extension),
		wile.WithAuthorizer(security.DenyAll()),
	)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name string
		code string
	}{
		{"files: open-output-file (write)", `(open-output-file "/tmp/wile-r12.txt")`},
		{"files: open-input-file (read)", `(open-input-file "/tmp/wile-r12.txt")`},
		{"files: delete-file (delete)", `(delete-file "/tmp/wile-r12.txt")`},
		{"eval: load (code load)", `(load "/tmp/wile-r12.scm")`},
		{"eval: eval (code eval)", `(eval '(+ 1 2) (environment))`},
		// The system extension gates exit/emergency-exit (ResourceProcess,
		// ActionExit). Under DenyAll the gate denies before os.Exit runs; the
		// cases above already confirm DenyAll is active on this engine. (The
		// shell `system` prim lives in the process extension, covered separately.)
		{"system: exit (process exit)", `(exit 0)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, evalErr := engine.EvalMultiple(context.Background(), tc.code)
			qt.Assert(t, evalErr, qt.IsNotNil)
			qt.Assert(t, evalErr, qt.ErrorIs, security.ErrAccessDenied)
		})
	}
}
