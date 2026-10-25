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
	"testing"

	"github.com/aalpar/wile/pkg/machine"

	qt "github.com/frankban/quicktest"
)

// TestEmptyLetWithoutDefinitionsPushesNoFrame pins that (let () e ...) whose
// body defines nothing compiles as a sequence, with no OpPushEnv. parameterize
// and let-optionals* expand their <body> to that let; each use pushed and popped
// a frame that held nothing (+13% on a parameterize loop at top level). A body
// that defines something still gets its frame, for the definitions' slots.
func TestEmptyLetWithoutDefinitionsPushesNoFrame(t *testing.T) {
	tcs := []struct {
		name       string
		code       string
		wantPushes int
	}{
		{name: "top level", code: `(let () 1 2)`, wantPushes: 0},
		{name: "in a procedure", code: `(lambda (a) (let () (a) (lambda () a)))`, wantPushes: 0},
		{name: "under with-continuation-mark", code: `(lambda (k v) (with-continuation-mark k v (let () (k) (v))))`, wantPushes: 0},
		{name: "a body define", code: `(let () (define x 1) x)`, wantPushes: 1},
		{name: "a define in a spliced begin", code: `(let () (begin (define x 1)) x)`, wantPushes: 1},
		{name: "a define-syntax", code: `(let () (define-syntax m (syntax-rules () ((_) 1))) (m))`, wantPushes: 1},
		{name: "a let with a binding", code: `(let ((x 1)) x)`, wantPushes: 1},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			tpl := compileToTemplate(t, tc.code)
			qt.Assert(t, countOpEverywhere(tpl, machine.OpPushEnv), qt.Equals, tc.wantPushes)
		})
	}
}
