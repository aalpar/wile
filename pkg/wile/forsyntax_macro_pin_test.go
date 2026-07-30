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
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// lookupMacroBinding's arm 1 (current-phase, scope-precise) sits AHEAD of the D2
// definition-site pin, and an ambient ∅-scoped binding satisfies bindingScopes ⊆
// symbolScopes vacuously. That is the macro-path twin of the CompileSymbol pin
// ordering fixed in bff525a8, and it needed the same cardinality guard.
//
// Phase 0 cannot reach it: every BindingTypeSyntax creation site writes either an
// Expand-phase global (compile_define_syntax.go, expander_body.go) or a local with
// keywordScopes (expander_let_syntax.go), so no ∅-scoped syntax global is reachable
// from a phase-0 p.env, and the whole guard-aux hijack class is already correct.
//
// A PROCEDURAL transformer body is the reaching site. compileAndEvalLambdaTransformer
// (compile_transformer.go, shared by `lambda` and `er-macro-transformer`) compiles the
// body against env.NextPhase(), so ExpandAndCompile roots the expander at the phase-1
// frame — which is exactly where a phase-0 (define-syntax …) deposits its ∅-scoped
// keyword. `begin-for-syntax` / `define-for-syntax` / `eval-when` do NOT reach it:
// they keep the expander rooted at p.env and let arm 2's NextPhase() do the climb.
//
// Both mutation directions were checked. Reverting the guard turns the first test's
// `lib-helper` into `user-helper`. Making the ∅-scoped arm-1 match DROP through
// instead of demoting below the pin breaks both the first and the third test with
// expansion failures: arm 1 is the only arm that finds a current-phase macro, since
// arm 2 looks a phase above p.env.

// forSyntaxPinFS exports `mac`, whose template names the library-private macro
// `helper`. `helper` is not exported, so observing `user-helper` means the use
// site's own keyword captured `mac`'s pinned free template identifier.
var forSyntaxPinFS = fstest.MapFS{
	"pinlib.sld": &fstest.MapFile{
		Data: []byte(`(define-library (pinlib)
  (import (scheme base))
  (export mac)
  (begin
    (define-syntax helper (syntax-rules () ((_ x) (syntax (quote lib-helper)))))
    (define-syntax mac (syntax-rules () ((_ y) (helper y))))))`),
	},
}

func newForSyntaxPinEngine(ctx context.Context, t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceFS(forSyntaxPinFS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// TestForSyntaxPin_TransformerBodyKeepsPinnedTemplateID is the discriminating repro:
// `(mac 1)` sits in a lambda transformer's body, so it expands with p.env at phase 1,
// where the use site's `helper` keyword lives. Before the guard this yielded
// user-helper.
func TestForSyntaxPin_TransformerBodyKeepsPinnedTemplateID(t *testing.T) {
	ctx := context.Background()
	eng := newForSyntaxPinEngine(ctx, t)

	v, err := eng.EvalMultiple(ctx, `
		(import (pinlib))
		(define-syntax helper (syntax-rules () ((_ x) (syntax (quote user-helper)))))
		(define-syntax probe (lambda (stx) (mac 1)))
		(probe)
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "lib-helper",
		qt.Commentf("`mac`'s free template identifier `helper` must keep resolving to the "+
			"library's private macro. Reading user-helper means lookupMacroBinding's arm 1 "+
			"matched the use site's ∅-scoped keyword ahead of the definition-site pin, "+
			"because the transformer body expands with p.env at phase 1."))
}

// TestForSyntaxPin_TransformerBodyControl is the same program without the colliding
// keyword: correct before and after, so it pins that the repro above discriminates on
// the collision rather than on the transformer-body path itself.
func TestForSyntaxPin_TransformerBodyControl(t *testing.T) {
	ctx := context.Background()
	eng := newForSyntaxPinEngine(ctx, t)

	v, err := eng.EvalMultiple(ctx, `
		(import (pinlib))
		(define-syntax probe (lambda (stx) (mac 1)))
		(probe)
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "lib-helper")
}

// TestForSyntaxPin_TransformerBodyReachesAmbientMacro pins the demote-don't-drop half.
// A DIRECTLY typed reference in a transformer body carries no pin, and the phase-0
// keyword it names is only reachable through arm 1's ∅-scoped match, so guarding arm 1
// must leave that path intact.
func TestForSyntaxPin_TransformerBodyReachesAmbientMacro(t *testing.T) {
	ctx := context.Background()
	eng := newForSyntaxPinEngine(ctx, t)

	v, err := eng.EvalMultiple(ctx, `
		(define-syntax mk (syntax-rules () ((_) (syntax (quote from-user-mk)))))
		(define-syntax probe2 (lambda (stx) (mk)))
		(probe2)
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "from-user-mk",
		qt.Commentf("a transformer body's direct macro reference resolves through arm 1's "+
			"∅-scoped match; arm 2 looks a phase ABOVE p.env and cannot find it"))
}
