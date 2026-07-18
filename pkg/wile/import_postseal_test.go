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

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

// hostileLibFS is a library that re-exports, under bootstrap names, values that
// differ from the sealed base: `not` is the C6 name (a macro referent pinned by
// bootstrap_macros_late.scm) and `%bind` is a syntax-rules literal of parameterize
// that the nil-pin census reports as not runtime-bound.
func hostileLibFS() fstest.MapFS {
	return fstest.MapFS{
		"hostile.scm": &fstest.MapFile{
			Data: []byte(`(define-library (hostile)
  (export not %bind fresh-name)
  (import (scheme base))
  (begin
    (define (not x) 'HIJACKED)
    (define %bind 'HIJACKED)
    (define fresh-name 99)))
`),
		},
	}
}

func newHostileEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(hostileLibFS()),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."))
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

func evalString(t *testing.T, eng *wile.Engine, code string) string {
	t.Helper()
	v, err := eng.EvalMultiple(context.Background(), code)
	if err != nil {
		t.Fatalf("eval %q: %v", code, err)
	}
	return v.Internal().SchemeString()
}

// TestImportBindsIntoRuntimePhaseAfterSeal pins the ACTUAL behavior of the import
// path, which is the opposite of the intuitive invariant: a top-level (import ...)
// executed long after bootstrap has sealed the base DOES create and overwrite
// bindings in the runtime phase of the same engine.
//
// There is no seal check on this path. ResolveAndInstallImportSet computes
// targetPhase == PhaseRuntime at top level (env.PhaseLevel() and the import set's
// PhaseShift are both 0), and CopyLibraryBindingsToEnvAtPhase then calls
// MaybeCreateOwnGlobalBinding / SetOwnGlobalValue on AtPhase(0), which resolves to
// the namespace's MUTABLE runtime child, not the sealed base. The only gate is
// importConflicts, which deliberately permits overwriting a non-imported binding.
//
// This test exists so that anyone who later assumes "the sealed base makes the
// runtime phase closed to imports" is contradicted by a failing test rather than by
// a silent capture bug. The safety property is asserted separately, below.
func TestImportBindsIntoRuntimePhaseAfterSeal(t *testing.T) {
	ctx := context.Background()
	eng := newHostileEngine(t)
	env := eng.Environment()

	// `not` is deliberately excluded from the precondition: it is a bootstrap
	// procedure reachable through the sealed base, so a global index for it already
	// exists. The two names below have no binding of any kind before the import.
	for _, name := range []string{"%bind", "fresh-name"} {
		if env.GetGlobalIndex(values.NewSymbol(name)) != nil {
			t.Fatalf("precondition: %q already bound in the runtime phase before import", name)
		}
	}

	_, err := eng.EvalMultiple(ctx, `(import (hostile))`)
	if err != nil {
		t.Fatalf("import (hostile): %v", err)
	}

	for _, name := range []string{"%bind", "fresh-name"} {
		if env.GetGlobalIndex(values.NewSymbol(name)) == nil {
			t.Errorf("expected import to bind %q into the runtime phase after seal, but it did not; "+
				"if the import path gained a seal check, update this test and the safety "+
				"expectations in bootstrap_nilpin_test.go together", name)
		}
	}

	// The overwrite is real, not merely a shadow that nothing observes.
	got := evalString(t, eng, `(not #t)`)
	if got != "HIJACKED" {
		t.Errorf("(not #t) after hostile import = %s, want HIJACKED "+
			"(the import is expected to take over the runtime binding)", got)
	}
}

// TestImportCannotCaptureSnapshottedMacroReferent is the safety property that the
// sealed base actually delivers, and the complement to bootstrap_nilpin_test.go.
//
// That test certifies bootstrap LOAD ORDER: every macro's free identifiers are
// pinned by the time the macro is snapshotted. It samples `runtimeBound` once, at
// the end of bootstrap. Per TestImportBindsIntoRuntimePhaseAfterSeal that sample is
// NOT stable — a later import can flip a name from unbound to bound — so the nil-pin
// census alone does not establish that macros are safe.
//
// What makes the drift harmless FOR THE RUNTIME PHASE is the sealed base, not the
// pin: a top-level define or an import writes to the mutable runtime child, a
// different frame, so the sealed-base binding a macro's free identifier resolves to
// is untouched. This test pins that end-to-end.
//
// Scope, deliberately narrow. An earlier version of this comment justified the
// assertion by claiming every remaining nil pin is "a template-introduced binder, a
// syntax-rules literal, or a sibling macro" and therefore safe. The sibling-macro
// clause is FALSE: the expand phase has no sealed base, a top-level define-syntax
// overwrites a bootstrap macro's binding IN PLACE (same *Binding, new value), and a
// pin is only a *Binding pointer, so it does not survive that. `guard-aux` is
// capturable today by plain user code:
//
//	(define-syntax guard-aux (syntax-rules () ((_ r ...) 'PWNED)))
//	(guard (e (else 'caught)) (raise 'x))                          => PWNED
//
// These tests therefore cover the runtime phase only. Do not read them as evidence
// that expand-phase referents are protected. See bootstrap_nilpin_test.go.
func TestImportCannotCaptureSnapshottedMacroReferent(t *testing.T) {
	ctx := context.Background()
	eng := newHostileEngine(t)

	before := evalString(t, eng, `(unless #f 'ok)`)
	if before != "ok" {
		t.Fatalf("precondition: (unless #f 'ok) = %s, want ok", before)
	}

	_, err := eng.EvalMultiple(ctx, `(import (hostile))`)
	if err != nil {
		t.Fatalf("import (hostile): %v", err)
	}

	// `unless` expands to a use of `not`. The import owns the runtime `not`
	// (asserted above), so if the macro's referent were resolved at the use site
	// instead of through its pin, this would yield HIJACKED.
	got := evalString(t, eng, `(unless #f 'ok)`)
	if got != "ok" {
		t.Errorf("(unless #f 'ok) after hostile import = %s, want ok; the macro's `not` "+
			"referent was captured by the imported binding, which means it is no longer "+
			"pinned to the sealed base (see bootstrap_macros_late.scm)", got)
	}

	// %bind is a declared syntax-rules literal of parameterize with a nil pin.
	// Binding it in the runtime phase must not disturb literal matching.
	got = evalString(t, eng, `(let ((p (make-parameter 1))) (parameterize ((p 7)) (p)))`)
	if got != "7" {
		t.Errorf("parameterize after hostile import = %s, want 7; binding the %%bind "+
			"literal in the runtime phase disturbed literal matching", got)
	}
}
