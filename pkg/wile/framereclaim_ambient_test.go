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

// Phase 2.6 — ambient base-primitive immutability
// (plans/2026-06-11-escape-gated-frame-allocation.local.md §"Phase 2.6").
//
// Phase 2 finding #1: base primitives (+, <, -) bound via the ambient
// Registry.Apply walk carry no Imported flag, so they are set!-able, so the
// frame-reclaim classifier trusts none of them — an ambient-base fib recovers
// 0% even after its own binding is stamped Stable. These tests pin the ambient
// case WITHOUT the (import (scheme base)) prelude the measurement harness relies
// on, so the signal must come from the WithImmutableTopLevel() stamp alone.

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/stdlib"
	"github.com/aalpar/wile/werr"
)

// classifyAmbient mirrors classifyCompiled (framereclaim_measure_test.go) but
// does NOT run the import prelude, so base primitives reach the namespace via
// the ambient Registry.Apply walk rather than (import (scheme base)). It expands
// once, validates that expansion, compiles the same syntax to stamp the
// producer's Stable bit, and classifies against the stamped env.
func classifyAmbient(ctx context.Context, t *testing.T, wrapped string, immutable bool) map[string]bool {
	t.Helper()

	opts := []EngineOption{
		WithProfile(KitchenSink),
		WithSourceFS(stdlib.FS),
		WithLibraryPaths(),
	}
	if immutable {
		opts = append(opts, WithImmutableTopLevel())
	}
	eng, err := NewEngine(ctx, opts...)
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	env := eng.Environment()

	pr := parser.NewParser(env, true, strings.NewReader(wrapped))
	stx, err := pr.ReadSyntax(ctx)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}

	evaluator := machine.NewVMMacroEvaluator()
	expander := compilation.NewExpanderTimeContinuation(ctx, env, evaluator)
	expanded, err := expander.ExpandExpression(stx)
	if err != nil {
		t.Fatalf("expand: %v", err)
	}

	result := validate.ValidateExpression(ctx, env, expanded)
	if !result.Ok() {
		t.Fatalf("validate: %s", result.Error())
	}
	unit := []validate.ValidatedExpr{result.Expr}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := compilation.NewCompileTimeCallContext(ctx, false)
	compiler := compilation.NewCompileTimeContinuation(tpl, env, evaluator)
	err = compiler.CompileExpression(cctx, expanded)
	if err != nil {
		t.Fatalf("compile: %v", err)
	}

	return validate.ClassifyFrameReclaim(unit, env)
}

// TestAmbientBasePrimitivesReclaimable is the Phase 2.6 positive case + flag-off
// control. fib's only callees are ambient base primitives (<, +, -) and itself.
// KitchenSink binds those primitives ambiently — it does NOT auto-import
// (scheme base) — so flag-off they are set!-able / non-Stable and fib is not
// reclaimable; flag-on the WithStableBasePrimitives stamp makes them Stable and
// fib becomes reclaimable without any import prelude. (If KitchenSink ever
// auto-imported (scheme base), the primitives would be Imported regardless of
// the flag and the flag-off row would silently go trivial.)
func TestAmbientBasePrimitivesReclaimable(t *testing.T) {
	ctx := context.Background()
	const fib = "(begin (define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))\n)"

	cases := []struct {
		name      string
		immutable bool
		want      bool
	}{
		{"flag on: ambient fib reclaimable", true, true},
		{"flag off: ambient fib inert", false, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			v := classifyAmbient(ctx, t, fib, tc.immutable)
			if v["fib"] != tc.want {
				t.Errorf("fib reclaimable = %v, want %v (WithImmutableTopLevel=%v)", v["fib"], tc.want, tc.immutable)
			}
		})
	}
}

// TestAmbientStableSoundnessControls proves that stamping ambient primitives
// Stable widens trust ONLY for capture-safe primitives. Each body must STAY
// non-reclaimable even under WithImmutableTopLevel, by a distinct path:
//   - call/cc edge: makeIsCaptureOp gates on IsImported (unchanged), so it
//     misses the ambient call/cc — but the call site is non-whitelisted, so the
//     edge-resolution fallback (unknown ⇒ unsafe) still rejects the body.
//   - escaping lambda: caught structurally by bodyCreatesEscapingClosure.
//   - non-whitelisted primitive: Stable ≠ capture-safe; assq is not on the
//     whitelist, so it yields an unsafe edge regardless of immutability.
//   - shadowed whitelisted primitive: the soundness path this change genuinely
//     widens. The global car is now Stable, so the OQ-1 lexical-shadow guard
//     (classifyCallee's bound.has check) is all that stops a set!-able local
//     car from being mistaken for the trusted global — it must stay unsafe.
func TestAmbientStableSoundnessControls(t *testing.T) {
	ctx := context.Background()
	cases := []struct {
		name string
		src  string
		fn   string
	}{
		{"call/cc via edge fallback", "(begin (define (bad k) (call/cc k))\n)", "bad"},
		{"escaping lambda", "(begin (define (esc) (lambda () 1))\n)", "esc"},
		{"non-whitelisted primitive", "(begin (define (uses-assq xs) (assq 'a xs))\n)", "uses-assq"},
		{"shadowed whitelisted primitive", "(begin (define (f h) (let ((car h)) (car 3)))\n)", "f"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			v := classifyAmbient(ctx, t, tc.src, true)
			if v[tc.fn] {
				t.Errorf("%s: must NOT be reclaimable even under WithImmutableTopLevel, got true", tc.fn)
			}
		})
	}
}

// TestStableBasePrimitivesEnforcement pins the dialect-deviation boundary: under
// WithImmutableTopLevel, ambient base primitives are Stable and BOTH the set!-gate
// and the redefine-guard (compile_validated.go) reject mutating/superseding them;
// with the flag off, R7RS-permissive ambient mutation and define-supersede still
// succeed. No import prelude — the stamp alone drives it. The define-redefine
// freeze is intentionally stricter than R7RS §5.3.1's import-supersede rule: a
// Stable ambient primitive must stay frozen for the classifier's trust to remain
// valid (an imported binding, by contrast, the redefine-guard still lets a define
// supersede).
func TestStableBasePrimitivesEnforcement(t *testing.T) {
	ctx := context.Background()
	cases := []struct {
		name      string
		expr      string
		immutable bool
		wantErr   bool
	}{
		{"flag on rejects set! on capture-safe primitive", "(set! car (lambda (x) x))", true, true},
		{"flag off permits set! on capture-safe primitive", "(set! car (lambda (x) x))", false, false},
		{"flag on rejects define-redefine of capture-safe primitive", "(define car (lambda (x) x))", true, true},
		{"flag off permits define-redefine of capture-safe primitive", "(define car (lambda (x) x))", false, false},
		// Narrow-scope control: a non-capture-safe primitive (vector-ref) is NOT
		// stamped even under the flag, so it stays R7RS-mutable. This pins the
		// "capture-safe set only" scope — the freeze does not extend to the whole
		// stdlib.
		{"flag on leaves non-capture-safe primitive mutable", "(set! vector-ref (lambda (v i) v))", true, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			opts := []EngineOption{
				WithProfile(KitchenSink),
				WithSourceFS(stdlib.FS),
				WithLibraryPaths(),
			}
			if tc.immutable {
				opts = append(opts, WithImmutableTopLevel())
			}
			eng, err := NewEngine(ctx, opts...)
			if err != nil {
				t.Fatalf("NewEngine: %v", err)
			}
			_, err = eng.EvalMultiple(ctx, tc.expr)
			if !tc.wantErr {
				if err != nil {
					t.Fatalf("flag off: %q should succeed, got %v", tc.expr, err)
				}
				return
			}
			if err == nil {
				t.Fatalf("flag on: %q on ambient base primitive should be rejected", tc.expr)
			}
			if !errors.Is(err, werr.ErrImmutableBinding) {
				t.Fatalf("flag on: expected ErrImmutableBinding, got %v", err)
			}
		})
	}
}
