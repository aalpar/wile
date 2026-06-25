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
	"errors"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/wile"
)

// A syntax-rules template that follows a pattern variable with more ellipses
// than its pattern depth (e.g. `(list a ...)` where `a` is bound at depth 0) is
// a syntax violation: there is no ellipsis-captured sequence to iterate. R7RS
// §4.3.2 requires each ellipsis sub-template to contain at least one "driver"
// variable whose pattern depth is at least the template ellipsis depth.
//
// Before the 6A fix this was accepted at define-syntax time and only failed at
// macro use with a misleading internal error ("all ellipsis IDs excluded").
// It must now fail at definition time with a clean CompilationError that names
// the variable, its template ellipsis depth, and its pattern depth, and carries
// a source location.
func TestSyntaxRules_TemplateEllipsisDepthMismatch_ErrorsAtDefinition(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx)
	if err != nil {
		t.Fatal(err)
	}

	src := "(define-syntax m3 (syntax-rules () ((_ a) (list a ...))))"
	_, err = engine.Eval(ctx, engine.MustParse(ctx, src))
	if err == nil {
		t.Fatal("expected a CompilationError at define-syntax time, got nil")
	}

	var compErr *wile.CompilationError
	if !errors.As(err, &compErr) {
		t.Fatalf("expected *wile.CompilationError, got %T: %v", err, err)
	}

	// Assert the full diagnostic phrase, not bare digits: "0"/"1" also appear in
	// the ":line:col" source prefix, so a digit-substring check would pass even
	// if the two depth values were swapped or broken.
	msg := compErr.Error()
	want := `template variable "a" used at ellipsis depth 1 but pattern binds it at depth 0`
	if !strings.Contains(msg, want) {
		t.Errorf("error message %q missing %q", msg, want)
	}
	// Provenance: the offending template node must carry a source location.
	if compErr.Source == "" {
		t.Errorf("expected a source location on the CompilationError, got empty; full error: %v", err)
	}
}

// The over-ellipsis rejection must fire across the full set of template shapes
// the validator walks — not just the depth-0/depth-1 base case. Each row is a
// distinct reachable branch: the depth+k recursion (nested), vector-pattern
// depth accounting, a custom ellipsis identifier, and an improper-list (dotted
// tail) binding. All must be rejected at definition time with the precise phrase.
func TestSyntaxRules_TemplateEllipsisDepth_RejectsOverEllipsis(t *testing.T) {
	ctx := context.Background()
	cases := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "nested over-ellipsis (depth+k recursion: used at 2, bound at 1)",
			src:  "(define-syntax m (syntax-rules () ((_ (a ...)) (list (list a ...) ...))))",
			want: `template variable "a" used at ellipsis depth 2 but pattern binds it at depth 1`,
		},
		{
			name: "vector-pattern depth (used at 2, bound at 1 in #(a ...))",
			src:  "(define-syntax m (syntax-rules () ((_ #(a ...)) (list (list a ...) ...))))",
			want: `template variable "a" used at ellipsis depth 2 but pattern binds it at depth 1`,
		},
		{
			name: "custom ellipsis identifier",
			src:  "(define-syntax m (syntax-rules ::: () ((_ a) (list a :::))))",
			want: `template variable "a" used at ellipsis depth 1 but pattern binds it at depth 0`,
		},
		{
			name: "dotted-tail binding (improper list pattern)",
			src:  "(define-syntax m (syntax-rules () ((_ x . a) (list a ...))))",
			want: `template variable "a" used at ellipsis depth 1 but pattern binds it at depth 0`,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			engine, err := wile.NewEngine(ctx)
			if err != nil {
				t.Fatal(err)
			}
			_, err = engine.Eval(ctx, engine.MustParse(ctx, tc.src))
			var compErr *wile.CompilationError
			if !errors.As(err, &compErr) {
				t.Fatalf("expected *wile.CompilationError at definition time, got %T: %v", err, err)
			}
			if !strings.Contains(compErr.Error(), tc.want) {
				t.Errorf("error %q missing %q", compErr.Error(), tc.want)
			}
		})
	}
}

// When two pattern variables are bound at the same shallowest depth, the
// reported variable must be deterministic (lexicographically smallest by name),
// not whichever a randomized Go-map iteration happens to reach first.
func TestSyntaxRules_TemplateEllipsisDepth_DeterministicOffendingVar(t *testing.T) {
	ctx := context.Background()
	src := "(define-syntax m (syntax-rules () ((_ a b) (list (cons a b) ...))))"
	const want = `template variable "a"` // "a" < "b", both bound at depth 0
	for i := range 20 {
		engine, err := wile.NewEngine(ctx)
		if err != nil {
			t.Fatal(err)
		}
		_, err = engine.Eval(ctx, engine.MustParse(ctx, src))
		var compErr *wile.CompilationError
		if !errors.As(err, &compErr) {
			t.Fatalf("iter %d: expected *wile.CompilationError, got %T: %v", i, err, err)
		}
		if !strings.Contains(compErr.Error(), want) {
			t.Fatalf("iter %d: expected deterministic %q, got %q", i, want, compErr.Error())
		}
	}
}

// Legitimate ellipsis macros must keep compiling and running — the depth check
// must not produce false positives. Each case exercises a shape the validator
// could naively misclassify: plain repetition, broadcast (a lower-depth variable
// riding inside a higher-depth ellipsis), and nested ellipsis.
func TestSyntaxRules_TemplateEllipsisDepth_NoFalsePositives(t *testing.T) {
	ctx := context.Background()
	cases := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "plain repetition (depth 1 used at depth 1)",
			src:  "(define-syntax m (syntax-rules () ((_ a ...) (list a ...)))) (m 1 2 3)",
			want: "(1 2 3)",
		},
		{
			name: "broadcast (depth 0 var inside a depth 1 ellipsis)",
			src:  "(define-syntax m (syntax-rules () ((_ x a ...) (list (cons x a) ...)))) (m 9 1 2 3)",
			want: "((9 . 1) (9 . 2) (9 . 3))",
		},
		{
			name: "nested ellipsis (depth 2 used at depth 2)",
			src:  "(define-syntax m (syntax-rules () ((_ (a ...) ...) (list (list a ...) ...)))) (m (1 2) (3 4 5))",
			want: "((1 2) (3 4 5))",
		},
		{
			// A constant sub-template followed by `...` has no pattern variable
			// to drive iteration; R7RS §4.3.2 specifies repeating it zero times
			// (dropping it), so this is NOT a depth error — it yields `()`.
			name: "constant template followed by ellipsis (no pattern vars)",
			src:  "(define-syntax m (syntax-rules () ((_ a ...) (list (quote z) ...)))) (m 1 2 3)",
			want: "()",
		},
		{
			// Custom ellipsis identifier on the valid (matching-depth) path.
			name: "custom ellipsis, depth 1 used at depth 1",
			src:  "(define-syntax m (syntax-rules ::: () ((_ a :::) (list a :::)))) (m 1 2 3)",
			want: "(1 2 3)",
		},
		{
			// Ellipsis escape (... <tmpl>) emits <tmpl> literally with no ellipsis
			// interpretation, so the depth check must skip it (early return) rather
			// than treat the inner `...` as an iteration with no driver. The result
			// is quoted so the literal `...` is data, not an evaluable reference.
			name: "ellipsis escape emits a literal ellipsis",
			src:  "(define-syntax m (syntax-rules () ((_ a ...) (quote (a ... (... ...)))))) (m 1 2 3)",
			want: "(1 2 3 ...)",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			engine, err := wile.NewEngine(ctx)
			if err != nil {
				t.Fatal(err)
			}
			val, err := engine.EvalMultiple(ctx, tc.src)
			if err != nil {
				t.Fatalf("legitimate macro should compile and run, got: %v", err)
			}
			got := val.SchemeString()
			if got != tc.want {
				t.Errorf("got %q, want %q", got, tc.want)
			}
		})
	}
}
