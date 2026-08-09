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

	"github.com/aalpar/wile/pkg/wile"
)

// Scheme-level gates for review wave 5 cluster D — the pattern matcher's three
// independent matching gaps (2.3.3, 2.3.4, §3 discovery 1) plus the two
// template/pattern hygiene defects (2.2.13, 2.2.14).
//
// Every case below was observed to produce the value named in its comment at
// 003b3353 / fix/w1-syntax-case-relocation. The unit-level pins in
// pkg/internal/match cover the bytecode shapes; these cover the behaviour a
// Scheme program can see, which is what the review's §8 found missing.

// evalScheme evaluates one form and returns its printed representation.
func evalScheme(t *testing.T, src string) (string, error) {
	t.Helper()
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx)
	if err != nil {
		t.Fatal(err)
	}
	v, err := engine.Eval(ctx, engine.MustParse(ctx, src))
	if err != nil {
		return "", err
	}
	return v.SchemeString(), nil
}

// mustEvalScheme evaluates one form and fails the test if it errors.
func mustEvalScheme(t *testing.T, src string) string {
	t.Helper()
	got, err := evalScheme(t, src)
	if err != nil {
		t.Fatalf("eval: %v", err)
	}
	return got
}

// 2.3.3 — an ellipsis over a variable-free subpattern must repeat, not degrade
// to a literal compare against the ellipsis symbol. R7RS §4.3.2's "F matches P"
// clause list is purely structural; no clause mentions variable occurrence.
//
// All five forms raised "syntax-rules: no matching clause" at the base.
func TestEllipsisOverVariableFreeSubpattern(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "three repetitions of a literal",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ 1 ...) 'matched-ones)))
                    (m 1 1 1))`,
			want: "matched-ones",
		},
		{
			name: "zero repetitions of a literal",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ 1 ...) 'matched-ones)))
                    (m))`,
			want: "matched-ones",
		},
		{
			name: "one repetition inside a sublist",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ (1 ...)) 'matched-sub)))
                    (m (1)))`,
			want: "matched-sub",
		},
		{
			name: "zero repetitions inside a sublist",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ (1 ...)) 'matched-sub)))
                    (m ()))`,
			want: "matched-sub",
		},
		{
			name: "one repetition inside a vector",
			src: `(begin
                    (define-syntax m (syntax-rules () ((_ #(1 ...)) 'matched-vec)))
                    (m #(1)))`,
			want: "matched-vec",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got := mustEvalScheme(t, tc.src)
			if got != tc.want {
				t.Errorf("got %s, want %s", got, tc.want)
			}
		})
	}
}

// The repetition is real, not a blanket accept: a literal subpattern still has
// to match the input it repeats over. At the base the first clause matched
// nothing, so this returned (other other other).
func TestEllipsisOverVariableFreeSubpatternStillDiscriminates(t *testing.T) {
	src := `(begin
              (define-syntax m
                (syntax-rules ()
                  ((_ 1 ...) 'ones)
                  ((_ x ...) 'other)))
              (list (m 1 1) (m 2 2) (m 1 2)))`
	got := mustEvalScheme(t, src)
	want := "(ones other other)"
	if got != want {
		t.Errorf("got %s, want %s", got, want)
	}
}
