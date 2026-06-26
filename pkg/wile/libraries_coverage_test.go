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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// libcovEngine builds a KitchenSink engine with the embedded stdlib for the
// libraries-coverage backfill (plan Track C, items 11B/11E).
func libcovEngine(t *testing.T) (*wile.Engine, context.Context) {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	return eng, ctx
}

// TestPrinterSharingVsCycle is the 11B printer suite: structural sharing (an
// acyclic DAG) must render in full through write / write-simple / display, while
// a genuine cycle must render bounded ("...") rather than diverging — across
// pairs, vectors, and pair↔vector cross-references (A3).
func TestPrinterSharingVsCycle(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			name: "shared pair DAG renders in full (write-simple)",
			code: `(import (scheme base) (scheme write))
                   (define s (list 1 2))
                   (define p (open-output-string))
                   (write-simple (list s s) p)
                   (get-output-string p)`,
			want: `"((1 2) (1 2))"`,
		},
		{
			name: "shared vector DAG renders in full",
			code: `(import (scheme base) (scheme write))
                   (define s (vector 1 2))
                   (define p (open-output-string))
                   (write-simple (vector s s) p)
                   (get-output-string p)`,
			want: `"#(#(1 2) #(1 2))"`,
		},
		{
			name: "self-cyclic pair is bounded",
			code: `(import (scheme base) (scheme write))
                   (define c (list 1))
                   (set-cdr! c c)
                   (define p (open-output-string))
                   (write-simple c p)
                   (get-output-string p)`,
			want: `"(1 . ...)"`,
		},
		{
			name: "pair sharing through display",
			code: `(import (scheme base))
                   (define s (list 'a 'b))
                   (define p (open-output-string))
                   (display (list s s) p)
                   (get-output-string p)`,
			want: `"((a b) (a b))"`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng, ctx := libcovEngine(t)
			defer eng.Close()
			result, err := eng.EvalMultiple(ctx, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestSRFI13EdgeRows is part of the 11E per-library backfill: edge behavior of
// the procedures added in A2 (string-titlecase, string-hash, xsubstring).
func TestSRFI13EdgeRows(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"xsubstring cyclic wrap", `(import (srfi 13)) (xsubstring "abc" 0 7)`, `"abcabca"`},
		{"xsubstring negative start", `(import (srfi 13)) (xsubstring "abc" -2 1)`, `"bca"`},
		{"string-titlecase word boundaries", `(import (srfi 13)) (string-titlecase "hELLo wORLD")`, `"Hello World"`},
		{"string-hash stable + bounded", `(import (srfi 13)) (= (string-hash "abc" 100) (string-hash "abc" 100))`, `#t`},
		{"string-hash respects bound", `(import (srfi 13)) (< (string-hash "abcdefghij" 16) 16)`, `#t`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng, ctx := libcovEngine(t)
			defer eng.Close()
			result, err := eng.EvalMultiple(ctx, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestSRFI14SurrogateEdgeRows is part of 11E: char-sets never contain UTF-16
// surrogates (B4), so size excludes the 2048-codepoint block and iteration
// across a straddling range is safe and surrogate-free.
func TestSRFI14SurrogateEdgeRows(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"full excludes surrogates", `(import (srfi 14)) (= (char-set-size char-set:full) (- #x110000 2048))`, `#t`},
		{"complement of empty excludes surrogates", `(import (srfi 14)) (= (char-set-size (char-set-complement (char-set))) (- #x110000 2048))`, `#t`},
		{"ucs-range straddling block drops surrogates", `(import (srfi 14)) (char-set-fold (lambda (c acc) (+ acc 1)) 0 (ucs-range->char-set #xD7FE #xE002))`, `4`},
		{"no char-set member is a surrogate", `(import (srfi 14)) (char-set-contains? char-set:full (integer->char #xE000))`, `#t`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng, ctx := libcovEngine(t)
			defer eng.Close()
			result, err := eng.EvalMultiple(ctx, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.want)
		})
	}
}
