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

	"github.com/aalpar/wile/coverage"
	"github.com/aalpar/wile/pkg/wile"
)

func TestWithCoverage_TopLevelExprTracked(t *testing.T) {
	c := qt.New(t)
	col := coverage.NewCollector()
	eng, err := wile.NewEngine(context.Background(), wile.WithCoverage(col))
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalMultipleWithSource(context.Background(), "(+ 1 2)", "test.scm")
	c.Assert(err, qt.IsNil)

	entries := col.Entries()
	c.Assert(len(entries) > 0, qt.IsTrue, qt.Commentf("entries: %+v", entries))

	hit := false
	for _, e := range entries {
		if e.File == "test.scm" && e.Count == 1 {
			hit = true
			break
		}
	}
	c.Assert(hit, qt.IsTrue, qt.Commentf("entries: %+v", entries))
}

func TestWithCoverage_UncalledBodyNotCovered(t *testing.T) {
	c := qt.New(t)
	col := coverage.NewCollector()
	eng, err := wile.NewEngine(context.Background(), wile.WithCoverage(col))
	c.Assert(err, qt.IsNil)

	// Define two procedures on one line; call only f.
	// g's body (222) must appear as Count=0 — its template is registered
	// but never executed. f's body (111) must appear as Count=1.
	_, err = eng.EvalMultipleWithSource(
		context.Background(),
		"(define (f) 111) (define (g) 222) (f)",
		"t.scm",
	)
	c.Assert(err, qt.IsNil)

	entries := col.Entries()

	var hitCovered, hitUncovered bool
	for _, e := range entries {
		if e.File != "t.scm" {
			continue
		}
		if e.Count == 1 {
			hitCovered = true
		}
		if e.Count == 0 {
			hitUncovered = true
		}
	}
	c.Assert(hitCovered, qt.IsTrue, qt.Commentf("called body must have a Count=1 entry; entries: %+v", entries))
	c.Assert(hitUncovered, qt.IsTrue, qt.Commentf("uncalled body must have a Count=0 entry; entries: %+v", entries))
}

func TestWithCoverage_NestedLambdaTracked(t *testing.T) {
	c := qt.New(t)
	col := coverage.NewCollector()
	eng, err := wile.NewEngine(context.Background(), wile.WithCoverage(col))
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalMultipleWithSource(
		context.Background(),
		"(define (f x) (* x x)) (f 7)",
		"nested.scm",
	)
	c.Assert(err, qt.IsNil)

	entries := col.Entries()

	var hitBody bool
	for _, e := range entries {
		if e.File == "nested.scm" && e.Count == 1 {
			hitBody = true
		}
	}
	c.Assert(hitBody, qt.IsTrue, qt.Commentf("lambda body must be tracked; entries: %+v", entries))
}
