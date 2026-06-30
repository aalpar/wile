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

package match

import (
	"context"
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// TestSyntaxMatcher_CloneForMatchConcurrent is the unit-level guard for the
// shared-syntax-rules-matcher race (operation_syntax_rules_transform.go): a
// SyntaxMatcher compiled once at macro-definition time is reused for every
// expansion of that macro, including concurrent expansions from SRFI-18 threads.
// Each expansion must run on its own CloneForMatch().
//
// The pattern is (macro x) with template x, so expansion echoes the captured
// argument. Each goroutine feeds a distinct integer and must get that same
// integer back: a data race would trip -race, and any sharing of the embedded
// Matcher's capture stack across clones would return another goroutine's value.
// Run under -race.
func TestSyntaxMatcher_CloneForMatchConcurrent(t *testing.T) {
	c := qt.New(t)

	variables := map[string]struct{}{"x": {}}
	pattern := testSyntaxList(testSyntaxSym("macro"), testSyntaxSym("x"))
	compiler := NewSyntaxCompiler()
	compiler.variables = variables
	err := compiler.Compile(context.TODO(), pattern)
	c.Assert(err, qt.IsNil)

	// One shared, compiled-once matcher — the macro-definition-time prototype.
	shared := NewSyntaxMatcher(compiler.variables, compiler.codes, nil)
	template := syntax.NewSyntaxSymbol("x", nil)

	const n = 16
	results := make([]syntax.SyntaxValue, n)
	errs := make([]error, n)

	var wg sync.WaitGroup
	for i := range n {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			input := syntax.NewSyntaxCons(
				syntax.NewSyntaxSymbol("macro", nil),
				syntax.NewSyntaxCons(
					syntax.NewSyntaxObject(values.NewInteger(int64(i)), nil),
					syntax.SyntaxEmptyList,
					nil,
				),
				nil,
			)
			m := shared.CloneForMatch()
			matchErr := m.Match(context.Background(), input)
			if matchErr != nil {
				errs[i] = matchErr
				return
			}
			expanded, expandErr := m.Expand(template, ExpandOptions{})
			if expandErr != nil {
				errs[i] = expandErr
				return
			}
			results[i] = expanded
		}(i)
	}
	wg.Wait()

	for i := range n {
		c.Assert(errs[i], qt.IsNil)
		obj, ok := results[i].(*syntax.SyntaxObject)
		c.Assert(ok, qt.IsTrue)
		// Each clone must echo its OWN input — no cross-talk between goroutines.
		c.Assert(obj.Datum(), qt.DeepEquals, values.NewInteger(int64(i)))
	}
}
