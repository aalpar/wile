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

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// literalPoolCorpus exercises every compile-time path that can put an aggregate
// into a template's literal pool: the three constant appenders, both source
// orders for a deduping pair of twins, aggregates nested inside a constant,
// aggregates inside a sub-template (lambda bodies pool separately and hang off
// the parent pool as *NativeTemplate), macro templates expanded more than once,
// and the derived forms (case, do, named let) that emit quoted datums of their
// own.
var literalPoolCorpus = []struct {
	name string
	code string
}{
	{"quoted aggregates", `(begin '(1 2 3) '#(1 2 3) '#u8(1 2 3))`},
	{"self-evaluating aggregates", `(begin #(1 2 3) #u8(1 2 3))`},
	{"quasiquoted constants", "(begin `(1 2 3) `#(1 2 3) `#u8(1 2 3))"},
	{"twins, unquoted first", `(begin (define a #(1 2 3)) (define b '#(1 2 3)) (list a b))`},
	{"twins, quoted first", `(begin (define b '#(1 2 3)) (define a #(1 2 3)) (list a b))`},
	{"quasiquoted twin first", "(begin (define a `(1 2 3)) (define b '(1 2 3)) (list a b))"},
	{"nested aggregates", "(begin '(1 #(2 #u8(3)) (4 . 5)) `(1 #(2 #u8(3))) (vector (list 1) #(2)))"},
	{"improper and empty", `(begin '(1 . 2) '() '#() '#u8())`},
	{"inside a lambda body", `(lambda (x) (if x '(1 2) #(3 4)))`},
	{"inside nested lambdas", "(lambda (x) (lambda (y) (list '(a b) #(c d) `#u8(1 2))))"},
	{"macro template used twice", `(begin
		(define-syntax twice (syntax-rules () ((_ x) (list x '(1 2 3) #(4 5 6)))))
		(twice 'a)
		(twice 'b))`},
	{"syntax-case template", `(begin
		(define-syntax pick (syntax-rules () ((_ a b) (if a '#(1 2) '#u8(3 4)))))
		(pick #t #f))`},
	{"case and derived forms", `(begin
		(define (f x) (case x ((1 2 3) 'low) ((4 5 6) 'high) (else 'other)))
		(f 1)
		(do ((i 0 (+ i 1))) ((= i 0) '(done)) i)
		(let loop ((xs '(1 2 3))) (if (null? xs) '#() (loop (cdr xs)))))`},
	// A quasiquote containing an unquote expands to list/vector CALLS, so its
	// aggregates never pool; the trailing quote is what keeps this entry from
	// tripping the non-vacuity check, and its presence documents the split.
	{"quasiquote with unquote around constants", "(begin (define n 1) `(,n #(2 3) (4 5)) '(6 7))"},
	{"shared structure via datum label", `'(#0=(1 2) #0# #(3))`},
	{"string and char constants alongside", `(begin '("a" #\b 1.5 1/2) #("x" #\y))`},
}

// collectTemplates walks a template tree the way trackTemplateTree does:
// sub-templates appear as *NativeTemplate entries in the parent's literal pool.
func collectTemplates(root *machine.NativeTemplate) []*machine.NativeTemplate {
	var q []*machine.NativeTemplate
	visited := make(map[*machine.NativeTemplate]bool)
	queue := []*machine.NativeTemplate{root}
	for len(queue) > 0 {
		tpl := queue[0]
		queue = queue[1:]
		if tpl == nil || visited[tpl] {
			continue
		}
		visited[tpl] = true
		q = append(q, tpl)
		for _, lit := range tpl.Literals() {
			child, ok := lit.(*machine.NativeTemplate)
			if ok {
				queue = append(queue, child)
			}
		}
	}
	return q
}

// aggregateCounts separates what the walk TOUCHED from what it could ASSERT on.
// Pair literals are mutable by decision, so a pooled pair carries no obligation;
// it is still counted, because reaching a nested vector is what the spine walk
// is for and an entry that pools nothing at all cannot ratchet anything.
type aggregateCounts struct {
	inspected int // every aggregate reached
	flaggable int // those with an immutability obligation: vectors, bytevectors
}

func (p *aggregateCounts) add(o aggregateCounts) {
	p.inspected += o.inspected
	p.flaggable += o.flaggable
}

// assertAggregateImmutable checks v and every aggregate reachable from it.
// Recursion mirrors markLiteralImmutable's walk, so a flag that stops short of a
// nested element fails here rather than at some later mutator call site.
func assertAggregateImmutable(
	t *testing.T,
	v values.Value,
	visited values.MapSet[values.Value],
) aggregateCounts {
	switch obj := v.(type) {
	case *values.Pair:
		seen := visited.ContainsOne(obj)
		if seen {
			return aggregateCounts{}
		}
		visited.Add(obj)
		// No assertion: a pair literal is mutable, and that is the decision
		// (a flag word is ~25% growth on the 32-byte cons cell). The recursion
		// below is the whole reason this arm still exists.
		q := aggregateCounts{inspected: 1}
		q.add(assertAggregateImmutable(t, obj.Car(), visited))
		q.add(assertAggregateImmutable(t, obj.Cdr(), visited))
		return q
	case *values.Vector:
		seen := visited.ContainsOne(obj)
		if seen {
			return aggregateCounts{}
		}
		visited.Add(obj)
		qt.Assert(t, obj.IsImmutable(), qt.IsTrue,
			qt.Commentf("pooled vector %s is mutable", obj.SchemeString()))
		q := aggregateCounts{inspected: 1, flaggable: 1}
		for _, elem := range obj.Elems() {
			q.add(assertAggregateImmutable(t, elem, visited))
		}
		return q
	case *values.ByteVector:
		seen := visited.ContainsOne(obj)
		if seen {
			return aggregateCounts{}
		}
		visited.Add(obj)
		qt.Assert(t, obj.IsImmutable(), qt.IsTrue,
			qt.Commentf("pooled bytevector %s is mutable", obj.SchemeString()))
		return aggregateCounts{inspected: 1, flaggable: 1}
	default:
		// Non-aggregate pool entries carry no immutability obligation:
		// values.Void, *GlobalIndex, *NativeTemplate, *EnvironmentFrame,
		// *ClausesWrapper, *SyntaxCaseClause and syntax objects all reach the
		// pool from the other MaybeAppendLiteral call sites. The set is derived
		// from those call sites, not from a remembered type blacklist: only the
		// three types markLiteralImmutable walks are counted.
		return aggregateCounts{}
	}
}

// TestLiteralPoolAggregatesAreImmutable is the ratchet behind shape D′. The
// invariant it states is stronger than "the three appenders flag": every
// aggregate that reaches a literal pool is a compile-time constant — (vector 1
// 2) is a call and never pools — so every pooled aggregate that CAN be frozen
// must be. A fourth appender added later fails here instead of silently
// shipping a mutable constant.
//
// It was the only gate on the immutability-representation plan's Phase 2, and it
// earned that: with *Vector flagged while the compiler still called set.Mark,
// every row of this test went red and (vector-set! '#(1 2 3) 0 9) answered
// #(9 2 3). Phase 3 then deleted the side set, so the query is now the value's
// own IsImmutable. Do not weaken or fold this test.
//
// Scope it lost, stated rather than left implicit: pooled PAIRS are no longer
// asserted on, because pair-literal immutability was dropped with the side set.
// Entries whose only aggregates are pairs (the two quasiquote rows) therefore
// ratchet the walk's reach and termination and nothing else; the corpus-level
// flaggable count below is what keeps the suite as a whole non-vacuous.
func TestLiteralPoolAggregatesAreImmutable(t *testing.T) {
	ctx := context.Background()
	corpusFlaggable := 0
	for _, tc := range literalPoolCorpus {
		t.Run(tc.name, func(t *testing.T) {
			eng, err := NewEngine(ctx, WithProfile(KitchenSink))
			qt.Assert(t, err, qt.IsNil)
			expr, err := eng.Parse(ctx, tc.code)
			qt.Assert(t, err, qt.IsNil)
			cc, err := eng.Compile(ctx, expr)
			qt.Assert(t, err, qt.IsNil)

			visited := values.NewMapSet[values.Value](0)
			var total aggregateCounts
			for _, tpl := range collectTemplates(cc.template) {
				for _, lit := range tpl.Literals() {
					total.add(assertAggregateImmutable(t, lit, visited))
				}
			}
			corpusFlaggable += total.flaggable
			qt.Assert(t, total.inspected > 0, qt.IsTrue,
				qt.Commentf("corpus entry pooled no aggregate; it cannot ratchet anything"))
		})
	}
	qt.Assert(t, corpusFlaggable > 0, qt.IsTrue,
		qt.Commentf("no corpus entry pooled a flaggable aggregate: every row would "+
			"pass with immutability switched off entirely"))
}

// TestEvalOfRuntimeVectorReturnsMutableCopy is the pre-check that licensed
// marking at CompileSelfEvaluating. That appender now freezes every aggregate it
// pools, and eval routes a caller's runtime vector through it — so if eval
// returned the SAME object, marking there would freeze an object the Go or
// Scheme caller still owns. It returns a copy, and the original stays mutable.
func TestEvalOfRuntimeVectorReturnsMutableCopy(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink))
	qt.Assert(t, err, qt.IsNil)

	same, err := eng.EvalMultiple(ctx,
		`(define v (vector 1 2 3)) (eq? v (eval v (interaction-environment)))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, same.Internal(), qt.Equals, values.Value(values.FalseValue))

	elem, err := eng.EvalMultiple(ctx, `(vector-set! v 0 9) (vector-ref v 0)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, elem.SchemeString(), qt.Equals, "9")
}
