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

// Package sample is a fixture for the cxmeasure metrics. Each function pins one
// rule of the cognitive score: the nesting surcharge, the flat else-if chain,
// the flat cost of a bare else, boolean sequencing, closure descent, and the
// wide-versus-deep distinction that the -arms mode exists to settle.
//
// Expected values live in main_test.go, not here, so that a fixture edit and an
// expectation edit cannot silently agree with each other.
package sample

// flat has no control flow at all: cognitive 0, cyclomatic 1.
func flat() int {
	q := 1
	q++
	return q
}

// oneIf charges a single unnested branch: cognitive 1.
func oneIf(a bool) int {
	if a {
		return 1
	}
	return 0
}

// nestedIfInFor is the nesting surcharge: the range costs 1 at depth 0, the if
// costs 1+1 at depth 1. Cognitive 3, versus cyclomatic 3 for the same code.
func nestedIfInFor(xs []int) int {
	q := 0
	for _, x := range xs {
		if x > 0 {
			q += x
		}
	}
	return q
}

// elseIfLadder pins the flat-chain rule: gofmt renders an else-if ladder at one
// indentation level, so each rung costs 1 with no depth surcharge. Cognitive 3
// for three rungs, not 1+2+3.
func elseIfLadder(n int, q *string) {
	if n == 0 {
		*q = "zero"
	} else if n == 1 {
		*q = "one"
	} else if n == 2 {
		*q = "two"
	}
}

// bareElse pins the other half of that rule: a terminal else costs a flat 1,
// with no depth surcharge, on top of the if's 1. Cognitive 2.
func bareElse(a bool, q *int) {
	if a {
		*q = 1
	} else {
		*q = 2
	}
}

// wideSwitch is the shape the whole -arms mode exists for: five arms, cognitive
// 1, cyclomatic 6. Breadth is free; the metric must not punish a dispatch table.
func wideSwitch(n int) string {
	switch n {
	case 0:
		return "a"
	case 1:
		return "b"
	case 2:
		return "c"
	case 3:
		return "d"
	default:
		return "z"
	}
}

// switchWithNestedIf is the same table with one non-trivial arm. As written the
// if pays the switch's depth (1+1); extracted, it pays nothing. This is the
// collapse that -arms reports.
func switchWithNestedIf(n int, flag bool) int {
	switch n {
	case 0:
		return 0
	case 1:
		if flag {
			return 1
		}
		return 2
	default:
		return 3
	}
}

// booleans pins boolean sequencing: each && and || costs 1, on top of the if.
// Cognitive 3.
func booleans(a, b, c bool) bool {
	if a && b || c {
		return true
	}
	return false
}

// withClosure pins closure descent: unlike tools/cmd/nestinglint, which measures a
// literal as its own scope, cognitive complexity charges the closure's interior
// to the enclosing function one level deeper. The range costs 1+1. Cognitive 2.
func withClosure(xs []int) func() int {
	return func() int {
		q := 0
		for _, x := range xs {
			q += x
		}
		return q
	}
}

// counter exists only to give the fixture a method, pinning qualifiedName's
// pointer-receiver rendering.
type counter struct {
	n int
}

// bump is measured as (*counter).bump.
func (p *counter) bump(by int) {
	if by > 0 {
		p.n += by
	}
}
