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

// Package sample is a fixture for the nestinglint detector. Each function's
// intended maximum control-nesting depth is stated in its name and doc comment;
// the tests assert maxNesting against these. This file is never compiled into
// the tool (it lives under testdata/) and is not itself linted.
package sample

// flat has no control nesting: depth 0.
func flat(a, b int) int {
	q := a + b
	return q
}

// oneGuard is a single guard clause: depth 1.
func oneGuard(a int) int {
	if a < 0 {
		return 0
	}
	return a
}

// elseIfLadder is a long if / else-if / else chain. gofmt renders it flat at a
// single indentation level, so its depth is 1 — NOT one level per rung. This is
// the linter's core correctness property.
func elseIfLadder(a int) string {
	if a == 0 {
		return "zero"
	} else if a == 1 {
		return "one"
	} else if a == 2 {
		return "two"
	} else if a == 3 {
		return "three"
	} else {
		return "many"
	}
}

// nestedThree buries a return under if > for > if: depth 3.
func nestedThree(xs []int, limit int) int {
	if limit > 0 {
		for _, x := range xs {
			if x > limit {
				return x
			}
		}
	}
	return -1
}

// switchWithIf has an if inside a case. The switch consumes one level and the
// if inside the case consumes the next: depth 2.
func switchWithIf(tag int, ok bool) int {
	switch tag {
	case 1:
		if ok {
			return 1
		}
	case 2:
		return 2
	}
	return 0
}

// deepBodyShallowPlacement places a closure at the top level whose OWN body
// nests if > for > if (depth 3). The closure is measured as its own scope, so
// its depth is 3 while the enclosing function contributes nothing beyond it.
func deepBodyShallowPlacement(xs []int) func(int) int {
	return func(limit int) int {
		if limit > 0 {
			for _, x := range xs {
				if x > limit {
					return x
				}
			}
		}
		return -1
	}
}

// shallowLiteralDeepPlacement is the mirror: the enclosing function nests
// for > if > if (depth 3) around a closure whose own body is flat (depth 0).
// The closure must not be charged for its placement, and the enclosing function
// must not be charged for the closure's interior.
func shallowLiteralDeepPlacement(xs []int) {
	for _, x := range xs {
		if x > 0 {
			if x < 100 {
				fn := func(v int) int {
					return v * 2
				}
				_ = fn(x)
			}
		}
	}
}
