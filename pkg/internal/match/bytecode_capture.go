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

import "fmt"

// ByteCodeCaptureCar captures the current car as a pattern variable binding.
type ByteCodeCaptureCar struct {
	Binding string
}

func (p ByteCodeCaptureCar) String() string {
	return fmt.Sprintf("CaptureCar(%s)", p.Binding)
}

// ByteCodeCaptureCdr captures the current cdr as a pattern variable binding.
// This is used for improper list patterns like (_ a . rest) where rest
// should capture the remaining elements of the input list.
//
// R7RS §4.3.2: In a pattern, an identifier followed by . and another
// identifier matches any input that is a list of one or more elements,
// binding the first identifier to the first element and the second
// identifier to the rest of the list.
type ByteCodeCaptureCdr struct {
	Binding string
}

func (p ByteCodeCaptureCdr) String() string {
	return fmt.Sprintf("CaptureCdr(%s)", p.Binding)
}

// ByteCodeDiscardCar asserts that the current position holds an element and
// matches it without binding.
//
// R7RS §4.3.2: `_` matches any input and never binds. Emitting NOTHING for it
// was not equivalent to matching it: the element stays visible to Done, whose
// one-instruction lookahead then reads the following pattern element's bytecode
// and cannot tell "the pattern ends here" from "the next pattern element is a
// wildcard". So `((k (y) _) …)` rejected `(d (1) 9)` and, in the other
// direction, `((k (y) _ _) …)` accepted a two-element input.
//
// The presence assertion is the whole instruction; advancement is the emitted
// VisitCdr's job, exactly as for a pattern variable's CaptureCar.
type ByteCodeDiscardCar struct{}

func (ByteCodeDiscardCar) String() string {
	return "DiscardCar"
}

// ByteCodeDiscardCdr consumes the current cdr without binding it.
//
// R7RS §4.3.2: `_` is a wildcard that matches any input but never binds. In a
// dotted tail (`(_ a . _)`) it must still consume the rest of the input, so the
// matcher cannot simply emit nothing — that would leave the trailing elements
// visible to Done, which requires the position to be exhausted.
type ByteCodeDiscardCdr struct{}

func (ByteCodeDiscardCdr) String() string {
	return "DiscardCdr"
}
