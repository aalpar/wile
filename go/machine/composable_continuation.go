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

package machine

import (
	"github.com/aalpar/wile/go/values"
)

// ComposableContinuation is a callable value wrapping a delimited continuation
// segment (a chain of MachineContinuation frames) plus the captured winding stack.
// When applied, it splices its frames onto the current continuation, effectively
// composing the captured computation with the current one.
//
// This implements Racket-style composable continuations as described in
// Flatt, Yu, Findler, Felleisen "Adding Delimited and Composable Control
// to a Production Programming Environment" (ICFP 2007).
type ComposableContinuation struct {
	cont         *MachineContinuation
	windingStack WindingStack
}

// NewComposableContinuation creates a composable continuation from a
// continuation chain segment and the winding stack captured at the point
// of capture.
func NewComposableContinuation(cont *MachineContinuation, windingStack WindingStack) *ComposableContinuation {
	q := &ComposableContinuation{
		cont:         cont,
		windingStack: windingStack,
	}
	return q
}

func (p *ComposableContinuation) Cont() *MachineContinuation { return p.cont }
func (p *ComposableContinuation) WindingStack() WindingStack { return p.windingStack }

func (p *ComposableContinuation) SchemeString() string {
	return "#<composable-continuation>"
}

func (p *ComposableContinuation) IsVoid() bool { return p == nil }

func (p *ComposableContinuation) EqualTo(o values.Value) bool {
	v, ok := o.(*ComposableContinuation)
	if !ok {
		return false
	}
	return p == v
}
