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

// Internal test: exercises CaptureStackTrace's parentMC hop, which requires
// wiring the unexported sub-context fields (parentMC, cont, template,
// isolatedMarks) that an external test cannot reach. The external suite in
// source_tracking_coverage_test.go covers the single-context (no-hop) path.

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/environment"
)

// traceNames extracts frame function names for order-sensitive assertions.
func traceNames(trace StackTrace) []string {
	names := make([]string, len(trace))
	for i := range trace {
		names[i] = trace[i].FunctionName
	}
	return names
}

// namedTemplate builds a zero-arg native template with a name, matching the
// idiom in source_tracking_coverage_test.go.
func namedTemplate(name string) *NativeTemplate {
	tpl := NewNativeTemplate(0, 0, false)
	tpl.SetName(name)
	return tpl
}

// TestCaptureStackTrace_SubContextHop verifies that a trace captured inside a
// sub-context spans the Go boundary via parentMC, and that the isolatedMarks
// guard cuts the walk off the way findParameterInMarks does.
//
// Scenario built per case:
//
//	parent context : live frame "middle", saved frame "outer"
//	  └─ (Go primitive bridges here)
//	      sub-context : live frame "inner-top", saved frame "inner-saved"
//
// A trace taken from the sub-context should read inner→outer as one stack,
// with a boundary marker at the crossing — unless the sub-context carries
// isolatedMarks (a re-invoked, grafted continuation), in which case it stops
// at its own root.
func TestCaptureStackTrace_SubContextHop(t *testing.T) {
	tcs := []struct {
		name          string
		isolatedMarks bool
		want          []string
	}{
		{
			name:          "spans boundary into parent frames",
			isolatedMarks: false,
			want: []string{
				"inner-top", "inner-saved",
				foreignBoundaryName,
				"middle", "outer",
			},
		},
		{
			name:          "isolated marks stops at sub-context root",
			isolatedMarks: true,
			want: []string{
				"inner-top", "inner-saved",
			},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			env := environment.NewNamespace().Runtime()

			// Parent context: live frame "middle", one saved frame "outer".
			outer := NewMachineContinuation(nil, namedTemplate("outer"), env)
			parent := NewMachineContext(
				context.Background(),
				NewMachineContinuation(outer, namedTemplate("middle"), env),
			)

			// Sub-context as a Go primitive running inside the parent would
			// create it: parentMC wired by NewSubContext. Its own Scheme frames
			// are set directly (the foreign function would have run Scheme into
			// these).
			sub := parent.NewSubContext()
			sub.template = namedTemplate("inner-top")
			sub.cont = NewMachineContinuation(nil, namedTemplate("inner-saved"), env)
			sub.isolatedMarks = tc.isolatedMarks

			got := traceNames(sub.CaptureStackTrace(20))
			c.Assert(got, qt.DeepEquals, tc.want)
		})
	}
}

// TestCaptureStackTrace_HopRespectsMaxDepth verifies the depth budget still
// truncates across the boundary: with a budget smaller than the inner chain,
// the walk never hops and emits no boundary frame.
func TestCaptureStackTrace_HopRespectsMaxDepth(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()

	outer := NewMachineContinuation(nil, namedTemplate("outer"), env)
	parent := NewMachineContext(
		context.Background(),
		NewMachineContinuation(outer, namedTemplate("middle"), env),
	)

	sub := parent.NewSubContext()
	sub.template = namedTemplate("inner-top")
	sub.cont = NewMachineContinuation(nil, namedTemplate("inner-saved"), env)

	// Budget of 2 covers only the two inner frames; the boundary and parent
	// frames are beyond it, so no hop occurs and no boundary frame appears.
	got := traceNames(sub.CaptureStackTrace(2))
	c.Assert(got, qt.DeepEquals, []string{"inner-top", "inner-saved"})
	for _, n := range got {
		c.Assert(n, qt.Not(qt.Equals), foreignBoundaryName)
	}
}
