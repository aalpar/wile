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
	"errors"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// TestUnboundDiagnosticSurvivesATieAtAnotherPhase pins that the cross-phase
// probe behind the unbound diagnostic REPORTS a tie rather than raising it.
//
// x is bound at phase 1 under {a,b} and {a,c}: equal cardinality, incomparable,
// so a reference carrying {a,b,c} cannot choose. The program compiles at phase
// 0, where x is simply unbound, so the error being built is ErrNoSuchBinding. A
// probe that raised ErrAmbiguousBinding at phase 1 would be recovered by the
// engine as ErrInternal and take the sentinel and the identifier's location with
// it. Tied names ARE bound, so the clause names the phase.
//
// Control: the same tie compiled AT its own phase raises ErrAmbiguousBinding
// from the ordinary resolution, which is a separate, pre-existing refusal (filed
// in todo/tier1-security-correctness.md) and not what is pinned here.
func TestUnboundDiagnosticSurvivesATieAtAnotherPhase(t *testing.T) {
	t.Parallel()

	cases := []struct {
		name string
		src  string
		tail string
	}{
		{
			name: "reference",
			src:  "xtie",
			tail: `probe.scm:1:0: no such binding "xtie" with compatible scopes (bound at phase 1): no such binding`,
		},
		{
			name: "set!",
			src:  "(set! xtie 1)",
			tail: `probe.scm:1:6: no such binding "xtie" with compatible scopes for set! (bound at phase 1): no such binding`,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			eng, err := NewEngine(ctx, WithProfile(KitchenSink))
			qt.Assert(t, err, qt.IsNil)
			defer eng.Close()

			a := syntax.NewScope()
			b := syntax.NewScope()
			c := syntax.NewScope()
			x := values.NewSymbol("xtie")
			expand := eng.Namespace().Expand()
			expand.MaybeCreateOwnGlobalBinding(x, environment.BindingTypeVariable, syntax.Scopes{}.Add(a).Add(b))
			expand.MaybeCreateOwnGlobalBinding(x, environment.BindingTypeVariable, syntax.Scopes{}.Add(a).Add(c))

			expr, err := eng.ParseWithSource(ctx, tc.src, "probe.scm")
			qt.Assert(t, err, qt.IsNil)
			stx := syntax.AddScopeToSyntax(syntax.AddScopeToSyntax(syntax.AddScopeToSyntax(expr.stx, a), b), c)

			_, err = eng.Compile(ctx, &Expression{stx: stx, source: "probe.scm"})
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue, qt.Commentf("got: %v", err))
			qt.Assert(t, errors.Is(err, werr.ErrAmbiguousBinding), qt.IsFalse, qt.Commentf("got: %v", err))
			qt.Assert(t, errors.Is(err, werr.ErrInternal), qt.IsFalse, qt.Commentf("got: %v", err))
			qt.Assert(t, strings.HasSuffix(err.Error(), tc.tail), qt.IsTrue,
				qt.Commentf("want tail: %s\ngot: %v", tc.tail, err))
		})
	}
}
