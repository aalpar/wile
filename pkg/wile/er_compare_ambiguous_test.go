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
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// TestERCompareAmbiguousIdentifierIsACatchableLocatedError: an ER compare whose
// argument resolves to an incomparable tie raises ErrAmbiguousBinding from inside
// the foreign closure. The panic is recovered at the foreign-call boundary into
// an ordinary compile error: matchable with errors.Is and located at the use.
// The tie is two phase-0 globals of xtie keyed {a,b} and {a,c}, read by an
// identifier carrying {a,b,c}, as in TestUnboundDiagnosticSurvivesATieAtAnotherPhase.
func TestERCompareAmbiguousIdentifierIsACatchableLocatedError(t *testing.T) {
	t.Parallel()
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithGoSyntaxForms())
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	_, err = eng.EvalMultiple(ctx, `(define-syntax cmp (er-macro-transformer (lambda (f r c) (list (r 'quote) (c (car (cdr f)) (r 'xtie))))))`)
	qt.Assert(t, err, qt.IsNil)

	a := syntax.NewScope()
	b := syntax.NewScope()
	c := syntax.NewScope()
	x := values.NewSymbol("xtie")
	runtime := eng.Namespace().Runtime()
	runtime.MaybeCreateOwnGlobalBinding(x, environment.BindingTypeVariable, syntax.Scopes{}.Add(a).Add(b))
	runtime.MaybeCreateOwnGlobalBinding(x, environment.BindingTypeVariable, syntax.Scopes{}.Add(a).Add(c))

	expr, err := eng.ParseWithSource(ctx, "(cmp xtie)", "probe.scm")
	qt.Assert(t, err, qt.IsNil)
	stx := syntax.AddScopeToSyntax(syntax.AddScopeToSyntax(syntax.AddScopeToSyntax(expr.stx, a), b), c)

	_, err = eng.Compile(ctx, &Expression{stx: stx, source: "probe.scm"})
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrAmbiguousBinding), qt.IsTrue, qt.Commentf("got: %v", err))
	qt.Assert(t, err.Error(), qt.Contains, "probe.scm:1:", qt.Commentf("got: %v", err))
	qt.Assert(t, errors.Is(err, werr.ErrInternal), qt.IsFalse, qt.Commentf("got: %v", err))
}
