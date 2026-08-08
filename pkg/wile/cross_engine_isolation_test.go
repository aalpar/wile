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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

// TestExceptionHandlerBaseIsNotWritableFromScheme pins the immutable base of the
// process-wide %exception-handlers parameter (reviews/2026-08-07/REVIEW.md 2.1.9).
// The object is shared by every Engine as a continuation-mark key; the isolation
// argument rests on its base value never changing, and before ImmutableBase
// ordinary Scheme could rewrite it through the one-argument parameter form.
func TestExceptionHandlerBaseIsNotWritableFromScheme(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx, WithProfile(Tiny))
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(%exception-handlers (list (lambda (e) 'stolen)))`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrImmutableParameterBase), qt.IsTrue)

	// parameterize still binds it -- the refusal is of the base rewrite only, so
	// with-exception-handler and guard are unaffected.
	v, err := engine.EvalMultiple(ctx,
		`(guard (e (#t (list 'caught e))) (raise 'boom))`)
	c.Assert(err, qt.IsNil)
	c.Assert(v.SchemeString(), qt.Equals, "(caught boom)")
}

// TestExceptionHandlerNotStealableAcrossEngines is the end-to-end shape of the
// same defect: a Tiny engine poisoning the shared base so that conditions
// escaping another engine's handler stack are delivered to it instead.
func TestExceptionHandlerNotStealableAcrossEngines(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	attacker, err := NewEngine(ctx, WithProfile(Tiny))
	c.Assert(err, qt.IsNil)
	victim, err := NewEngine(ctx, WithProfile(KitchenSink))
	c.Assert(err, qt.IsNil)

	_, err = attacker.EvalMultiple(ctx, `(define stolen (list 'box #f))`)
	c.Assert(err, qt.IsNil)
	_, err = attacker.EvalMultiple(ctx,
		`(%exception-handlers (list (lambda (e) (set-car! (cdr stolen) e) 'ok)))`)
	c.Assert(errors.Is(err, werr.ErrImmutableParameterBase), qt.IsTrue)

	// The victim's non-matching guard re-raises; the condition must escape to the
	// victim's own embedder, not into the attacker's handler.
	_, err = victim.EvalMultiple(ctx, `(guard (ignore (#f #f)) (raise "B-SECRET-VALUE"))`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(strings.Contains(err.Error(), "B-SECRET-VALUE"), qt.IsTrue)

	stolen, err := attacker.EvalMultiple(ctx, `(cadr stolen)`)
	c.Assert(err, qt.IsNil)
	c.Assert(stolen.SchemeString(), qt.Equals, "#f")
}
