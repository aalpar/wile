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
	"context"
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/werr"
)

// TestPopWindInternalErrorStaysUncatchable is Wave 4 item 3's negative guard
// (Neg2a.1). It pins the arm the fix must NOT take.
//
// Item 3's fix bridges the `after`-operand check inside OperationPushWind.Apply.
// The tempting alternative — bridge once, at Run()'s OpComplex arm — is wrong,
// and nothing else in the suite can see why: OpComplex dispatches every
// side-table operation (op_kind.go's OpKind()==OpComplex set), so a blanket
// bridge would also route
// OperationPopWind's werr.ErrInternal ("winding stack is empty; PushWind/PopWind
// pairing is broken") through applyCallableError and make an impossible VM state
// `guard`-able. CODING_STYLE.md forbids exactly that: "Internal invariant
// violations ('impossible' states) stay werr or panic — a Scheme programmer
// should never be able to guard them."
//
// The assertion therefore runs the operation THROUGH the OpComplex dispatch
// rather than calling Apply directly: a bridge installed at that arm would
// convert the error before Run() returns it, and calling Apply by hand would
// step over the very site being guarded.
//
// This guard is green at its own commit — the invariant already holds. It was
// verified red against a throwaway spike that replaced Run()'s OpComplex arm
// with `return err` → `mc, err = bridgeForeignError(mc, err)`; under that spike
// the ErrExceptionEscape assertion below fails. The spike's diff hash is
// recorded in this commit's message.
func TestPopWindInternalErrorStaysUncatchable(t *testing.T) {
	ctx := context.Background()
	env := environment.NewNamespace().Runtime()

	// One PopWind against an empty winding stack: the underflow the compiler's
	// PushWind/PopWind pairing is supposed to make impossible.
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendOperations(NewOperationPopWind())
	qt.Assert(t, tpl.Code()[0].Op, qt.Equals, OpComplex,
		qt.Commentf("PopWind must dispatch via the side table, or this guard misses its site"))

	mc := AcquireTopLevelContext(ctx, tpl, env)
	defer ReleaseTopLevelContext(mc)

	err := mc.Run()

	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrInternal), qt.IsTrue,
		qt.Commentf("expected the raw werr.ErrInternal invariant error, got %T: %v", err, err))

	// The discriminator. If the OpComplex arm ever bridges, the error arrives as
	// the uncaught-exception carrier (or, with a handler installed, does not
	// arrive at all) — either way it has become a Scheme condition.
	var escape *ErrExceptionEscape
	qt.Assert(t, errors.As(err, &escape), qt.IsFalse,
		qt.Commentf("pop-wind's invariant violation must not become a catchable Scheme condition"))
}
