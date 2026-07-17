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

package validate

import (
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
)

// TestCallbackIsCaptureSafe pins the D2 call-site proof (callback specialization
// Strategy A): a callback argument is provably capture-safe ONLY with positive
// proof — a symbol resolving to a CaptureSafe+Stable binding, or a lambda literal
// whose body provably cannot capture. Everything else (a procedure-invoking
// symbol, a capturing lambda, a computed operator, an unbound or rebindable
// symbol) is conservatively unsafe. The asymmetry is the soundness invariant: a
// false negative only forgoes the optimization, but a false positive would let the
// inliner release a frame across a capturing call.
func TestCallbackIsCaptureSafe(t *testing.T) {
	// Positive: a symbol bound to a capture-safe + stable primitive.
	if !CallbackIsCaptureSafe(symRef("+"), envWithImported(t, "+")) {
		t.Errorf("a symbol bound to a capture-safe+stable primitive must be capture-safe")
	}

	// Negative: a procedure-invoking primitive (map applies a user proc). It is
	// Imported (Stable) but NOT CaptureSafe, so the && gate rejects it.
	if CallbackIsCaptureSafe(symRef("map"), envWithImported(t, "map")) {
		t.Errorf("a procedure-invoking primitive symbol must NOT be capture-safe")
	}

	// Negative: an unbound symbol — no binding to prove anything from.
	if CallbackIsCaptureSafe(symRef("nope"), envWithImported(t)) {
		t.Errorf("an unbound symbol must NOT be capture-safe")
	}

	// Positive: a pure lambda literal (body is a literal: no capture operator, no
	// callees) is capture-safe by ProcedureBodyIsCaptureSafe.
	if !CallbackIsCaptureSafe(lam(lit()), envWithImported(t)) {
		t.Errorf("a lambda with a capture-safe body must be capture-safe")
	}

	// Negative: a lambda whose body calls call/cc — a capture operator.
	captureLambda := lam(call(symRef("call/cc"), symRef("k")))
	if CallbackIsCaptureSafe(captureLambda, envWithImported(t, "call/cc", "k")) {
		t.Errorf("a lambda whose body calls call/cc must NOT be capture-safe")
	}

	// Negative: a computed callback (a call expression — neither symbol nor lambda),
	// e.g. the (car fns) in (for-each (car fns) xs). Falls to the conservative default.
	if CallbackIsCaptureSafe(call(symRef("car"), symRef("fns")), envWithImported(t)) {
		t.Errorf("a computed callback (non-symbol, non-lambda) must NOT be capture-safe")
	}

	// Negative: a symbol that is CaptureSafe but NOT Stable (rebindable). Both are
	// required, mirroring the frame-reclaim classifier's IsCaptureSafe()&&IsStable()
	// gate — a capture-safe value reachable through a rebindable name could be
	// replaced by a capturing one.
	env := environment.NewNamespace().Runtime()
	sym := syntax.NewSyntaxSymbol("unstable", nil).Sym
	env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
	// CaptureSafe but not Imported/Stable.
	env.GetBinding(sym, nil).UpdateMeta(func(m *environment.BindingMeta) bool {
		m.CaptureSafe = true
		return true
	})
	if CallbackIsCaptureSafe(symRef("unstable"), env) {
		t.Errorf("a CaptureSafe-but-rebindable symbol must NOT be capture-safe (Stable is required)")
	}

	// Defensive: a nil env cannot prove anything.
	if CallbackIsCaptureSafe(symRef("+"), nil) {
		t.Errorf("a nil env must yield not-capture-safe")
	}
}
