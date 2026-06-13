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
	"testing"
)

// TestFrameReclaimSeam_RealScopeResolution exercises the Stable→classifier seam
// through the REAL compile pipeline under `go test ./...` (no WILE_FRAME_RECLAIM_
// MEASURE gate, no benchmark execution). The gated measurement harness is the
// only OTHER place real (compiled, hygienic) scopes flow into the classifier's
// immutability read, so without this the split-key path — `byName[name]` resolves
// the edge TARGET by Key while `env.GetBinding(sym, sym.Scopes())` resolves
// IMMUTABILITY by scope (plan OQ-1) — has zero CI coverage. The internal/validate
// unit tests all use nil scopes (match-any), which cannot exercise scope-aware
// resolution.
//
// This pins the SAFE direction: a genuine self-recursive define's self-edge must
// resolve through real scopes to its own Stable binding ⇒ reclaimable when the
// flag stamps Stable, not reclaimable when it does not. classifyCompiled also
// runs assertStampLanded, so the stamp-landed positive control is exercised here
// too.
func TestFrameReclaimSeam_RealScopeResolution(t *testing.T) {
	ctx := context.Background()
	// fib over imported, capture-safe primitives (<, +, -); its only same-unit
	// edges are the two self-calls.
	wrapped := "(begin (define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) )"

	off, err := classifyCompiled(ctx, wrapped, false)
	if err != nil {
		t.Fatalf("flag off: %v", err)
	}
	if off["fib"] {
		t.Fatalf("flag off: fib must NOT be reclaimable (self-edge mutable, no Stable bit)")
	}

	on, err := classifyCompiled(ctx, wrapped, true)
	if err != nil {
		t.Fatalf("flag on: %v", err)
	}
	if !on["fib"] {
		t.Fatalf("flag on: fib's real-scope self-edge must resolve to its Stable binding ⇒ reclaimable")
	}
}

// TestFrameReclaimSeam_LocalShadowSound is the end-to-end OQ-1 guard through the
// real compile pipeline: a local binding that shadows a Stable top-level define
// name in operator position must NOT make its enclosing define reclaimable. The
// runtime callee is the local (here the parameter h), not the Stable top-level
// sq, so reclaiming the frame would corrupt a continuation h might capture. The
// internal/validate unit test exercises the same fix synthetically; this proves
// the real hygienic scopes flowing through compile do not defeat the guard.
func TestFrameReclaimSeam_LocalShadowSound(t *testing.T) {
	ctx := context.Background()
	wrapped := "(begin (define (sq x) (* x x)) (define (use h) (let ((sq h)) (sq 3))) )"

	on, err := classifyCompiled(ctx, wrapped, true)
	if err != nil {
		t.Fatalf("flag on: %v", err)
	}
	if !on["sq"] {
		t.Fatalf("sanity: sq over only the capture-safe primitive * must be reclaimable")
	}
	if on["use"] {
		t.Fatalf("OQ-1: use must NOT be reclaimable — (sq 3) calls the local parameter h, not the Stable top-level sq")
	}
}
