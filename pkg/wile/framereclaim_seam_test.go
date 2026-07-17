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

// TestFrameReclaimSeam_RealScopeResolution exercises the frame-reclaim classifier
// through the REAL compile pipeline under `go test ./...` (no WILE_FRAME_RECLAIM_
// MEASURE gate, no benchmark execution). Real (compiled, hygienic) scopes flow
// into the classifier here: capture-safe primitive callees (<, +, -) resolve
// through `env.GetBinding(sym, sym.Scopes())`, and the lexical shadow guard reads
// the same scopes — the internal/validate unit tests use nil scopes (match-any)
// and cannot exercise scope-aware resolution.
//
// This pins the SAFE direction on a genuine self-recursive define. The same-unit
// self-edge's immutability comes from the callee node's rebindStable
// (StableInUnit ∧ immutable-top), computed thread-locally — NOT from a read of the
// callee's shared *Binding (the T1.5 follow-on removed that dependency and its
// transient pre-stamp). So fib is reclaimable under the flag and not reclaimable
// without it, because rebindStable carries the flag conjunct. classifyCompiled
// also runs assertStampLanded, so the binding stamp (still produced for the
// redefine/set! guards) is verified here too.
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
