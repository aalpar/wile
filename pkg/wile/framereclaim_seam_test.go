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
// through `env.GetBinding(sym, syntax.ScopesOf(sym.Scopes()))`, and the lexical shadow guard reads
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
	if reclaimByName(off)["fib"] {
		t.Fatalf("flag off: fib must NOT be reclaimable (self-edge mutable, no Stable bit)")
	}

	on, err := classifyCompiled(ctx, wrapped, true)
	if err != nil {
		t.Fatalf("flag on: %v", err)
	}
	if !reclaimByName(on)["fib"] {
		t.Fatalf("flag on: fib's real-scope self-edge must resolve to its Stable binding ⇒ reclaimable")
	}
}

// TestFrameReclaimSeam_LetNestedMutualCallResolves pins the SUBSET direction of
// same-unit edge resolution — the case fingerprint equality cannot handle, and the
// case a self-recursive define does NOT exercise (its body is independently
// capture-safe, so its binding's CaptureSafe flag makes the self-edge redundant).
//
// Mutual recursion ff↔gg, with ff's cross-call to gg nested one let deep so gg's
// operator scopes strictly exceed gg's define-name scopes (verified: name-fp="" vs
// ref-fp a fresh let-scope id). NEITHER define is independently capture-safe (each
// depends on the other), so the CaptureSafe fallback cannot fire — the ff→gg edge
// resolves ONLY under env.GetBinding's subset semantics (validate.resolveNodeByScopes).
// Keying edges by equal fingerprints misses it, leaving ff with an unresolved
// (unsafe) edge and BOTH defines non-reclaimable. Measured: the same program yields
// ff=gg=false under equality-keyed edges, ff=gg=true here. A regression to equality
// fails HERE while leaving the direct-self-call seam test above green.
func TestFrameReclaimSeam_LetNestedMutualCallResolves(t *testing.T) {
	ctx := context.Background()
	wrapped := "(begin " +
		"(define (ff n) (if (< n 1) n (let ((m (- n 1))) (gg m)))) " +
		"(define (gg n) (if (< n 1) n (ff (- n 1)))) )"

	// Flag-off control: mutual recursion over mutable (non-Stable) edges is not
	// reclaimable, whichever way the edge resolves — so a green flag-off here proves
	// the flag-on recovery below is the Stable bit, not a resolution artifact.
	off, err := classifyCompiled(ctx, wrapped, false)
	if err != nil {
		t.Fatalf("flag off: %v", err)
	}
	offV := reclaimByName(off)
	if offV["ff"] || offV["gg"] {
		t.Fatalf("flag off: mutual recursion over mutable edges must NOT be reclaimable — got ff=%v gg=%v", offV["ff"], offV["gg"])
	}

	on, err := classifyCompiled(ctx, wrapped, true)
	if err != nil {
		t.Fatalf("flag on: %v", err)
	}
	onV := reclaimByName(on)
	if !onV["ff"] {
		t.Fatalf("flag on: ff's let-nested cross-call to gg must resolve by subset (not equality) ⇒ reclaimable")
	}
	if !onV["gg"] {
		t.Fatalf("flag on: gg (calls the now-reclaimable ff) must be reclaimable")
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
	if !reclaimByName(on)["sq"] {
		t.Fatalf("sanity: sq over only the capture-safe primitive * must be reclaimable")
	}
	if reclaimByName(on)["use"] {
		t.Fatalf("OQ-1: use must NOT be reclaimable — (sq 3) calls the local parameter h, not the Stable top-level sq")
	}
}
