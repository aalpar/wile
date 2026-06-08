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

package wile_test

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/werr"
)

// A recursive macro whose output grows structural nesting one level per input
// element is the canonical way deep syntax reaches the expander without passing
// through the parser (the parser only bounds textual nesting, and the input
// list here is flat). `(gen (a a ... a) 0)` with N elements expands to N nested
// (list ...) wrappers, re-expanded at each step. This exercises the engine's
// WithMaxExpandDepth knob end-to-end: a tight bound must return a catchable
// ErrExpandDepthExceeded rather than crash the host with a fatal stack overflow.
func TestEngine_WithMaxExpandDepth_Trips(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx, wile.WithMaxExpandDepth(100))
	if err != nil {
		t.Fatal(err)
	}
	const n = 600 // expansion depth ~n, comfortably over the limit of 100
	src := "(define-syntax gen (syntax-rules () ((_ () acc) acc) ((_ (x . xs) acc) (gen xs (list acc)))))" +
		" (gen (" + strings.Repeat("a ", n) + ") 0)"
	_, err = engine.EvalMultiple(ctx, src)
	if err == nil {
		t.Fatal("expected ErrExpandDepthExceeded for deep macro-generated nesting, got nil")
	}
	if !errors.Is(err, werr.ErrExpandDepthExceeded) {
		t.Fatalf("expected ErrExpandDepthExceeded, got: %v", err)
	}
}

// The same program under the default bound (50000) must expand and run without
// error — the bound does not regress ordinary recursive-macro use.
func TestEngine_WithMaxExpandDepth_DefaultAllowsOrdinary(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx)
	if err != nil {
		t.Fatal(err)
	}
	const n = 600
	src := "(define-syntax gen (syntax-rules () ((_ () acc) acc) ((_ (x . xs) acc) (gen xs (list acc)))))" +
		" (gen (" + strings.Repeat("a ", n) + ") 0)"
	_, err = engine.EvalMultiple(ctx, src)
	if err != nil {
		t.Fatalf("depth-%d macro nesting under the default bound should succeed, got: %v", n, err)
	}
}
