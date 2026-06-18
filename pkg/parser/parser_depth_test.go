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

package parser

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/werr"
)

// Deeply nested parens must return a catchable ErrParseDepthExceeded,
// never crash with a fatal Go stack overflow.
func TestParser_DepthLimit_Trips(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	// Well past DefaultMaxParseDepth (10000) but tiny in memory.
	src := strings.Repeat("(", 50000)
	p := NewParser(env, true, strings.NewReader(src))
	_, err := p.ReadSyntax(context.TODO())
	if err == nil {
		t.Fatal("expected depth-limit error, got nil")
	}
	if !errors.Is(err, werr.ErrParseDepthExceeded) {
		t.Fatalf("expected ErrParseDepthExceeded, got: %v", err)
	}
}

// Each compound form recurses through readSyntax and must be bounded.
func TestParser_DepthLimit_AllForms(t *testing.T) {
	cases := []struct {
		name   string
		prefix string // repeated to build nesting
	}{
		{"list", "("},
		{"bracket", "["},
		{"vector", "#("},
		{"quote", "'"},
		{"quasiquote", "`"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			src := strings.Repeat(tc.prefix, 50000)
			p := NewParser(env, true, strings.NewReader(src))
			_, err := p.ReadSyntax(context.TODO())
			if !errors.Is(err, werr.ErrParseDepthExceeded) {
				t.Fatalf("%s: expected ErrParseDepthExceeded, got: %v", tc.name, err)
			}
		})
	}
}

// Nesting within the limit must parse without error.
func TestParser_DepthLimit_WithinLimitOK(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	depth := 1000 // well under 10000
	src := strings.Repeat("(", depth) + "1" + strings.Repeat(")", depth)
	p := NewParser(env, true, strings.NewReader(src))
	q, err := p.ReadSyntax(context.TODO())
	if err != nil {
		t.Fatalf("valid depth-%d nesting should parse, got: %v", depth, err)
	}
	if q == nil {
		t.Fatal("expected a syntax value, got nil")
	}
}

// The limit is configurable; SetMaxDepth(0) disables it for callers with
// legitimately deep machine-generated data (bounded here so the test is cheap).
func TestParser_DepthLimit_Configurable(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	src := strings.Repeat("(", 50) + "1" + strings.Repeat(")", 50)

	// A tight limit trips.
	p := NewParser(env, true, strings.NewReader(src))
	p.SetMaxDepth(10)
	_, err := p.ReadSyntax(context.TODO())
	if !errors.Is(err, werr.ErrParseDepthExceeded) {
		t.Fatalf("tight limit should trip, got: %v", err)
	}

	// SetMaxDepth(0) disables the check.
	p2 := NewParser(env, true, strings.NewReader(src))
	p2.SetMaxDepth(0)
	_, err = p2.ReadSyntax(context.TODO())
	if err != nil {
		t.Fatalf("disabled limit should parse, got: %v", err)
	}
}

// Depth must reset between successive top-level reads, so a long stream of
// shallow expressions does not accumulate depth and falsely trip.
func TestParser_DepthLimit_ResetsBetweenReads(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	src := strings.Repeat("(a) ", 20000) // 20000 shallow forms, depth never exceeds ~2
	p := NewParser(env, true, strings.NewReader(src))
	for i := range 20000 {
		_, err := p.ReadSyntax(context.TODO())
		if err != nil {
			t.Fatalf("read %d should succeed (depth must reset), got: %v", i, err)
		}
	}
}
