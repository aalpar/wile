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

package syntax

import (
	"errors"
	"testing"

	"github.com/aalpar/wile/werr"
)

func TestFormParts(t *testing.T) {
	mksym := func(s string) SyntaxValue {
		return NewSyntaxSymbol(s, nil)
	}
	proper := func(syms ...string) SyntaxValue {
		elems := make([]SyntaxValue, len(syms))
		for i, s := range syms {
			elems[i] = mksym(s)
		}
		return SyntaxList(nil, elems...)
	}
	improper := NewSyntaxCons(mksym("a"), mksym("b"), nil) // (a . b)

	tests := []struct {
		name    string
		form    SyntaxValue
		minLen  int
		maxLen  int
		wantLen int
		wantErr bool
	}{
		{"exact match", proper("a", "b"), 2, 2, 2, false},
		{"exact too few", proper("a"), 2, 2, 0, true},
		{"exact too many", proper("a", "b", "c"), 2, 2, 0, true},
		{"unbounded ok", proper("a", "b", "c"), 1, -1, 3, false},
		{"at-least too few", proper("a"), 2, -1, 0, true},
		{"range within", proper("a", "b"), 1, 3, 2, false},
		{"range over", proper("a", "b", "c", "d"), 1, 3, 0, true},
		{"improper list", improper, 1, -1, 0, true},
		{"empty form min 0", SyntaxEmptyList, 0, -1, 0, false},
		{"empty form min 1", SyntaxEmptyList, 1, 1, 0, true},
		{"non-list form", mksym("x"), 1, 1, 0, true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			parts, err := FormParts(tt.form, "test-form", tt.minLen, tt.maxLen)
			if tt.wantErr {
				if err == nil {
					t.Fatalf("expected error, got nil (parts=%v)", parts)
				}
				if !errors.Is(err, werr.ErrInvalidSyntax) {
					t.Errorf("expected ErrInvalidSyntax, got %v", err)
				}
				if parts != nil {
					t.Errorf("expected nil parts on error, got %v", parts)
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if len(parts) != tt.wantLen {
				t.Fatalf("expected %d parts, got %d", tt.wantLen, len(parts))
			}
		})
	}
}

// TestFormPartsPreservesOrder verifies elements are returned positionally,
// in source order, with identity preserved.
func TestFormPartsPreservesOrder(t *testing.T) {
	a := NewSyntaxSymbol("a", nil)
	b := NewSyntaxSymbol("b", nil)
	form := SyntaxList(nil, a, b)

	parts, err := FormParts(form, "f", 2, 2)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if parts[0] != a {
		t.Errorf("parts[0]: expected the 'a' symbol, got %v", parts[0])
	}
	if parts[1] != b {
		t.Errorf("parts[1]: expected the 'b' symbol, got %v", parts[1])
	}
}
