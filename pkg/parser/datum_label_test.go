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
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// readOneDatum parses a single datum from src for the datum-label tests.
func readOneDatum(src string) (syntax.SyntaxValue, error) {
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader(src))
	return p.ReadSyntax(context.TODO())
}

// TestReader_UndefinedDatumLabelIsReadError pins R7RS §2.4: a #n# reference with
// no matching #n= (a forward or undefined reference) is a read error, not the
// silently-substituted integer n that the old SyntaxDatumLabel fallback produced.
func TestReader_UndefinedDatumLabelIsReadError(t *testing.T) {
	cases := []struct {
		name string
		src  string
	}{
		{"undefined forward reference", "#5#"},
		// R7RS §2.4: a label whose value is its own reference (#n= #n#) is
		// ill-defined; the inner #0# is not yet registered, so it is undefined.
		{"self-referential label", "#0=#0#"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, err := readOneDatum(tc.src)
			if err == nil {
				t.Fatalf("ReadSyntax(%q) = nil error; want an undefined-datum-label read error", tc.src)
			}
			if !errors.Is(err, werr.ErrDatumLabelUndefined) {
				t.Fatalf("ReadSyntax(%q) error = %v; want errors.Is ErrDatumLabelUndefined", tc.src, err)
			}
			var perr *ParserError
			if !errors.As(err, &perr) {
				t.Fatalf("ReadSyntax(%q) error = %T; want a located *ParserError", tc.src, err)
			}
		})
	}
}

// TestReader_CircularVectorResolvesSelfReference pins read-level circular-vector
// support (R7RS §2.4): #0=#(1 #0#) reads as a vector whose element 1 is the
// vector itself, mirroring the pre-registered circular-list path.
func TestReader_CircularVectorResolvesSelfReference(t *testing.T) {
	q, err := readOneDatum("#0=#(1 #0#)")
	if err != nil {
		t.Fatalf("ReadSyntax error: %v", err)
	}
	asg, ok := q.(*syntax.SyntaxDatumLabelAssignment)
	if !ok {
		t.Fatalf("got %T, want *SyntaxDatumLabelAssignment", q)
	}
	vec, ok := asg.Value.(*syntax.SyntaxVector)
	if !ok {
		t.Fatalf("labeled value is %T, want *SyntaxVector", asg.Value)
	}
	if len(vec.Values) != 2 {
		t.Fatalf("vector has %d elements, want 2", len(vec.Values))
	}
	elem1, ok := vec.Values[1].(*syntax.SyntaxVector)
	if !ok || elem1 != vec {
		t.Fatalf("element 1 = %v (%T), want the vector itself (circular self-reference)", vec.Values[1], vec.Values[1])
	}
}

// TestReader_CircularVectorRoundTrips proves the data-path round-trip that
// datum labels exist for (R7RS §2.4): reading #0=#(1 #0#) and writing the
// unwrapped value reproduces the same notation, so write output re-reads.
func TestReader_CircularVectorRoundTrips(t *testing.T) {
	q, err := readOneDatum("#0=#(1 #0#)")
	if err != nil {
		t.Fatalf("ReadSyntax error: %v", err)
	}
	got := values.WriteValueToString(q.UnwrapAll())
	want := "#0=#(1 #0#)"
	if got != want {
		t.Fatalf("circular vector round-trip = %q, want %q", got, want)
	}
}

// TestReader_VectorClose covers readVectorInto's delimiter handling (the
// function this change factored out of readVector). A vector opened with #(
// must close with ); a non-) closer is a located mismatch error, and a trailing
// datum comment before the close must not leave a nil element — readVectorInto
// must mirror readList's nil-on-close handling.
func TestReader_VectorClose(t *testing.T) {
	t.Run("bracket-closed vector is a located mismatch error", func(t *testing.T) {
		_, err := readOneDatum("#(1 2]")
		if err == nil {
			t.Fatal("#(1 2] accepted; want a located close-delimiter error")
		}
		if !errors.Is(err, werr.ErrNotACloseParen) {
			t.Fatalf("#(1 2] error = %v; want errors.Is ErrNotACloseParen", err)
		}
	})
	t.Run("trailing datum comment closes cleanly", func(t *testing.T) {
		q, err := readOneDatum("#(1 #;2 )")
		if err != nil {
			t.Fatalf("#(1 #;2 ) rejected: %v", err)
		}
		vec, ok := q.UnwrapAll().(*values.Vector)
		if !ok || len(*vec) != 1 {
			t.Fatalf("#(1 #;2 ) = %s, want a 1-element vector #(1)", q.UnwrapAll().SchemeString())
		}
	})
}

// TestReader_DatumLabelValidFormsResolve guards the already-working label paths
// against regression from the undefined-label and circular-vector changes.
func TestReader_DatumLabelValidFormsResolve(t *testing.T) {
	cases := []string{
		"#0=(1 . #0#)", // circular list (pre-registered pair placeholder)
		"#0=#(1 2 3)",  // non-circular labeled vector
		"#0=42",        // labeled atom
	}
	for _, src := range cases {
		t.Run(src, func(t *testing.T) {
			_, err := readOneDatum(src)
			if err != nil {
				t.Fatalf("ReadSyntax(%q) rejected a valid labeled form: %v", src, err)
			}
		})
	}
}
