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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// TestSetBangRefusalLocatesTheIdentifier pins the defect filed 2026-09-09:
// CompileValidatedSetBang's ErrNoSuchBinding arm wraps wrapSourcedError so the
// reported location is the offending NAME, and its two ErrImmutableBinding
// siblings returned a bare wrap with no SourcedError at all. wrapCompilationError
// then fell back to whatever located cause was left in the chain — a DIFFERENT
// form, on a different LINE.
//
// Measured before the fix, through this harness: rows 1 and 2 reported
// pin.scm:4:1 — the enclosing set! FORM — against the identifier's 4:6. The
// filing's own measurement, taken through the CLI, reported ti.scm:1:0, i.e. the
// import form on another line entirely; the CLI wraps a file in (begin …), so the
// nearest located cause it falls back to is further away. Same defect, two
// distances, and the assertion is the full "file:line:col" rather than a
// substring so neither can drift past it.
//
// Row 3 is a CONTROL, green before and after: it is the already-stamped sibling,
// and it is what says 4:6 is that arm's own answer rather than an invented
// number. Only rows 1 and 2 are the ratchet.
func TestSetBangRefusalLocatesTheIdentifier(t *testing.T) {
	tests := []struct {
		name    string
		src     string
		wantErr error
		want    string
	}{
		{
			// The engine default is an immutable top level, and R7RS §5.2 refuses
			// set! on an imported binding regardless.
			name:    "ratchet: imported binding",
			src:     "(import (scheme base))\n\n\n(set! car 1)\n",
			wantErr: werr.ErrImmutableBinding,
			want:    "pin.scm:4:6",
		},
		{
			// IsStable(), the frame-reclaim anchor, which the engine default arms.
			name:    "ratchet: immutable top-level binding",
			src:     "(define zz 1)\n\n\n(set! zz 2)\n",
			wantErr: werr.ErrImmutableBinding,
			want:    "pin.scm:4:6",
		},
		{
			name:    "control (green before and after): the stamped unbound arm",
			src:     ";; pad\n;; pad\n;; pad\n(set! qqq 2)\n",
			wantErr: werr.ErrNoSuchBinding,
			want:    "pin.scm:4:6",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()
			eng, err := wile.NewEngine(ctx,
				wile.WithProfile(wile.KitchenSink),
				wile.WithSourceFS(stdlib.FS),
				wile.WithLibraryPaths("."),
			)
			c.Assert(err, qt.IsNil)
			defer func() {
				_ = eng.Close()
			}()

			_, err = eng.EvalMultipleWithSource(ctx, tc.src, "pin.scm")
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.wantErr), qt.IsTrue,
				qt.Commentf("sentinel identity must survive the stamp; got %v", err))

			var compErr *wile.CompilationError
			c.Assert(errors.As(err, &compErr), qt.IsTrue)
			c.Assert(compErr.Source, qt.Equals, tc.want,
				qt.Commentf("the location must name the set! identifier, not the enclosing "+
					"or preceding form; full error: %v", err))
		})
	}
}
