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

// White-box (package wile) so the direct-InstallForms test can name the internal
// *forms.FormRegistry, matching dialect_test.go.
package wile

import (
	"context"
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// TestR7RSMinimal_Name pins the dialect identity.
func TestR7RSMinimal_Name(t *testing.T) {
	c := qt.New(t)
	c.Assert(R7RSMinimal, qt.IsNotNil)
	c.Assert(R7RSMinimal.Name(), qt.Equals, "r7rs-minimal")
}

// TestR7RSMinimal_InstallForms_RemovesSetBang proves InstallForms removes set!
// from a registry that had it (r7rs-minimal derives from the R7RS default) while
// leaving the other special forms intact.
func TestR7RSMinimal_InstallForms_RemovesSetBang(t *testing.T) {
	c := qt.New(t)
	fr := forms.DefaultRegistry().Clone()
	before := len(fr.Names())
	c.Assert(fr.Lookup("set!"), qt.IsNotNil,
		qt.Commentf("set! is part of the R7RS baseline the dialect derives from"))

	err := R7RSMinimal.InstallForms(fr)
	c.Assert(err, qt.IsNil)
	c.Assert(fr.Lookup("set!"), qt.IsNil,
		qt.Commentf("r7rs-minimal must remove the set! special form"))
	// Exactly one form removed — set! only, nothing else (derives from the R7RS
	// baseline minus set!).
	c.Assert(len(fr.Names()), qt.Equals, before-1,
		qt.Commentf("r7rs-minimal must remove ONLY set!, not other forms"))
	c.Assert(fr.Lookup("if"), qt.IsNotNil)
	c.Assert(fr.Lookup("lambda"), qt.IsNotNil)
}

// TestR7RSMinimal_Engine_SetBangGone_RestIntact is the end-to-end validation: an
// engine on r7rs-minimal treats set! as an unbound reference, the rest of R7RS
// still works, and a default engine retains set!.
func TestR7RSMinimal_Engine_SetBangGone_RestIntact(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithDialect(R7RSMinimal))
	c.Assert(err, qt.IsNil)

	// set! is no longer a special form → unbound reference.
	_, err = eng.EvalMultiple(ctx, "(let ((x 1)) (set! x 2) x)")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
		qt.Commentf("set! must be gone under r7rs-minimal, got %v", err))
	c.Assert(err.Error(), qt.Contains, "set!",
		qt.Commentf("the unbound identifier must be set! specifically"))

	// The rest of R7RS is intact (named let / if / arithmetic, no set!).
	got, err := eng.EvalMultiple(ctx,
		"(let loop ((i 0) (acc 0)) (if (< i 5) (loop (+ i 1) (+ acc i)) acc))")
	c.Assert(err, qt.IsNil)
	c.Assert(got.SchemeString(), qt.Equals, "10")

	// A default engine retains set! — the difference is the dialect, not the build.
	base, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)
	_, err = base.EvalMultiple(ctx, "(let ((x 1)) (set! x 2) x)")
	c.Assert(err, qt.IsNil)
}

// TestR7RSMinimal_MutationPrimitivesRemain pins the documented scope boundary:
// the forms-only dialect removes the set! FORM but NOT the mutation PRIMITIVES
// (set-car! etc.), whose removal is the separate registry-filtering track. This
// guards against the name being mistaken for full no-mutation.
func TestR7RSMinimal_MutationPrimitivesRemain(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithDialect(R7RSMinimal))
	c.Assert(err, qt.IsNil)

	got, err := eng.EvalMultiple(ctx, "(let ((p (cons 1 2))) (set-car! p 9) (car p))")
	c.Assert(err, qt.IsNil,
		qt.Commentf("set-car! is a primitive, not removed by the forms-only r7rs-minimal dialect"))
	c.Assert(got.SchemeString(), qt.Equals, "9")
}
