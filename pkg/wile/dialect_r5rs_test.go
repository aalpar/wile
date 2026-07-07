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

// White-box (package wile) so the tests can name the internal *forms.FormRegistry
// and the r5rsRemovedForms table, matching dialect_test.go / dialect_minimal_test.go.
package wile

import (
	"context"
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// TestR5RSStrict_Name pins the dialect identity.
func TestR5RSStrict_Name(t *testing.T) {
	c := qt.New(t)
	c.Assert(R5RSStrict, qt.IsNotNil)
	c.Assert(R5RSStrict.Name(), qt.Equals, "r5rs-strict")
}

// TestR5RSStrict_InstallForms_RemovesEffectiveForms proves every form in
// r5rsRemovedForms is present in the R7RS baseline and removed by InstallForms,
// that EXACTLY those (nothing else) are removed, and that the R5RS core plus
// case-lambda (retained for Wile's bootstrap) survive.
func TestR5RSStrict_InstallForms_RemovesEffectiveForms(t *testing.T) {
	c := qt.New(t)
	fr := forms.DefaultRegistry().Clone()
	before := len(fr.Names())

	// Both removal slices name real baseline forms. Present-before: iterate the same
	// slices the removal uses, so a typo'd or non-baseline entry is caught (not just a
	// vacuous nil-after). That every R6RS macro form has a FormSpec here is the fact
	// that makes forms-layer removal effective for them.
	removed := append(append([]string{}, r5rsRemovedForms...), r5rsRemovedMacroForms...)
	for _, n := range removed {
		c.Assert(fr.Lookup(n), qt.IsNotNil,
			qt.Commentf("%q must be in the R7RS baseline the dialect derives from", n))
	}

	err := R5RSStrict.InstallForms(fr)
	c.Assert(err, qt.IsNil)

	for _, n := range removed {
		c.Assert(fr.Lookup(n), qt.IsNil,
			qt.Commentf("r5rs-strict must remove %q", n))
	}
	// Exactly the two removal slices gone — nothing over-removed.
	c.Assert(len(fr.Names()), qt.Equals, before-len(removed),
		qt.Commentf("r5rs-strict must remove exactly r5rsRemovedForms+r5rsRemovedMacroForms, nothing else"))
	// R5RS §4.3 macro system and core forms are kept.
	for _, n := range []string{"if", "lambda", "define", "set!", "let", "let*", "letrec", "define-syntax", "syntax-rules", "let-syntax", "letrec-syntax", "case-lambda"} {
		c.Assert(fr.Lookup(n), qt.IsNotNil,
			qt.Commentf("r5rs-strict must keep %q", n))
	}
}

// TestR5RSStrict_Engine_R5RSWorks_R7RSFormsRejected is the end-to-end validation:
// R5RS syntax works, and every removed R7RS-only form is rejected as an unbound
// identifier (pinning the mechanism, not just any error). A default engine
// accepts the ones that evaluate cleanly, proving the difference is the dialect.
func TestR5RSStrict_Engine_R5RSWorks_R7RSFormsRejected(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithDialect(R5RSStrict))
	c.Assert(err, qt.IsNil)

	// R5RS syntax: named let, if, arithmetic.
	got, err := eng.EvalMultiple(ctx,
		"(let loop ((i 0) (acc 0)) (if (< i 5) (loop (+ i 1) (+ acc i)) acc))")
	c.Assert(err, qt.IsNil)
	c.Assert(got.SchemeString(), qt.Equals, "10")
	// R5RS hygienic macro.
	got, err = eng.EvalMultiple(ctx, "(let-syntax ((m (syntax-rules () ((_ a) (+ a 1))))) (m 41))")
	c.Assert(err, qt.IsNil)
	c.Assert(got.SchemeString(), qt.Equals, "42")

	// Every removed form is rejected as an unbound identifier (well-formed use).
	rejects := []struct{ name, src string }{
		{"letrec*", "(letrec* ((x 1)) x)"},
		{"cond-expand", "(cond-expand (else 1))"},
		{"include", `(include "zzz-nofile")`},
		{"include-ci", `(include-ci "zzz-nofile")`},
		{"with-continuation-mark", "(with-continuation-mark (quote k) (quote v) 1)"},
	}
	for _, r := range rejects {
		_, err := eng.EvalMultiple(ctx, r.src)
		c.Assert(err, qt.IsNotNil,
			qt.Commentf("%q must be rejected under r5rs-strict", r.name))
		c.Assert(errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
			qt.Commentf("%q must fail as an unbound reference, got %v", r.name, err))
		c.Assert(err.Error(), qt.Contains, r.name,
			qt.Commentf("the unbound identifier must be %q", r.name))
	}

	// Isolation: a default engine accepts the cleanly-evaluating removed forms,
	// so the rejection above is the dialect's doing, not the build's.
	base, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)
	_, err = base.EvalMultiple(ctx, "(letrec* ((x 1)) x)")
	c.Assert(err, qt.IsNil, qt.Commentf("default engine accepts letrec*"))
	_, err = base.EvalMultiple(ctx, "(cond-expand (else 1))")
	c.Assert(err, qt.IsNil, qt.Commentf("default engine accepts cond-expand"))
}

// TestR5RSStrict_ImportNotDisabled_ExpanderCeiling pins the documented ceiling:
// import (and the rest of the library/module system) is handled by the EXPANDER,
// which does not consult the forms registry, so forms-only removal cannot disable
// it. R5RSStrict deliberately omits it; under R5RSStrict, import still fails the
// way it does on a default bare engine (no library registry) — NOT as an unbound
// identifier. Guards against re-adding import to r5rsRemovedForms on the false
// belief it takes effect.
func TestR5RSStrict_ImportNotDisabled_ExpanderCeiling(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithDialect(R5RSStrict))
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, "(import (scheme base))")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNoSuchBinding), qt.IsFalse,
		qt.Commentf("import is expander-driven; forms removal must not turn it into an unbound ref (got %v)", err))
}

// TestR5RSStrict_InstallForms_RemovesR6RSMacroForms proves InstallForms drops the
// R6RS macro-transformer forms from the per-engine registry while leaving the R5RS
// §4.3 macro forms (syntax-rules, define-syntax, let-syntax, letrec-syntax) present.
func TestR5RSStrict_InstallForms_RemovesR6RSMacroForms(t *testing.T) {
	c := qt.New(t)
	fr := forms.DefaultRegistry().Clone()

	// Each R6RS macro form is a real baseline FormSpec — that is what makes forms-layer
	// removal effective for them (they are not import-style expander-only forms).
	for _, name := range r5rsRemovedMacroForms {
		c.Assert(fr.Lookup(name), qt.IsNotNil,
			qt.Commentf("%q must be a baseline form before removal", name))
	}

	err := R5RSStrict.InstallForms(fr)
	c.Assert(err, qt.IsNil)

	for _, name := range r5rsRemovedMacroForms {
		c.Assert(fr.Lookup(name), qt.IsNil,
			qt.Commentf("r5rs-strict must remove the R6RS macro form %q", name))
	}
	for _, kept := range []string{"syntax-rules", "define-syntax", "let-syntax", "letrec-syntax"} {
		c.Assert(fr.Lookup(kept), qt.IsNotNil,
			qt.Commentf("%q is R5RS §4.3 and must stay present", kept))
	}
}

// TestR5RSStrict_Engine_R6RSMacroFormsRejected is the end-to-end validation that the
// R6RS macro surface is gone: under r5rs-strict every R6RS macro-transformer form is
// an unbound identifier, while R5RS's own syntax-rules macros still expand, and a
// default engine still recognizes syntax-case as a form (not an unbound ref) — the
// difference is the dialect, not the build.
func TestR5RSStrict_Engine_R6RSMacroFormsRejected(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithDialect(R5RSStrict))
	qt.Assert(t, err, qt.IsNil)

	for _, name := range r5rsRemovedMacroForms {
		t.Run(name, func(t *testing.T) {
			// Form removed from the registry → the head resolves to an unbound global.
			_, err := eng.EvalMultiple(ctx, "("+name+")")
			qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
				qt.Commentf("%s must be an unbound identifier under r5rs-strict, got %v", name, err))
		})
	}

	// A R6RS transformer form is also rejected in transformer position (not just as a
	// bare call): define-syntax with a syntax-case transformer is an unsupported type.
	_, err = eng.EvalMultiple(ctx,
		"(define-syntax m (syntax-case x () (_ 1)))")
	qt.Assert(t, err, qt.IsNotNil,
		qt.Commentf("syntax-case transformer must be rejected under r5rs-strict"))

	// R5RS macros survive: define-syntax + syntax-rules still expand.
	got, err := eng.EvalMultiple(ctx,
		"(define-syntax swap (syntax-rules () ((_ a b) (list b a)))) (swap 1 2)")
	qt.Assert(t, err, qt.IsNil,
		qt.Commentf("syntax-rules/define-syntax are R5RS §4.3 and must still work, got %v", err))
	qt.Assert(t, got.SchemeString(), qt.Equals, "(2 1)")

	// A default engine still recognizes syntax-case as a form (it errors for being
	// malformed, NOT as an unbound reference).
	base, err := NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)
	_, baseErr := base.EvalMultiple(ctx, "(syntax-case)")
	qt.Assert(t, errors.Is(baseErr, werr.ErrNoSuchBinding), qt.IsFalse,
		qt.Commentf("default engine must treat syntax-case as a form, not an unbound ref (got %v)", baseErr))
}

// TestR5RSStrict_CaseLambdaRetained pins the documented Wile-implementation
// caveat: case-lambda is R7RS, but Wile's bootstrap stdlib is defined using it,
// so removing it would break engine construction. r5rs-strict therefore RETAINS
// case-lambda — strict about the R5RS syntax it can reach, not a certified R5RS.
func TestR5RSStrict_CaseLambdaRetained(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithDialect(R5RSStrict))
	c.Assert(err, qt.IsNil)

	got, err := eng.EvalMultiple(ctx, "((case-lambda ((x) x) ((x y) (+ x y))) 3 4)")
	c.Assert(err, qt.IsNil,
		qt.Commentf("case-lambda is retained (Wile bootstrap dependency), so it still works"))
	c.Assert(got.SchemeString(), qt.Equals, "7")
}
