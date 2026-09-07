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
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// TestP1_BootstrapMacrosVisibleFromNestedTransformer (F10): fails on master with
// "no such binding and".
func TestP1_BootstrapMacrosVisibleFromNestedTransformer(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(define-syntax m
  (lambda (stx)
    (datum->syntax #f
      (let-syntax ((k (lambda (s) (datum->syntax #f (and 1 2)))))
        (k)))))
(m)`)
	c.Assert(got, qt.Equals, "2")
}

// TestP1_Q8NestedTransformerUsesSyntaxCase is design Q8's own pin: a let-syntax
// inside a transformer body whose transformer uses syntax-case, under the Scheme
// layer, one rung above TestP1_BootstrapMacrosVisibleFromNestedTransformer: k is
// a transformer written inside another transformer's body, so the let-syntax RHS
// in ITS body evaluates at phase 3 (design §3.0), and the syntax-case there binds
// the pattern variable v the §3.0 pin asks for. Green through the Go compiler until Task 8
// defines syntax-case in Scheme (F1: an excluded row does not stop the
// name-dispatched compiler), so it discriminates arm 2b only from Task 8 on.
func TestP1_Q8NestedTransformerUsesSyntaxCase(t *testing.T) {
	c := qt.New(t)
	got := evalSyntaxForms(t, `(define-syntax m
  (lambda (stx)
    (datum->syntax #f
      (let-syntax ((k (lambda (s)
                        (datum->syntax #f
                          (let-syntax ((j (lambda (w) (syntax-case w () ((_ v) #'(+ 3 v))))))
                            (j 4))))))
        (k)))))
(m)`, wile.WithSchemeSyntaxForms())
	c.Assert(got, qt.Equals, "7")
}

const syntaxCaseProbe = `(define-syntax m (lambda (stx) (syntax-case stx () ((_ x) #'x))))
(m 5)`

// TestP1_SwitchSelectsTheLayer: the Go syntax-case compiler runs under
// WithGoSyntaxForms and is unreachable under WithSchemeSyntaxForms (design §7
// P1's unreachability pin, as a process-global counter delta).
func TestP1_SwitchSelectsTheLayer(t *testing.T) {
	c := qt.New(t)
	before := compilation.GoSyntaxFormCompiles()
	c.Assert(evalSyntaxForms(t, syntaxCaseProbe, wile.WithGoSyntaxForms()), qt.Equals, "5")
	c.Assert(compilation.GoSyntaxFormCompiles() > before, qt.IsTrue)

	before = compilation.GoSyntaxFormCompiles()
	c.Assert(evalSyntaxForms(t, syntaxCaseProbe, wile.WithSchemeSyntaxForms()), qt.Equals, "5")
	c.Assert(compilation.GoSyntaxFormCompiles(), qt.Equals, before)
}

// TestP1_EnvironmentVariableSetsTheDefault (F9): WILE_SYNTAX_FORMS is read per
// NewEngine, so the whole suite runs both ways from the environment.
func TestP1_EnvironmentVariableSetsTheDefault(t *testing.T) {
	c := qt.New(t)
	t.Setenv("WILE_SYNTAX_FORMS", "scheme")
	before := compilation.GoSyntaxFormCompiles()
	c.Assert(evalSyntaxForms(t, syntaxCaseProbe), qt.Equals, "5")
	c.Assert(compilation.GoSyntaxFormCompiles(), qt.Equals, before)

	t.Setenv("WILE_SYNTAX_FORMS", "go")
	before = compilation.GoSyntaxFormCompiles()
	c.Assert(evalSyntaxForms(t, syntaxCaseProbe), qt.Equals, "5")
	c.Assert(compilation.GoSyntaxFormCompiles() > before, qt.IsTrue)
}

// scLibFS: a library whose macro uses syntax-case. Library environments bootstrap
// from the namespace's registry, so the switch must reach them (F9).
var scLibFS = fstest.MapFS{
	"sclib.sld": &fstest.MapFile{
		Data: []byte(`(define-library (sclib)
  (import (scheme base))
  (export m)
  (begin (define-syntax m (lambda (stx) (syntax-case stx () ((_ x) #'(list x x)))))))`),
	},
}

func TestP1_SwitchReachesLibraryEnvironments(t *testing.T) {
	c := qt.New(t)
	before := compilation.GoSyntaxFormCompiles()
	got := evalSyntaxForms(t, `(import (sclib)) (m 5)`,
		wile.WithSchemeSyntaxForms(), wile.WithSourceFS(stdlib.FS), wile.WithSourceFS(scLibFS), wile.WithLibraryPaths())
	c.Assert(got, qt.Equals, "(5 5)")
	c.Assert(compilation.GoSyntaxFormCompiles(), qt.Equals, before)
}
