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
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/wile"
)

// The self-tail family decides self-identity by SPELLING. Its predicates run
// post-expansion, where a macro-introduced identifier and a user identifier of the
// same name differ only in scopes, so a name comparison conflates them.
//
// Both repros here fail OPEN — they arm the optimization where it is unsound, and
// both hang rather than misbehave quietly, so each is capped by a context deadline:
// a hang must FAIL, not wedge CI.
//
// Every case asserts the OpSelfTailCall site count as well as the value, and each
// is paired with a distinct-name control. The value alone is not a sufficient
// witness: both mechanisms in the fix can fail toward TOTAL DEOPT (0 sites
// everywhere), which every value assertion in this repo would still pass. The
// controls hold the shape fixed and vary only the collision, so a passing
// assertion cannot be explained by "named lets do not get OpSelfTailCall".
//
// selfTailSites (selftail_callee_stamp_test.go) recurses into literal
// sub-templates, which a named let requires: its loop lambda is a child template
// pushed as a literal.

// procSelfTailSites compiles src, then counts OpSelfTailCall in the named
// procedure's template and every template reachable from its literals.
func procSelfTailSites(t *testing.T, src, name string, opts ...wile.EngineOption) int {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, append([]wile.EngineOption{wile.WithProfile(wile.KitchenSink)}, opts...)...)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	_, err = eng.EvalMultiple(ctx, src)
	qt.Assert(t, err, qt.IsNil)

	v, ok := eng.Get(name)
	qt.Assert(t, ok, qt.IsTrue)
	closure, ok := v.Internal().(*machine.MachineClosure)
	qt.Assert(t, ok, qt.IsTrue)
	return selfTailSites(closure.Template())
}

// runCapped evaluates src under a deadline and returns the last value and error.
// The VM checks ctx.Done() every 1024 ops, so an armed-but-unsound self-tail loop
// surfaces as a deadline error instead of hanging the package.
func runCapped(t *testing.T, src string, opts ...wile.EngineOption) (string, error) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	eng, err := wile.NewEngine(ctx, append([]wile.EngineOption{wile.WithProfile(wile.KitchenSink)}, opts...)...)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	v, err := eng.EvalMultiple(ctx, src)
	if err != nil {
		return "", err
	}
	return v.SchemeString(), nil
}

// F1a: a macro whose template opens (let loop …) and calls the pattern variable
// `f` in tail position. Instantiated with the USER's top-level `loop`, the escape
// (f i) is spelled `loop` and so passes the emit gate's Sym.Key comparison, which
// rewrites it to OpSelfTailCall — machine_context.go's jump-to-pc=0 instead of a
// call to a different procedure.
const f1aCollide = `
	(define (loop x) (* x 100))
	(define-syntax mk
	  (syntax-rules ()
	    ((_ f) (let loop ((i 0))
	             (if (= i 3) (f i) (loop (+ i 1)))))))
	(define (h) (mk loop))
`

// F1a-control: identical shape, different spelling for the escaped binding, so the
// pre-fix name comparison already refused it.
const f1aControl = `
	(define (other x) (* x 100))
	(define-syntax mk
	  (syntax-rules ()
	    ((_ f) (let loop ((i 0))
	             (if (= i 3) (f i) (loop (+ i 1)))))))
	(define (h) (mk other))
`

// TestSelfTailEmit_EscapeToSameSpelledBinding pins repro (a): only the genuine self
// call may be rewritten. The template's own (loop (+ i 1)) is one site; the escape
// (f i) is a call to a DIFFERENT binding that happens to share the spelling and
// must compile as an ordinary tail call.
func TestSelfTailEmit_EscapeToSameSpelledBinding(t *testing.T) {
	t.Run("control", func(t *testing.T) {
		sites := procSelfTailSites(t, f1aControl, "h")
		qt.Assert(t, sites, qt.Equals, 1)
		v, err := runCapped(t, f1aControl+"\n(h)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v, qt.Equals, "300")
	})

	t.Run("collision", func(t *testing.T) {
		sites := procSelfTailSites(t, f1aCollide, "h")
		qt.Assert(t, sites, qt.Equals, 1,
			qt.Commentf("the template-introduced `loop` letrec binder is the only self; "+
				"the escaped (f i) resolves to the top-level `loop` global and must not "+
				"be emitted as OpSelfTailCall. 2 sites means the emit gate compared "+
				"spellings (compile_call.go) instead of resolved bindings"))
		v, err := runCapped(t, f1aCollide+"\n(h)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v, qt.Equals, "300",
			qt.Commentf("(h) must reach the user's (define (loop x) (* x 100)) with i=3. "+
				"A deadline error here is the mis-emitted jump to pc=0 looping forever"))
	})
}

// F1b: the macro introduces its own (let ((loop 0)) …) around a set! of the
// pattern variable `v`. Post-expansion the shadow set holds a `loop` entry, so a
// name-keyed lookup reports the real named-let `loop` as shadowed and the set! of
// it goes unreported — arming self-tail on a mutable self.
const f1bCollide = `
	(define-syntax hide
	  (syntax-rules ()
	    ((_ v e) (let ((loop 0)) (set! v e)))))
	(define (h)
	  (let loop ((i 0))
	    (if (= i 2)
	        (begin (hide loop 42) (loop 5))
	        (loop (+ i 1)))))
`

// F1b-control: the same set! written directly, where no introduced binder can hide
// it.
const f1bControl = `
	(define (h)
	  (let loop ((i 0))
	    (if (= i 2)
	        (begin (set! loop 42) (loop 5))
	        (loop (+ i 1)))))
`

// TestSelfTailArming_ShadowSetHidesMacroIntroducedSetBang pins repro (b): a set!
// of the loop must deny the arming whether it is written directly or arrives
// through a macro. Both cases raise when the loop reaches (loop 5) with `loop`
// holding 42; the collision case must not be armed into a jump instead.
func TestSelfTailArming_ShadowSetHidesMacroIntroducedSetBang(t *testing.T) {
	t.Run("control", func(t *testing.T) {
		sites := procSelfTailSites(t, f1bControl, "h")
		qt.Assert(t, sites, qt.Equals, 0)
		_, err := runCapped(t, f1bControl+"\n(h)")
		qt.Assert(t, err, qt.IsNotNil,
			qt.Commentf("applying 42 must raise"))
	})

	t.Run("collision", func(t *testing.T) {
		sites := procSelfTailSites(t, f1bCollide, "h")
		qt.Assert(t, sites, qt.Equals, 0,
			qt.Commentf("the `hide`-introduced (let ((loop 0)) …) binder carries the intro "+
				"scope, which is NOT a subset of the use-site set! target's scopes, so it "+
				"must not shadow the real loop. 2 sites means nameSet answered by name"))
		_, err := runCapped(t, f1bCollide+"\n(h)")
		qt.Assert(t, err, qt.IsNotNil,
			qt.Commentf("must raise exactly as the control does; a deadline error is the "+
				"armed jump ignoring the set!"))
	})
}

// TestSelfTailArmingCountRatchet is the release gate for this branch, and it is
// deliberately a table of hard-coded numbers rather than value assertions.
//
// BOTH mechanisms in the fix fail toward TOTAL DEOPT: a subset query that never
// matches, or an entry guard whose self never resolves, silently arms zero sites
// everywhere. Every value assertion in this repository would still pass, and the
// only visible symptom would be a wall-clock regression nobody attributes to this
// change. The counts below were read off the tree at the branch point.
//
// or_lowering_test.go:145,150 assert selfTailSites(...) == 1 on the or-shaped-let
// lowering and are a second witness in the same blast radius; they are left alone.
func TestSelfTailArmingCountRatchet(t *testing.T) {
	const primes = `
		(begin
		  (define (is-prime? n)
		    (if (< n 2)
		        #f
		        (let loop ((i 2))
		          (cond ((> (* i i) n) #t)
		                ((= (modulo n i) 0) #f)
		                (else (loop (+ i 1)))))))
		  (define (primes-upto n)
		    (let loop ((i 2) (result '()))
		      (if (> i n)
		          (reverse result)
		          (loop (+ i 1)
		                (if (is-prime? i)
		                    (cons i result)
		                    result))))))
	`

	const plainLoop = `
		(define (sum-to n)
		  (let loop ((i 0) (acc 0))
		    (if (> i n)
		        acc
		        (loop (+ i 1) (+ acc i)))))
	`

	// tak-shaped: a top-level define whose own tail call is to itself. Its arming
	// needs the self binding Stable, which only WithImmutableTopLevel provides —
	// which is also why frameReuseForDefine armed 0 sites in the finding report's
	// probe (Phase B4).
	const tak = `
		(define (tak x y z)
		  (if (not (< y x))
		      z
		      (tak (- x 1) y z)))
	`

	tests := []struct {
		name  string
		src   string
		proc  string
		opts  []wile.EngineOption
		sites int
	}{
		{name: "primes/is-prime?", src: primes, proc: "is-prime?", sites: 1},
		{name: "primes/primes-upto", src: primes, proc: "primes-upto", sites: 1},
		{name: "plain named let", src: plainLoop, proc: "sum-to", sites: 1},
		// The default already IS immutable top-level (the layered-environment carve),
		// so the explicit option is a no-op here and the discriminator is the opposite
		// one: WithMutableTopLevel denies the define arm its IsStable() and drops the
		// count to 0. Both rows are kept — one pins that a top-level define arms at
		// all, the other that stability is what gates it.
		{name: "tak default top-level", src: tak, proc: "tak", sites: 1},
		{
			name:  "tak immutable top-level",
			src:   tak,
			proc:  "tak",
			opts:  []wile.EngineOption{wile.WithImmutableTopLevel()},
			sites: 1,
		},
		{
			name:  "tak mutable top-level",
			src:   tak,
			proc:  "tak",
			opts:  []wile.EngineOption{wile.WithMutableTopLevel()},
			sites: 0,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			sites := procSelfTailSites(t, tt.src, tt.proc, tt.opts...)
			qt.Assert(t, sites, qt.Equals, tt.sites,
				qt.Commentf("arming-count ratchet: a DROP here is the total-deopt failure "+
					"mode this branch's two mechanisms have, and no value assertion in the "+
					"repo would catch it"))
		})
	}
}

// F1a-define: repro (a)'s shape on the OTHER producer. frameReuseForDefine arms a
// top-level define, and the finding report's probe armed 0 sites there because
// frameReuseSelfTail also demands binding.IsStable() — which the report read as a
// mutable top level. The default is immutable, so the arm does fire, and the
// template's escape to a same-spelled user global is the same hazard.
//
// The template exports its own hygienic `loop` through `export` so the test can
// invoke it; there is no other way to reach a macro-introduced top-level binder.
const f1aDefineCollide = `
	(define (loop x) (* x 100))
	(define-syntax mkdef
	  (syntax-rules ()
	    ((_ f export)
	     (begin (define (loop i) (if (= i 3) (f i) (loop (+ i 1))))
	            (define export loop)))))
	(mkdef loop entry)
`

const f1aDefineControl = `
	(define (other x) (* x 100))
	(define-syntax mkdef
	  (syntax-rules ()
	    ((_ f export)
	     (begin (define (loop i) (if (= i 3) (f i) (loop (+ i 1))))
	            (define export loop)))))
	(mkdef other entry)
`

// TestSelfTailEmit_DefineArmEscape closes the `define`-arm half of repro (a).
// Measured on the branch: pre-fix the collision emitted 2 sites and hung, exactly
// as the named-let arm did, so frameReuseForDefine was a second LIVE repro rather
// than the "exposed but unproven" the report recorded — the missing conjunct was
// that immutable top level is the default, not that the arm was unreachable.
func TestSelfTailEmit_DefineArmEscape(t *testing.T) {
	t.Run("control", func(t *testing.T) {
		sites := procSelfTailSites(t, f1aDefineControl, "entry")
		qt.Assert(t, sites, qt.Equals, 1)
		v, err := runCapped(t, f1aDefineControl+"\n(entry 0)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v, qt.Equals, "300")
	})

	t.Run("collision", func(t *testing.T) {
		sites := procSelfTailSites(t, f1aDefineCollide, "entry")
		qt.Assert(t, sites, qt.Equals, 1,
			qt.Commentf("the define arm resolves its self through the same emit gate; the "+
				"escaped (f i) is the user's top-level `loop` and must not be rewritten"))
		v, err := runCapped(t, f1aDefineCollide+"\n(entry 0)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, v, qt.Equals, "300")
	})
}
