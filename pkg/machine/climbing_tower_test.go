package machine_test

import (
	"context"
	"io"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/values"
)

// evalSchemeReturningError runs a multi-expression program like evalScheme but
// returns the terminating error instead of asserting nil — for tests that assert
// a construction is *rejected* (e.g. the hermetic phase boundary) rather than
// evaluated.
func evalSchemeReturningError(t *testing.T, code string) (values.Value, error) {
	t.Helper()
	env, err := bootstrap.NewNamespaceFrame(context.TODO())
	if err != nil {
		return nil, err
	}
	ctx := context.Background()
	p := parser.NewParser(env, true, strings.NewReader(code))
	last := values.Void
	for {
		stx, err := p.ReadSyntax(context.TODO())
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, err
		}
		expanded, err := compilation.NewExpanderTimeContinuation(context.Background(), env, machine.NewVMMacroEvaluator()).ExpandExpression(stx)
		if err != nil {
			return nil, err
		}
		tpl := machine.NewNativeTemplate(0, 0, false)
		cctx := compilation.NewCompileTimeCallContext(context.Background(), false)
		err = compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator()).CompileExpression(cctx, expanded)
		if err != nil {
			return nil, err
		}
		cont := machine.NewMachineContinuation(nil, tpl, env)
		mc := machine.NewMachineContext(ctx, cont)
		err = mc.Run()
		if err != nil {
			return nil, err
		}
		last = mc.GetValue()
	}
	return last, nil
}

// TestClimbingTower_CrossPhaseCollision is the genuine RED acceptance target
// (the plan's Task 1 NOTE: "rewrite it so the inner macro shadows a phase-1 name
// with a different phase-2 meaning — the case that genuinely requires phase
// separation").
//
// `M` is defined in top's transformer body (phase-1 context) meaning A, and
// re-defined in mid's transformer body (phase-2 context) meaning B. mid's
// transformer runs while expanding top's body. Under the pre-tower collapse both
// definitions land in the single global phase-1 frame, so mid's M (B) clobbers
// top's M (A) and (top) yields "B". A climbing tower keeps top's M at one phase
// and mid's M one phase higher, so (top) must yield "A".
//
// The declarative _TwoStorey/_ThreeStorey/_BeginForSyntaxClimbs cases below do
// NOT exercise the climb (they place inner macros in expansion OUTPUT = the same
// phase as their use, so absolute Expand() is already consistent); they are kept
// as level-0-identity regression guards and pass with and without the tower.
func TestClimbingTower_CrossPhaseCollision(t *testing.T) {
	const src = `
(define-syntax top
  (lambda (stx)
    (define-syntax M (syntax-rules () ((_) #'(quote A))))
    (define-syntax mid
      (lambda (s)
        (define-syntax M (syntax-rules () ((_) #'(quote B))))
        (M)))
    (mid)
    (M)))
(top)`
	got := evalScheme(t, src).SchemeString()
	if got != "A" {
		t.Fatalf("cross-phase collision: got %q want %q (mid's phase-2 M clobbered top's phase-1 M)", got, "A")
	}
}

// TestClimbingTower_TwoStorey: level-0-identity regression guard. A macro whose
// expansion output defines and uses an inner macro; the inner define-syntax and
// its use share the output phase, so this passes with and without the tower.
func TestClimbingTower_TwoStorey(t *testing.T) {
	const src = `
(define-syntax outer
  (syntax-rules ()
    ((_ x)
     (let ()
       (define-syntax inner (syntax-rules () ((_ y) (* y y))))
       (inner x)))))
(outer 6)`
	got := evalScheme(t, src).SchemeString()
	if got != "36" {
		t.Fatalf("two-storey climb: got %q want %q", got, "36")
	}
}

// TestClimbingTower_ThreeStorey: inner macro that itself defines and uses a
// third macro — the phase-3 frame must be created on demand.
func TestClimbingTower_ThreeStorey(t *testing.T) {
	const src = `
(define-syntax storey1
  (syntax-rules ()
    ((_ x)
     (let ()
       (define-syntax storey2
         (syntax-rules ()
           ((_ y)
            (let ()
              (define-syntax storey3 (syntax-rules () ((_ z) (+ z 1))))
              (storey3 y)))))
       (storey2 x)))))
(storey1 10)`
	got := evalScheme(t, src).SchemeString()
	if got != "11" {
		t.Fatalf("three-storey climb: got %q want %q", got, "11")
	}
}

// TestClimbingTower_CrossPhaseMutationIsHermetic is the design §8
// _PerPhaseStateIndependent boundary row, asserted rather than skipped. It is the
// executable resolution of design §10 Q4 ("silently share mutable compile-time
// state across a phase climb?").
//
// The answer is that the footgun is *unconstructible* under shipped Tier 1: the
// hermetic phase-frame reparent (createPhaseEnv → SealedBaseTarget, master
// fcbf034c) already severs the lexical cross-phase path, so a mutable binding
// defined in top's transformer body (phase 1) is simply NOT VISIBLE in mid's
// transformer body (phase 2). Observing it there is a loud compile-time
// "no such binding", not a silent share of diverging state.
//
// This means Q4 needs no ErrCrossPhaseMutation reject (option b): the hermetic
// error already delivers the loud failure option (b) wanted, at zero cost and
// with no mutation-reachability analysis — and with no false positive on the
// R7RS §4.3 carve-out (see TestClimbingTower_MarchHareMutatedIsCoherent). The
// boundary is enforced by rejection (nearer Racket's rejection-by-construction)
// rather than by a bespoke diagnostic. Tier 2 (per-phase instantiation) would
// *enable* independent per-phase mutable state; it is not a soundness fix,
// because there is no unsound program here to fix.
func TestClimbingTower_CrossPhaseMutationIsHermetic(t *testing.T) {
	const src = `
(define-syntax top
  (lambda (stx)
    (define counter 0)
    (set! counter (+ counter 1))
    (define-syntax mid
      (lambda (s)
        (set! counter (+ counter 10))
        (quasisyntax (quote (unsyntax counter)))))
    (mid)
    (quasisyntax (quote (unsyntax counter)))))
(top)`
	_, err := evalSchemeReturningError(t, src)
	if err == nil {
		t.Fatal("cross-phase mutation: expected a hermetic \"no such binding\" rejection at phase 2, got success (phase-1 mutable state leaked to phase 2)")
	}
	if !strings.Contains(err.Error(), "no such binding") {
		t.Fatalf("cross-phase mutation: expected a \"no such binding\" hermetic rejection, got: %v", err)
	}
}

// TestClimbingTower_MarchHareMutatedIsCoherent guards that the hermetic boundary
// above does NOT false-positive on the R7RS §4.3 referential-transparency
// carve-out (GetGlobalIndexAcrossPhases). jabberwocky/march-hare resolves a free
// template identifier to a SINGLE binding location; a set! of that location is
// observed coherently (one value, no per-phase divergence), so the mutated
// variant still yields the mutated value. This is the companion that proves the
// Q4 resolution costs no conformance.
func TestClimbingTower_MarchHareMutatedIsCoherent(t *testing.T) {
	const src = `
(define-syntax jab
  (syntax-rules ()
    ((_ h)
     (begin (define hare 42)
            (set! hare 99)
            (define-syntax h (syntax-rules () ((_) hare)))))))
(jab hatter)
(hatter)`
	got := evalScheme(t, src).SchemeString()
	if got != "99" {
		t.Fatalf("march-hare mutated: got %q want %q (R7RS §4.3 single-location resolution under set!)", got, "99")
	}
}

// TestClimbingTower_BeginForSyntaxClimbs: a begin-for-syntax nested inside a
// transformer body must place its bindings at phase 2, not phase 1.
func TestClimbingTower_BeginForSyntaxClimbs(t *testing.T) {
	const src = `
(define-syntax outer
  (syntax-rules ()
    ((_ x)
     (let ()
       (begin-for-syntax
         (define helper (lambda (n) (* n 2))))
       (define-syntax inner
         (syntax-rules () ((_ y) (quote climbed))))
       (inner x)))))
(outer 3)`
	got := evalScheme(t, src).SchemeString()
	if got != "climbed" {
		t.Fatalf("begin-for-syntax climb: got %q want %q", got, "climbed")
	}
}
