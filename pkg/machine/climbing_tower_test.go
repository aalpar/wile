package machine_test

import "testing"

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
