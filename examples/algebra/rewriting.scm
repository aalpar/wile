;;; rewriting.scm - Equational rewriting with axiom-driven normalization
;;;
;;; Demonstrates: term protocols, identity/commutativity/absorbing/idempotence/
;;;               involution/absorption/associativity axioms, composed normalizers
;;;
;;; Usage: ./dist/wile --file examples/algebra/rewriting.scm

(import (scheme base) (scheme write) (wile algebra))

;; ============================================================
;; 1. Term protocol
;; ============================================================
;;
;; A term protocol tells the rewriting engine how to inspect terms.
;; sexp-term-protocol builds one for S-expression terms where
;; compound terms are lists (op arg ...) and atoms are symbols.
;;
;; The compare function orders atoms for commutativity normalization:
;; it returns #t when a should sort before b. We sort symbols
;; alphabetically, and put atoms before compounds.

(display "=== Term Protocol ===\n\n")

(define proto
  (sexp-term-protocol
    (lambda (a b)
      (cond
        ((and (symbol? a) (symbol? b))
         (string<? (symbol->string a) (symbol->string b)))
        ((symbol? a) #t)    ; atoms before compounds
        (else #f)))))

;; Inspecting terms through the protocol
(display "Is '(+ a b) compound? ")
(display (term-compound? proto '(+ a b)))
(newline)

(display "Is 'x compound? ")
(display (term-compound? proto 'x))
(newline)

(display "Operator of '(+ a b): ")
(display (term-get-operator proto '(+ a b)))
(newline)

(display "Operands of '(+ a b): ")
(display (term-get-operands proto '(+ a b)))
(newline)
(newline)

;; ============================================================
;; 2. Identity axiom
;; ============================================================
;;
;; An identity axiom says: (op x e) = x when e is the identity.
;; The predicate tests whether a value is the identity element.
;; Fires for either operand position: (op x e) or (op e x).

(display "=== Identity Axiom ===\n\n")

(define id-axiom (make-identity-axiom '+ (lambda (x) (eq? x 'zero))))
(define id-normalize (make-normalizer (list id-axiom) proto))

(display "(+ x zero) => ")
(display (id-normalize '(+ x zero)))
(newline)

(display "(+ zero x) => ")
(display (id-normalize '(+ zero x)))
(newline)

(display "(+ x y)    => ")
(display (id-normalize '(+ x y)))
(display "  (no match)")
(newline)
(newline)

;; ============================================================
;; 3. Commutativity axiom
;; ============================================================
;;
;; Commutativity normalizes operand order using the protocol's
;; compare function. If the second operand should sort before the
;; first, the axiom swaps them. Already-ordered terms don't match.

(display "=== Commutativity Axiom ===\n\n")

(define comm-axiom (make-commutativity-axiom '+))
(define comm-normalize (make-normalizer (list comm-axiom) proto))

(display "(+ b a) => ")
(display (comm-normalize '(+ b a)))
(display "  (swapped: a < b)")
(newline)

(display "(+ a b) => ")
(display (comm-normalize '(+ a b)))
(display "  (already ordered)")
(newline)
(newline)

;; ============================================================
;; 4. Absorbing axiom
;; ============================================================
;;
;; An absorbing element swallows the other operand:
;; (op x absorber) = absorber, regardless of x.
;; Classic example: zero absorbs multiplication.

(display "=== Absorbing Axiom ===\n\n")

(define abs-axiom (make-absorbing-axiom '* (lambda (x) (eq? x 'zero))))
(define abs-normalize (make-normalizer (list abs-axiom) proto))

(display "(* x zero) => ")
(display (abs-normalize '(* x zero)))
(newline)

(display "(* zero x) => ")
(display (abs-normalize '(* zero x)))
(newline)
(newline)

;; ============================================================
;; 5. Idempotence axiom
;; ============================================================
;;
;; An idempotent operation satisfies (op x x) = x.
;; Classic example: logical AND. Only fires when both operands
;; are equal.

(display "=== Idempotence Axiom ===\n\n")

(define idem-axiom (make-idempotence-axiom 'and))
(define idem-normalize (make-normalizer (list idem-axiom) proto))

(display "(and x x) => ")
(display (idem-normalize '(and x x)))
(newline)

(display "(and x y) => ")
(display (idem-normalize '(and x y)))
(display "  (no match)")
(newline)
(newline)

;; ============================================================
;; 6. Involution axiom
;; ============================================================
;;
;; An involution satisfies (op (op x)) = x. Applying the
;; operation twice cancels out. Classic example: double negation.

(display "=== Involution Axiom ===\n\n")

(define inv-axiom (make-involution-axiom 'neg))
(define inv-normalize (make-normalizer (list inv-axiom) proto))

(display "(neg (neg x)) => ")
(display (inv-normalize '(neg (neg x))))
(newline)

(display "(neg x)       => ")
(display (inv-normalize '(neg x)))
(display "  (no match)")
(newline)
(newline)

;; ============================================================
;; 7. Absorption axiom
;; ============================================================
;;
;; Absorption says: (outer x (inner x y)) = x. The variable x
;; appears in both the outer and inner terms, and the inner term
;; is absorbed. Works in either operand position.

(display "=== Absorption Axiom ===\n\n")

(define absorb-axiom (make-absorption-axiom 'and 'or))
(define absorb-normalize (make-normalizer (list absorb-axiom) proto))

(display "(and x (or x y)) => ")
(display (absorb-normalize '(and x (or x y))))
(newline)

(display "(and (or x y) x) => ")
(display (absorb-normalize '(and (or x y) x)))
(newline)

(display "(and x y)         => ")
(display (absorb-normalize '(and x y)))
(display "  (no match)")
(newline)
(newline)

;; ============================================================
;; 8. Associativity axiom
;; ============================================================
;;
;; Associativity rewrites left-associated terms to right-associated:
;;   (op (op a b) c) => (op a (op b c))
;; This is directional — already right-associated terms don't match.

(display "=== Associativity Axiom ===\n\n")

(define assoc-axiom (make-associativity-axiom '+))
(define assoc-normalize (make-normalizer (list assoc-axiom) proto))

(display "(+ (+ a b) c) => ")
(display (assoc-normalize '(+ (+ a b) c)))
(display "  (left to right)")
(newline)

(display "(+ a (+ b c)) => ")
(display (assoc-normalize '(+ a (+ b c))))
(display "  (already right-associated)")
(newline)
(newline)

;; ============================================================
;; 9. Composed normalizer
;; ============================================================
;;
;; A normalizer built from multiple axioms tries each in order.
;; The first axiom whose rule matches wins. Here identity fires
;; before commutativity when a zero operand is present.

(display "=== Composed Normalizer ===\n\n")

(define zero? (lambda (x) (eq? x 'zero)))

(define combined-normalize
  (make-normalizer
    (list (make-identity-axiom '+ zero?)
          (make-commutativity-axiom '+)
          (make-absorbing-axiom '* zero?))
    proto))

;; Identity fires: zero operand present
(display "(+ x zero) => ")
(display (combined-normalize '(+ x zero)))
(display "  (identity fires)")
(newline)

;; Absorbing fires: different operator
(display "(* y zero) => ")
(display (combined-normalize '(* y zero)))
(display "  (absorbing fires)")
(newline)

;; Commutativity fires: no zero, wrong order
(display "(+ y a)    => ")
(display (combined-normalize '(+ y a)))
(display "  (commutativity fires)")
(newline)

;; Nothing fires
(display "(+ a b)    => ")
(display (combined-normalize '(+ a b)))
(display "  (no match)")
(newline)
(newline)

;; ============================================================
;; Summary
;; ============================================================

(display "=== What We Covered ===\n")
(display "  sexp-term-protocol          Build a term protocol for S-expressions\n")
(display "  term-compound?              Test if a term is compound\n")
(display "  term-get-operator           Extract operator from compound term\n")
(display "  term-get-operands           Extract operands from compound term\n")
(display "  make-identity-axiom         (op x e) = x when e is identity\n")
(display "  make-commutativity-axiom    (op b a) = (op a b) when a < b\n")
(display "  make-absorbing-axiom        (op x z) = z when z absorbs\n")
(display "  make-idempotence-axiom      (op x x) = x\n")
(display "  make-involution-axiom       (op (op x)) = x\n")
(display "  make-absorption-axiom       (outer x (inner x y)) = x\n")
(display "  make-associativity-axiom    (op (op a b) c) = (op a (op b c))\n")
(display "  make-normalizer             Compose axioms into a single normalizer\n")
(newline)
(display "Next: examples/algebra/symbolic.scm — theories, recursive normalization, and tracing.\n")
