;;; algebra-rewrite-test.scm — Rewrite library tests

(import (scheme base)
        (chibi test)
        (wile algebra rewrite))

(test-begin "rewriting")

;; ─── Shared protocol ────────────────────────

(define proto
  (make-term-protocol pair? car cdr
    (lambda (term new-args)
      (cons (car term) new-args))
    (lambda (a b)
      (string<? (symbol->string a) (symbol->string b)))))

(define (zero? x)
  (eq? x 'zero))

;; ─── Term protocol ──────────────────────────

(test-group "term-protocol"
  (test #t (term-protocol? proto))
  (test #f (term-protocol? 42))
  (test #t (term-compound? proto '(+ a b)))
  (test #f (term-compound? proto 'x))
  (test '+ (term-get-operator proto '(+ a b)))
  (test '(a b) (term-get-operands proto '(+ a b)))
  (test '(+ c d) (term-make-term proto '(+ a b) '(c d)))
  (test #t (term-compare proto 'a 'b))
  (test #f (term-compare proto 'b 'a)))

;; ─── Axiom predicates ───────────────────────

(test-group "axiom-predicates"
  (test #t (axiom? (make-identity-axiom '+ zero?)))
  (test #t (axiom? (make-commutativity-axiom '+)))
  (test #t (axiom? (make-absorbing-axiom '* zero?)))
  (test #t (axiom? (make-idempotence-axiom 'and)))
  (test #t (axiom? (make-involution-axiom 'neg)))
  (test #t (axiom? (make-absorption-axiom 'and 'or)))
  (test #t (axiom? (make-associativity-axiom '+)))
  (test #f (axiom? 42))
  (test #f (axiom? "not an axiom")))

(test-group "directional-axiom"
  (test #t (directional-axiom? (make-associativity-axiom '+)))
  (test #f (directional-axiom? (make-identity-axiom '+ zero?)))
  (test #f (directional-axiom? (make-commutativity-axiom '+))))

;; ─── Identity ───────────────────────────────

(test-group "identity"
  (let ((norm (make-normalizer (list (make-identity-axiom '+ zero?)) proto)))
    ;; Right identity: (+ x zero) → x
    (test 'x (norm '(+ x zero)))
    ;; Left identity: (+ zero x) → x
    (test 'x (norm '(+ zero x)))
    ;; No match: neither operand is identity
    (test #f (norm '(+ x y)))
    ;; No match: wrong operator
    (test #f (norm '(* x zero)))))

;; ─── Commutativity ──────────────────────────

(test-group "commutativity"
  (let ((norm (make-normalizer (list (make-commutativity-axiom '+)) proto)))
    ;; b before a alphabetically → swap
    (test '(+ a b) (norm '(+ b a)))
    ;; Already ordered → no match
    (test #f (norm '(+ a b)))))

;; ─── Absorbing ──────────────────────────────

(test-group "absorbing"
  (let ((norm (make-normalizer (list (make-absorbing-axiom '* zero?)) proto)))
    ;; Right absorbing: (* x zero) → zero
    (test 'zero (norm '(* x zero)))
    ;; Left absorbing: (* zero x) → zero
    (test 'zero (norm '(* zero x)))
    ;; No match
    (test #f (norm '(* x y)))))

;; ─── Idempotence ────────────────────────────

(test-group "idempotence"
  (let ((norm (make-normalizer (list (make-idempotence-axiom 'and)) proto)))
    ;; (and x x) → x
    (test 'x (norm '(and x x)))
    ;; No match: different operands
    (test #f (norm '(and x y)))))

;; ─── Involution ─────────────────────────────

(test-group "involution"
  (let ((norm (make-normalizer (list (make-involution-axiom 'neg)) proto)))
    ;; (neg (neg x)) → x
    (test 'x (norm '(neg (neg x))))
    ;; Single negation: no match
    (test #f (norm '(neg x)))))

;; ─── Absorption ─────────────────────────────

(test-group "absorption"
  (let ((norm (make-normalizer (list (make-absorption-axiom 'and 'or)) proto)))
    ;; (and x (or x y)) → x  [shared first in inner]
    (test 'x (norm '(and x (or x y))))
    ;; (and x (or y x)) → x  [shared second in inner]
    (test 'x (norm '(and x (or y x))))
    ;; (and (or x y) x) → x  [inner first, shared first]
    (test 'x (norm '(and (or x y) x)))
    ;; (and (or y x) x) → x  [inner first, shared second]
    (test 'x (norm '(and (or y x) x)))
    ;; No match: no shared element pattern
    (test #f (norm '(and x y)))))

;; ─── Associativity ──────────────────────────

(test-group "associativity"
  (let ((norm (make-normalizer (list (make-associativity-axiom '+)) proto)))
    ;; Left-associated → right-associated
    (test '(+ a (+ b c)) (norm '(+ (+ a b) c)))
    ;; Already right-associated → no match
    (test #f (norm '(+ a (+ b c))))))

;; ─── Composed normalizer ────────────────────

(test-group "composed"
  (let ((norm (make-normalizer
                (list (make-identity-axiom '+ zero?)
                      (make-commutativity-axiom '+))
                proto)))
    ;; Identity fires first
    (test 'x (norm '(+ x zero)))
    ;; Commutativity fires when identity doesn't
    (test '(+ a b) (norm '(+ b a)))))

;; ─── No-match sentinel ─────────────────────

(test-group "no-match-sentinel"
  (test #t (no-match? *no-match*))
  (test #f (no-match? #f))
  (test #f (no-match? '()))
  (test #f (no-match? 42)))

;; ─── axiom->rules ───────────────────────────

(test-group "axiom->rules"
  ;; Identity produces 2 rules (left + right)
  (test 2 (length (axiom->rules (make-identity-axiom '+ zero?) proto)))
  ;; Commutativity produces 1 rule
  (test 1 (length (axiom->rules (make-commutativity-axiom '+) proto)))
  ;; Absorption produces 4 rules
  (test 4 (length (axiom->rules (make-absorption-axiom 'and 'or) proto)))
  ;; Associativity produces 1 rule
  (test 1 (length (axiom->rules (make-associativity-axiom '+) proto))))

(test-end)
(test-exit)
