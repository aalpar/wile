;;; algebra-category-test.scm — Category tests

(import (scheme base)
        (chibi test)
        (wile algebra monoid)
        (wile algebra category))

(test-begin "categories")

;; ─── construction ────────────────────────────

(test-group "construction"
  (test #t (category? (procedure-category)))
  (test #f (category? 42))
  (test #f (category? (make-monoid + 0))))

;; ─── procedure-category composition ──────────

(define C (procedure-category))

(test-group "procedure-category-compose"
  ;; f ∘ g means apply g first, then f
  (let* ((f (lambda (x) (* x 2)))
         (g (lambda (x) (+ x 1)))
         (fg (category-compose C f g)))
    ;; (f ∘ g)(3) = f(g(3)) = f(4) = 8
    (test 8 (fg 3))
    ;; (g ∘ f)(3) = g(f(3)) = g(6) = 7
    (test 7 ((category-compose C g f) 3))))

;; ─── procedure-category identity ─────────────

(test-group "procedure-category-identity"
  (let ((id (category-identity C 'any)))
    (test 42 (id 42))
    (test "hello" (id "hello"))
    (test '(1 2 3) (id '(1 2 3)))))

;; ─── Alist-based finite category on {0,1,2} ─
;;;
;;; Morphisms are alists representing functions on {0,1,2}.
;;; Composition applies alist lookup, and equal? works on alists.

(define domain '(0 1 2))

(define (alist-lookup alist key)
  (cdr (assv key alist)))

(define (alist-compose f g)
  ;; f ∘ g: for each x in domain, map x to f(g(x))
  (map (lambda (x)
         (cons x (alist-lookup f (alist-lookup g x))))
       domain))

(define (alist-identity obj)
  ;; Identity function on the domain
  (map (lambda (x) (cons x x)) domain))

(define alist-cat
  (make-category alist-compose alist-identity equal?))

;; Some concrete morphisms on {0,1,2}
(define rot   '((0 . 1) (1 . 2) (2 . 0)))  ; rotation
(define rot2  '((0 . 2) (1 . 0) (2 . 1)))  ; rotation squared
(define id012 '((0 . 0) (1 . 1) (2 . 2)))  ; identity
(define swap01 '((0 . 1) (1 . 0) (2 . 2))) ; swap 0 and 1
(define const0 '((0 . 0) (1 . 0) (2 . 0))) ; constant 0

;; ─── associativity ───────────────────────────

(test-group "associativity"
  ;; (f ∘ g) ∘ h = f ∘ (g ∘ h) for several triples
  (let ((triples (list (list rot rot2 swap01)
                       (list swap01 rot const0)
                       (list rot swap01 rot2))))
    (for-each
      (lambda (triple)
        (let ((f (car triple))
              (g (cadr triple))
              (h (caddr triple)))
          (test (alist-compose (alist-compose f g) h)
                (alist-compose f (alist-compose g h)))))
      triples)))

;; ─── identity laws ───────────────────────────

(test-group "identity-laws"
  ;; id ∘ f = f, f ∘ id = f
  (for-each
    (lambda (f)
      (test f (category-compose alist-cat id012 f))
      (test f (category-compose alist-cat f id012)))
    (list rot rot2 swap01 const0 id012))
  ;; identity-fn returns the identity alist
  (test id012 (category-identity alist-cat 'domain)))

;; ─── endomorphism-monoid ─────────────────────

(test-group "endomorphism-monoid"
  (let ((M (category->endomorphism-monoid alist-cat 'domain)))
    (test #t (monoid? M))
    ;; Monoid identity is the category identity
    (test id012 (monoid-identity M))
    ;; Monoid op is composition
    (test (alist-compose rot rot2) (monoid-op M rot rot2))
    ;; rot ∘ rot2 = id (rotation by 3 = identity)
    (test id012 (monoid-op M rot rot2))
    ;; monoid-fold chains operations
    (test (alist-compose rot (alist-compose rot rot))
          (monoid-fold M (list rot rot rot)))
    ;; monoid-power: rot^3 = id
    (test id012 (monoid-power M rot 3))))

;; ─── validate-category ──────────────────────

(test-group "validate-category"
  ;; Valid: alist-based category
  (let ((triples (list (list rot rot2 swap01)
                       (list swap01 rot const0)
                       (list rot swap01 rot2)
                       (list const0 rot rot2)))
        (identities (list (list 'domain rot)
                          (list 'domain rot2)
                          (list 'domain swap01)
                          (list 'domain const0))))
    (test #t (validate-category alist-cat triples identities)))
  ;; Invalid: broken identity (returns const0 instead of id)
  (let ((bad-cat (make-category
                   alist-compose
                   (lambda (obj) const0)  ; wrong identity
                   equal?)))
    ;; rot composed with const0 is not rot
    (test #f (eq? #t (validate-category
                       bad-cat
                       '()
                       (list (list 'domain rot)))))))

;; ─── with-category macro ─────────────────────

(test-group "with-category"
  (with-category alist-cat (compose identity equiv?)
    ;; compose works
    (test id012 (compose rot rot2))
    ;; identity works
    (test id012 (identity 'domain))
    ;; equiv? works
    (test #t (equiv? (compose rot rot2) id012))
    (test #f (equiv? rot swap01))))

(test-end)
(test-exit)
