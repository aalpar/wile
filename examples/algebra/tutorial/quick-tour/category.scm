;; quick-tour: (wile algebra category)
;;
;; A category has objects, morphisms, an associative composition (with
;; matching domains/codomains), and identity morphisms. The library
;; abstracts just the two operations a generic categorical construction
;; needs: compose and identity. Pick it up when you want to write code
;; polymorphic over "things that compose."

(import (scheme base) (wile algebra category) (wile algebra monoid))
(include "../lib/check.scm")

;; -- The category of Scheme procedures ------------------------------

(define Proc (procedure-category))
(check-true (category? Proc)                          "procedure-category is a category")

;; Composition: (f o g) means apply g first, then f (math convention).
(define f (lambda (x) (* x 2)))
(define g (lambda (x) (+ x 1)))
(define fg (category-compose Proc f g))

(check= (fg 3)  8    "(f o g)(3) = f(g(3)) = f(4) = 8")

;; Identity morphism.
(define id (category-identity Proc 'any-object))
(check= (id 42)  42  "identity returns its argument unchanged")

;; Equivalence: equal? on procedures.
(check-true  (category-equiv? Proc + +)               "same procedure is equivalent")

;; -- Projection to the endomorphism monoid --------------------------
;;
;; Endomorphisms on a single object form a monoid. This is the shortest
;; statement of "every monoid is a one-object category."

(define EM (category->endomorphism-monoid Proc 'any-object))
(check-true (monoid? EM)                              "endomorphisms form a monoid")

;; Apply (+1) then (*2).
(check= ((monoid-op EM (lambda (x) (* x 2))
                       (lambda (x) (+ x 1))) 3)
        8
        "endomorphism monoid: (*2) o (+1) applied to 3")

;; -- with-category destructuring ------------------------------------

(define composed-by-three
  (with-category Proc (o ident eq?)
    (lambda (f g h)
      (o f (o g h)))))

(check= ((composed-by-three (lambda (x) (+ x 100))
                            (lambda (x) (* x 10))
                            (lambda (x) (- x 5))) 8)
        130   ; ((8-5)*10)+100 = 130
        "three-way composition via with-category")

(display "category tour complete") (newline)
