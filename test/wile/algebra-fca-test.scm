;;; algebra-fca-test.scm -- Formal Concept Analysis tests

(import (scheme base)
        (chibi test)
        (wile algebra fca)
        (wile algebra lattice))

;; Local helpers (not in (scheme base))
(define (every pred lst)
  (cond ((null? lst) #t)
        ((pred (car lst)) (every pred (cdr lst)))
        (else #f)))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(test-begin "fca")

;;; ── Sorted string sets ─────────────────────────────────

(test-group "sort-strings"
  (test '() (sort-strings '()))
  (test '("a" "b" "c") (sort-strings '("c" "a" "b")))
  ;; dedup
  (test '("a" "b") (sort-strings '("b" "a" "b" "a")))
  ;; already sorted
  (test '("x" "y" "z") (sort-strings '("x" "y" "z"))))

(test-group "set-intersect"
  (test '() (set-intersect '() '("a" "b")))
  (test '() (set-intersect '("a" "b") '()))
  (test '("b") (set-intersect '("a" "b" "c") '("b" "d")))
  (test '("a" "b") (set-intersect '("a" "b") '("a" "b")))
  ;; disjoint
  (test '() (set-intersect '("a") '("b"))))

(test-group "set-union"
  (test '("a" "b") (set-union '() '("a" "b")))
  (test '("a" "b") (set-union '("a" "b") '()))
  (test '("a" "b" "c" "d") (set-union '("a" "c") '("b" "d")))
  ;; overlap
  (test '("a" "b" "c") (set-union '("a" "b") '("b" "c"))))

(test-group "set-subset?"
  (test #t (set-subset? '() '("a" "b")))
  (test #t (set-subset? '() '()))
  (test #t (set-subset? '("a") '("a" "b")))
  (test #t (set-subset? '("a" "b") '("a" "b")))
  (test #f (set-subset? '("a" "b") '("a")))
  (test #f (set-subset? '("c") '("a" "b"))))

(test-group "set-member?"
  (test #t (set-member? "b" '("a" "b" "c")))
  (test #f (set-member? "d" '("a" "b" "c")))
  (test #f (set-member? "a" '()))
  ;; early exit: element before all list elements
  (test #f (set-member? "a" '("b" "c"))))

;;; ── Context construction ───────────────────────────────

;; Test context:
;;   f1 accesses A.x and B.y
;;   f2 accesses A.x
;;   f3 accesses B.y and C.z
(define ctx
  (context-from-alist
    '(("f1" "A.x" "B.y")
      ("f2" "A.x")
      ("f3" "B.y" "C.z"))))

(test-group "context-construction"
  (test #t (fca-context? ctx))
  (test #f (fca-context? 42))
  (test #f (fca-context? '()))
  ;; objects are sorted
  (test '("f1" "f2" "f3") (context-objects ctx))
  ;; attributes are sorted, deduped
  (test '("A.x" "B.y" "C.z") (context-attributes ctx)))

;;; ── Galois operators ───────────────────────────────────

(test-group "intent"
  ;; intent of {f1} = attributes of f1 = {A.x, B.y}
  (test '("A.x" "B.y") (intent ctx '("f1")))
  ;; intent of {f2} = {A.x}
  (test '("A.x") (intent ctx '("f2")))
  ;; intent of {f1, f2} = intersection = {A.x}
  (test '("A.x") (intent ctx '("f1" "f2")))
  ;; vacuous truth: empty set -> all attributes
  (test '("A.x" "B.y" "C.z") (intent ctx '())))

(test-group "extent"
  ;; extent of {A.x} = objects having A.x = {f1, f2}
  (test '("f1" "f2") (extent ctx '("A.x")))
  ;; extent of {B.y} = {f1, f3}
  (test '("f1" "f3") (extent ctx '("B.y")))
  ;; extent of {A.x, B.y} = intersection = {f1}
  (test '("f1") (extent ctx '("A.x" "B.y")))
  ;; vacuous truth: empty set -> all objects
  (test '("f1" "f2" "f3") (extent ctx '())))

;;; ── Concept lattice ────────────────────────────────────

(define concepts (concept-lattice ctx))

(test-group "concept-lattice"
  ;; non-empty
  (test #t (> (length concepts) 0))
  ;; each concept is (extent . intent) pair
  (test #t (every (lambda (c) (and (pair? c) (list? (concept-extent c)) (list? (concept-intent c)))) concepts))
  ;; top concept has all objects (intent = closure of empty set)
  (let ((top (car (filter (lambda (c) (equal? (concept-extent c) '("f1" "f2" "f3"))) concepts))))
    (test '("f1" "f2" "f3") (concept-extent top)))
  ;; closure property: intent(extent(intent)) = intent for every concept
  (test #t (every (lambda (c) (equal? (concept-intent c) (intent ctx (extent ctx (concept-intent c))))) concepts)))

;;; ── Algebra bridge ─────────────────────────────────────

(define alg (concept-lattice->algebra-lattice ctx concepts))

(test-group "algebra-lattice"
  (test #t (lattice? alg))
  ;; top has all objects
  (test '("f1" "f2" "f3") (concept-extent (lattice-top alg)))
  ;; bottom has fewest objects (most attributes)
  (test #t (list? (concept-extent (lattice-bottom alg))))
  ;; leq: bottom <= top
  (test #t (lattice-leq? alg (lattice-bottom alg) (lattice-top alg)))
  ;; leq: top not <= bottom (unless trivial)
  (test #f (lattice-leq? alg (lattice-top alg) (lattice-bottom alg))))

;;; ── Concept relationship ───────────────────────────────

(test-group "concept-relationship"
  ;; equal for same concept
  (let ((c (car concepts)))
    (test 'equal (concept-relationship c c)))
  ;; top vs bottom: top is superconcept of bottom
  (let ((top (lattice-top alg))
        (bot (lattice-bottom alg)))
    (test 'superconcept (concept-relationship top bot))
    (test 'subconcept (concept-relationship bot top))))

(test-end)
(test-exit)
