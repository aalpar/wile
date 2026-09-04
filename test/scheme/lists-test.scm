;;; lists-test.scm - R7RS 6.4 Pairs and lists: edge cases and detailed coverage
;;;
;;; Converted from pkg/registry/core/prim_list_test.go, which was deleted in
;;; the same change; each test-group carries the name of the Go test it
;;; replaces. Complements the canonical R7RS tests in
;;; integration/testdata/r7rs-tests.scm.

(import (scheme base) (scheme char) (scheme cxr) (chibi test))

(test-begin "lists")

;; ── car, cdr, cons, list ─────────────────────────────────────────

(test-group "car"
  (test 1 (car '(1 2 3)))
  (test 'a (car (cons 'a 'b)))
  (test 42 (car '(42)))
  (test '(1 2) (car '((1 2) (3 4))))
  (test "hello" (car '("hello" 1 #t))))

(test-group "car errors"
  (test-error (car '()))
  (test-error (car 42))
  (test-error (car "hello"))
  (test-error (car 'foo))
  (test-error (car #t))
  (test-error (car #(1 2 3))))

(test-group "cdr"
  (test '(2 3) (cdr '(1 2 3)))
  (test 'b (cdr (cons 'a 'b)))
  (test '() (cdr '(42)))
  (test '(2) (cdr '(1 2)))
  (test '((3 4)) (cdr '((1 2) (3 4)))))

(test-group "cdr errors"
  (test-error (cdr '()))
  (test-error (cdr 42))
  (test-error (cdr "hello"))
  (test-error (cdr 'foo))
  (test-error (cdr #t))
  (test-error (cdr #(1 2 3))))

(test-group "cons"
  (test '(1 . 2) (cons 1 2))
  (test '(1) (cons 1 '()))
  (test '(1 2 3) (cons 1 '(2 3)))
  (test '(a . b) (cons 'a 'b))
  (test '((1 . 2) . (3 . 4)) (cons (cons 1 2) (cons 3 4)))
  (test '("hello") (cons "hello" '()))
  (test '((1 2) 3 4) (cons '(1 2) '(3 4))))

(test-group "list"
  (test '(1 2 3) (list 1 2 3))
  (test '() (list))
  (test '(a) (list 'a))
  (test '(1 "two" #t) (list 1 "two" #t)))

;; ── car, cdr, cons: improper lists ───────────────────────────────

(test-group "car improper list"
  (test 1 (car '(1 . 2)))
  (test 'a (car '(a b . c)))
  (test '(1 . 2) (car '((1 . 2) 3))))

(test-group "cdr improper list"
  (test 2 (cdr '(1 . 2)))
  (test '(b . c) (cdr '(a b . c)))
  (test 'y (cdr '(x . y))))

(test-group "cons improper list"
  (test '(a b . c) (cons 'a '(b . c)))
  (test '(1 2 . 3) (cons 1 (cons 2 3))))

;; ── set-car!, set-cdr!: improper lists ───────────────────────────

(test-group "set-car! improper list"
  (test '(10 . 2) (let ((p (cons 1 2))) (set-car! p 10) p))
  (test 2 (let ((p (cons 1 2))) (set-car! p 'x) (cdr p))))

(test-group "set-cdr! improper list"
  (test '(1 . 3) (let ((p (cons 1 2))) (set-cdr! p 3) p))
  (test '(1) (let ((p (cons 1 2))) (set-cdr! p '()) p))
  (test '(1 3 . 4) (let ((p (cons 1 2))) (set-cdr! p (cons 3 4)) p)))

;; ── list-ref, list-tail, list-set!: improper lists ───────────────

(test-group "list-ref improper list"
  (test 'a (list-ref '(a b . c) 0))
  (test 'b (list-ref '(a b . c) 1)))

(test-group "list-ref improper list errors"
  (test-error (list-ref '(a b . c) 2)))

(test-group "list-tail improper list"
  (test '(a b . c) (list-tail '(a b . c) 0))
  (test '(b . c) (list-tail '(a b . c) 1))
  (test 'c (list-tail '(a b . c) 2)))

(test-group "list-tail improper list errors"
  (test-error (list-tail '(a b . c) 3)))

(test-group "list-set! improper list"
  (test 'x (let ((lst (cons 'a (cons 'b 'c)))) (list-set! lst 0 'x) (car lst)))
  (test 'x (let ((lst (cons 'a (cons 'b 'c)))) (list-set! lst 1 'x) (cadr lst))))

(test-group "list-set! improper list errors"
  (test-error (list-set! '(a b . c) 2 'x)))

;; ── memq, memv, member: improper lists ───────────────────────────

(test-group "memq improper list"
  (test '(b . c) (memq 'b '(a b . c))))

(test-group "memq improper list errors"
  (test-error (memq 'z '(a b . c))))

(test-group "memv improper list"
  (test '(2 . 3) (memv 2 '(1 2 . 3))))

(test-group "memv improper list errors"
  (test-error (memv 9 '(1 2 . 3))))

(test-group "member improper list"
  (test '("b" . "c") (member "b" '("a" "b" . "c"))))

(test-group "member improper list errors"
  (test-error (member "z" '("a" "b" . "c"))))

;; ── assq, assv, assoc: improper alists ───────────────────────────

(test-group "assq improper alist"
  (test '(a 1) (assq 'a '((a 1) (b 2) . c)))
  (test '(b 2) (assq 'b '((a 1) (b 2) . c))))

(test-group "assq improper alist errors"
  (test-error (assq 'z '((a 1) (b 2) . c))))

(test-group "assv improper alist"
  (test '(1 a) (assv 1 '((1 a) (2 b) . c)))
  (test '(2 b) (assv 2 '((1 a) (2 b) . c))))

(test-group "assv improper alist errors"
  (test-error (assv 9 '((1 a) (2 b) . c))))

(test-group "assoc improper alist"
  (test '("a" 1) (assoc "a" '(("a" 1) ("b" 2) . c)))
  (test '("b" 2) (assoc "b" '(("a" 1) ("b" 2) . c))))

(test-group "assoc improper alist errors"
  (test-error (assoc "z" '(("a" 1) ("b" 2) . c))))

;; ── memq, memv, member, assq, assv, assoc: circular lists ────────

(test-group "memq circular list"
  (test #t (let ((x (list 'a 'b 'c))) (set-cdr! (cddr x) x) (eq? (car (memq 'a x)) 'a)))
  (test #t (let ((x (list 'a 'b 'c))) (set-cdr! (cddr x) x) (eq? (car (memq 'b x)) 'b)))
  (test #t (let ((x (list 'a 'b 'c))) (set-cdr! (cddr x) x) (eq? (car (memq 'c x)) 'c))))

;; A key absent from a circular list. memq and memv used to spin here forever
;; while assq and assv raised immediately; all four now report the same
;; improper-list rejection.
(test-group "member search circular list not found"
  (test-error (let ((x (list 'a 'b 'c))) (set-cdr! (cddr x) x) (memq 'zz x)))
  (test-error (let ((x (list 1 2 3))) (set-cdr! (cddr x) x) (memv 99 x)))
  (test-error (let ((x (list '(a 1) '(b 2)))) (set-cdr! (cdr x) x) (assq 'zz x)))
  (test-error (let ((x (list '(1 a) '(2 b)))) (set-cdr! (cdr x) x) (assv 99 x))))

(test-group "memv circular list"
  (test #t (let ((x (list 1 2 3))) (set-cdr! (cddr x) x) (eqv? (car (memv 1 x)) 1)))
  (test #t (let ((x (list 1 2 3))) (set-cdr! (cddr x) x) (eqv? (car (memv 2 x)) 2)))
  (test #t (let ((x (list 1 2 3))) (set-cdr! (cddr x) x) (eqv? (car (memv 3 x)) 3))))

(test-group "member circular list"
  (test #t (let ((x (list "a" "b" "c"))) (set-cdr! (cddr x) x) (equal? (car (member "a" x)) "a")))
  (test #t (let ((x (list "a" "b" "c"))) (set-cdr! (cddr x) x) (equal? (car (member "b" x)) "b")))
  (test #t (let ((x (list "a" "b" "c"))) (set-cdr! (cddr x) x) (equal? (car (member "c" x)) "c"))))

(test-group "assq circular list"
  (test #t (let ((x (list '(a 1) '(b 2) '(c 3)))) (set-cdr! (cddr x) x) (equal? (assq 'a x) '(a 1))))
  (test #t (let ((x (list '(a 1) '(b 2) '(c 3)))) (set-cdr! (cddr x) x) (equal? (assq 'b x) '(b 2))))
  (test #t (let ((x (list '(a 1) '(b 2) '(c 3)))) (set-cdr! (cddr x) x) (equal? (assq 'c x) '(c 3)))))

(test-group "assv circular list"
  (test #t (let ((x (list '(1 a) '(2 b) '(3 c)))) (set-cdr! (cddr x) x) (equal? (assv 1 x) '(1 a))))
  (test #t (let ((x (list '(1 a) '(2 b) '(3 c)))) (set-cdr! (cddr x) x) (equal? (assv 2 x) '(2 b))))
  (test #t (let ((x (list '(1 a) '(2 b) '(3 c)))) (set-cdr! (cddr x) x) (equal? (assv 3 x) '(3 c)))))

(test-group "assoc circular list"
  (test #t (let ((x (list '("a" 1) '("b" 2) '("c" 3)))) (set-cdr! (cddr x) x) (equal? (assoc "a" x) '("a" 1))))
  (test #t (let ((x (list '("a" 1) '("b" 2) '("c" 3)))) (set-cdr! (cddr x) x) (equal? (assoc "b" x) '("b" 2))))
  (test #t (let ((x (list '("a" 1) '("b" 2) '("c" 3)))) (set-cdr! (cddr x) x) (equal? (assoc "c" x) '("c" 3)))))

;; ── memq, memv, member: lasso-shaped circular lists ──────────────
;; (a b c d) with the last cdr pointing back at the second cell:
;; a -> b -> c -> d -> b -> c -> d -> ...  The head is outside the cycle.

(test-group "memq circular list lasso"
  (test #t (let ((x (list 'a 'b 'c 'd))) (set-cdr! (cdddr x) (cdr x)) (eq? (car (memq 'a x)) 'a)))
  (test #t (let ((x (list 'a 'b 'c 'd))) (set-cdr! (cdddr x) (cdr x)) (eq? (car (memq 'b x)) 'b)))
  (test #t (let ((x (list 'a 'b 'c 'd))) (set-cdr! (cdddr x) (cdr x)) (eq? (car (memq 'c x)) 'c)))
  (test #t (let ((x (list 'a 'b 'c 'd))) (set-cdr! (cdddr x) (cdr x)) (eq? (car (memq 'd x)) 'd))))

(test-group "memv circular list lasso"
  (test #t (let ((x (list 10 20 30 40))) (set-cdr! (cdddr x) (cdr x)) (eqv? (car (memv 10 x)) 10)))
  (test #t (let ((x (list 10 20 30 40))) (set-cdr! (cdddr x) (cdr x)) (eqv? (car (memv 20 x)) 20)))
  (test #t (let ((x (list 10 20 30 40))) (set-cdr! (cdddr x) (cdr x)) (eqv? (car (memv 30 x)) 30)))
  (test #t (let ((x (list 10 20 30 40))) (set-cdr! (cdddr x) (cdr x)) (eqv? (car (memv 40 x)) 40))))

(test-group "member circular list lasso"
  (test #t (let ((x (list "w" "x" "y" "z"))) (set-cdr! (cdddr x) (cdr x)) (equal? (car (member "w" x)) "w")))
  (test #t (let ((x (list "w" "x" "y" "z"))) (set-cdr! (cdddr x) (cdr x)) (equal? (car (member "x" x)) "x")))
  (test #t (let ((x (list "w" "x" "y" "z"))) (set-cdr! (cdddr x) (cdr x)) (equal? (car (member "y" x)) "y")))
  (test #t (let ((x (list "w" "x" "y" "z"))) (set-cdr! (cdddr x) (cdr x)) (equal? (car (member "z" x)) "z"))))

;; ── memq, memv, member: single- and two-element cycles ───────────

(test-group "memq circular list small"
  (test #t (let ((x (list 'a))) (set-cdr! x x) (eq? (car (memq 'a x)) 'a)))
  (test #t (let ((x (list 'a 'b))) (set-cdr! (cdr x) x) (eq? (car (memq 'a x)) 'a)))
  (test #t (let ((x (list 'a 'b))) (set-cdr! (cdr x) x) (eq? (car (memq 'b x)) 'b))))

(test-group "memv circular list small"
  (test #t (let ((x (list 1))) (set-cdr! x x) (eqv? (car (memv 1 x)) 1)))
  (test #t (let ((x (list 1 2))) (set-cdr! (cdr x) x) (eqv? (car (memv 1 x)) 1)))
  (test #t (let ((x (list 1 2))) (set-cdr! (cdr x) x) (eqv? (car (memv 2 x)) 2))))

(test-group "member circular list small"
  (test #t (let ((x (list "a"))) (set-cdr! x x) (equal? (car (member "a" x)) "a")))
  (test #t (let ((x (list "a" "b"))) (set-cdr! (cdr x) x) (equal? (car (member "a" x)) "a")))
  (test #t (let ((x (list "a" "b"))) (set-cdr! (cdr x) x) (equal? (car (member "b" x)) "b"))))

;; ── assq, assv, assoc: lasso-shaped circular alists ──────────────

(test-group "assq circular alist lasso"
  (test #t (let ((x (list '(a 1) '(b 2) '(c 3) '(d 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assq 'a x) '(a 1))))
  (test #t (let ((x (list '(a 1) '(b 2) '(c 3) '(d 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assq 'b x) '(b 2))))
  (test #t (let ((x (list '(a 1) '(b 2) '(c 3) '(d 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assq 'c x) '(c 3))))
  (test #t (let ((x (list '(a 1) '(b 2) '(c 3) '(d 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assq 'd x) '(d 4)))))

(test-group "assv circular alist lasso"
  (test #t (let ((x (list '(1 a) '(2 b) '(3 c) '(4 d)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assv 1 x) '(1 a))))
  (test #t (let ((x (list '(1 a) '(2 b) '(3 c) '(4 d)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assv 2 x) '(2 b))))
  (test #t (let ((x (list '(1 a) '(2 b) '(3 c) '(4 d)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assv 3 x) '(3 c))))
  (test #t (let ((x (list '(1 a) '(2 b) '(3 c) '(4 d)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assv 4 x) '(4 d)))))

(test-group "assoc circular alist lasso"
  (test #t (let ((x (list '("a" 1) '("b" 2) '("c" 3) '("d" 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assoc "a" x) '("a" 1))))
  (test #t (let ((x (list '("a" 1) '("b" 2) '("c" 3) '("d" 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assoc "b" x) '("b" 2))))
  (test #t (let ((x (list '("a" 1) '("b" 2) '("c" 3) '("d" 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assoc "c" x) '("c" 3))))
  (test #t (let ((x (list '("a" 1) '("b" 2) '("c" 3) '("d" 4)))) (set-cdr! (cdddr x) (cdr x)) (equal? (assoc "d" x) '("d" 4)))))

;; ── assq, assv, assoc: single- and two-entry cycles ──────────────

(test-group "assq circular alist small"
  (test #t (let ((x (list '(a 1)))) (set-cdr! x x) (equal? (assq 'a x) '(a 1))))
  (test #t (let ((x (list '(a 1) '(b 2)))) (set-cdr! (cdr x) x) (equal? (assq 'a x) '(a 1))))
  (test #t (let ((x (list '(a 1) '(b 2)))) (set-cdr! (cdr x) x) (equal? (assq 'b x) '(b 2)))))

(test-group "assv circular alist small"
  (test #t (let ((x (list '(1 a)))) (set-cdr! x x) (equal? (assv 1 x) '(1 a))))
  (test #t (let ((x (list '(1 a) '(2 b)))) (set-cdr! (cdr x) x) (equal? (assv 1 x) '(1 a))))
  (test #t (let ((x (list '(1 a) '(2 b)))) (set-cdr! (cdr x) x) (equal? (assv 2 x) '(2 b)))))

(test-group "assoc circular alist small"
  (test #t (let ((x (list '("a" 1)))) (set-cdr! x x) (equal? (assoc "a" x) '("a" 1))))
  (test #t (let ((x (list '("a" 1) '("b" 2)))) (set-cdr! (cdr x) x) (equal? (assoc "a" x) '("a" 1))))
  (test #t (let ((x (list '("a" 1) '("b" 2)))) (set-cdr! (cdr x) x) (equal? (assoc "b" x) '("b" 2)))))

;; ── member, assoc: circular lists with a custom comparator ───────

(test-group "member circular list custom compare"
  ;; full cycle (10 20 30 10 20 30 ...)
  (test #t (let ((x (list 10 20 30)))
             (set-cdr! (cddr x) x)
             (eqv? (car (member 25 x (lambda (a b) (> b 15)))) 20)))
  ;; lasso 1 -> 2 -> 3 -> 4 -> 2 -> ...
  (test #t (let ((x (list 1 2 3 4)))
             (set-cdr! (cdddr x) (cdr x))
             (eqv? (car (member 0 x (lambda (a b) (= b 4)))) 4))))

(test-group "assoc circular alist custom compare"
  (test #t (let ((x (list '(1 a) '(2 b) '(3 c))))
             (set-cdr! (cddr x) x)
             (equal? (assoc 0 x (lambda (a b) (= b 3))) '(3 c))))
  (test #t (let ((x (list '(1 a) '(2 b) '(3 c) '(4 d))))
             (set-cdr! (cdddr x) (cdr x))
             (equal? (assoc 0 x (lambda (a b) (= b 4))) '(4 d)))))

;; ── append: improper non-last argument, structure sharing ────────

(test-group "append improper list errors"
  (test-error (append '(1 . 2) '(3)))
  (test-error (append '(a . b) '(c) '(d))))

;; R7RS 6.4: append copies every argument but the last, and the result's final
;; tail is the last argument itself, so eq? on the tail must hold.
(test-group "append shares last argument"
  (test #t (let ((tail (list 3 4))) (eq? (cddr (append (list 1 2) tail)) tail)))
  (test #t (let ((tail (list 5))) (eq? (cdddr (append (list 1) (list 2 3) tail)) tail)))
  (test #t (let ((tail (list 9))) (eq? (append '() '() tail) tail)))
  (test #t (let ((tail (cons 1 2))) (eq? (cdr (append (list 0) tail)) tail))))

;; The dual: copied prefix cells are fresh, so mutating the result must not
;; disturb the original prefix list.
(test-group "append copies prefix"
  (test #t (let ((p (list 1 2)))
             (let ((r (append p (list 3))))
               (set-car! r 99)
               (eqv? (car p) 1)))))

;; ── cxr: improper lists ──────────────────────────────────────────

(test-group "cxr improper list"
  (test 'b (cadr '(a b . c)))
  (test 'c (cddr '(a b . c)))
  (test 2 (cdar '((1 . 2) 3))))

(test-group "cxr improper list errors"
  (test-error (caddr '(a b . c)))
  (test-error (caaar '(1 . 2))))

;; ── null?, pair?, list? ──────────────────────────────────────────

(test-group "null?"
  (test #t (null? '()))
  (test #f (null? '(1 2 3)))
  (test #f (null? '(1)))
  (test #f (null? (cons 1 2)))
  (test #f (null? 42))
  (test #f (null? "hello"))
  (test #f (null? 'foo))
  (test #f (null? #t))
  (test #f (null? #f))
  (test #f (null? #(1 2 3)))
  (test #f (null? #\a)))

(test-group "pair?"
  (test #t (pair? (cons 1 2)))
  (test #t (pair? '(1 2 3)))
  (test #t (pair? '(1)))
  (test #t (pair? '((1 2) (3 4))))
  (test #t (pair? '(1 2 . 3)))
  (test #f (pair? '()))
  (test #f (pair? 42))
  (test #f (pair? "hello"))
  (test #f (pair? 'foo))
  (test #f (pair? #t))
  (test #f (pair? #(1 2 3)))
  (test #f (pair? #\a)))

(test-group "list?"
  (test #t (list? '()))
  (test #t (list? '(1)))
  (test #t (list? '(1 2 3)))
  (test #t (list? '((1 2) (3 4))))
  (test #t (list? '(1 "two" #t)))
  (test #f (list? (cons 1 2)))
  (test #f (list? '(1 2 . 3)))
  (test #f (list? 42))
  (test #f (list? "hello"))
  (test #f (list? 'foo))
  (test #f (list? #t))
  (test #f (list? #(1 2 3)))
  (test #f (list? #\a)))

;; ── set-car!, set-cdr!, list-set! ────────────────────────────────

(test-group "set-car!"
  (test 10 (let ((p (cons 1 2))) (set-car! p 10) (car p)))
  (test '(10 2 3) (let ((lst (list 1 2 3))) (set-car! lst 10) lst))
  (test "hello" (let ((p (cons 1 2))) (set-car! p "hello") (car p)))
  (test 2 (let ((p (cons 1 2))) (set-car! p 10) (cdr p)))
  (test '(10 20) (let ((lst (list (list 1 2) (list 3 4)))) (set-car! lst '(10 20)) (car lst))))

(test-group "set-car! errors"
  (test-error (set-car! '() 1))
  (test-error (set-car! 42 1))
  (test-error (set-car! "hello" 1)))

(test-group "set-cdr!"
  (test 20 (let ((p (cons 1 2))) (set-cdr! p 20) (cdr p)))
  (test '(1) (let ((lst (list 1 2 3))) (set-cdr! lst '()) lst))
  (test '(1 2 3) (let ((lst (list 1))) (set-cdr! lst '(2 3)) lst))
  (test 1 (let ((p (cons 1 2))) (set-cdr! p 20) (car p)))
  (test '(1 2 . 3) (let ((lst (list 1 2))) (set-cdr! (cdr lst) 3) lst)))

(test-group "set-cdr! errors"
  (test-error (set-cdr! '() 1))
  (test-error (set-cdr! 42 1))
  (test-error (set-cdr! "hello" 1)))

(test-group "list-set!"
  (test '(10 2 3) (let ((lst (list 1 2 3))) (list-set! lst 0 10) lst))
  (test '(1 20 3) (let ((lst (list 1 2 3))) (list-set! lst 1 20) lst))
  (test '(1 2 30) (let ((lst (list 1 2 3))) (list-set! lst 2 30) lst))
  (test '(1 "hello" 3) (let ((lst (list 1 2 3))) (list-set! lst 1 "hello") lst))
  (test '(a b) (let ((lst (list 1 2 3))) (list-set! lst 0 '(a b)) (car lst))))

(test-group "list-set! errors"
  (test-error (list-set! (list 1 2 3) 5 10))
  (test-error (list-set! (list 1 2 3) -1 10))
  (test-error (list-set! '() 0 10))
  (test-error (list-set! 42 0 10)))

;; ── make-list, append ────────────────────────────────────────────

(test-group "make-list"
  (test '(a a a) (make-list 3 'a))
  (test '(0 0 0 0) (make-list 4 0))
  (test '(x) (make-list 1 'x))
  (test '() (make-list 0 'a))
  (test 5 (length (make-list 5))))

(test-group "make-list errors"
  (test-error (make-list -1 'a))
  (test-error (make-list "three" 'a)))

(test-group "append"
  (test '() (append))
  (test '() (append '()))
  (test '(1 2) (append '(1 2)))
  (test '(1 2 3 4) (append '(1 2) '(3 4)))
  (test '(a b c) (append '(a) '(b) '(c)))
  (test '(1 2) (append '(1) '() '(2)))
  (test '(1 2 . 3) (append '(1 2) 3))
  (test '() (append '() '()))
  (test '((1 2) (3 4)) (append '((1 2)) '((3 4))))
  (test '(1 2 3 4) (append '(1) '(2) '(3) '(4)))
  (test '(1 2 "a" "b" #t #f) (append '(1 2) '("a" "b") '(#t #f)))
  (test '(a b . c) (append '(a b) 'c))
  (test '() (append '() '() '())))

;; ── length, reverse, list-ref, list-tail ─────────────────────────

(test-group "length"
  (test 3 (length '(1 2 3)))
  (test 0 (length '()))
  (test 1 (length '(a)))
  (test 2 (length '(a b)))
  (test 5 (length '(1 2 3 4 5)))
  (test 3 (length '((1 2) (3 4) (5 6))))
  (test 4 (length '(1 "two" #t 'four))))

(test-group "length errors"
  (test-error (length (cons 1 2)))
  (test-error (length 42))
  (test-error (length "hello"))
  (test-error (length 'foo)))

(test-group "reverse"
  (test '(3 2 1) (reverse '(1 2 3)))
  (test '() (reverse '()))
  (test '(a) (reverse '(a)))
  (test '(b a) (reverse '(a b)))
  (test '(5 4 3 2 1) (reverse '(1 2 3 4 5)))
  (test '((3 4) (1 2)) (reverse '((1 2) (3 4))))
  (test '(3 4) (car (reverse '((1 2) (3 4))))))

(test-group "reverse errors"
  (test-error (reverse (cons 1 2)))
  (test-error (reverse 42))
  (test-error (reverse "hello")))

(test-group "list-ref"
  (test 'a (list-ref '(a b c) 0))
  (test 'b (list-ref '(a b c) 1))
  (test 'c (list-ref '(a b c) 2))
  (test '(b) (list-ref '((a) (b) (c)) 1))
  (test 10 (list-ref '(1 2 3 4 5 6 7 8 9 10) 9))
  (test 'a (list-ref '(a b c d e) 0)))

(test-group "list-ref errors"
  (test-error (list-ref '() 0))
  (test-error (list-ref '(a b c) 5))
  (test-error (list-ref '(a b c) -1))
  (test-error (list-ref 42 0))
  (test-error (list-ref '(a b c) "one")))

(test-group "list-tail"
  (test '(a b c) (list-tail '(a b c) 0))
  (test '(b c) (list-tail '(a b c) 1))
  (test '() (list-tail '(a b c) 3))
  (test '(c d e) (list-tail '(a b c d e) 2))
  (test '(1 2) (list-tail '(1 2) 0))
  (test '() (list-tail '() 0)))

(test-group "list-tail errors"
  (test-error (list-tail '(a b c) 5))
  (test-error (list-tail '(a b c) -1))
  (test-error (list-tail '(a b c) "two"))
  (test-error (list-tail 42 1)))

;; ── memq, memv, member ───────────────────────────────────────────

(test-group "memq"
  ;; eq? identity: booleans are singletons, symbols are interned
  (test '(#t 1) (memq #t '(#f #t 1)))
  (test #f (memq #t '(#f 1 2)))
  (test #f (memq #t '()))
  (test '(b c) (memq 'b '(a b c)))
  (test #f (memq 'd '(a b c)))
  (test '(#t #t) (memq #t '(#f #f #t #t))))

(test-group "memv"
  (test '(2 3) (memv 2 '(1 2 3)))
  (test #f (memv 4 '(1 2 3)))
  (test '(3 4 5) (memv 3 '(1 2 3 4 5)))
  (test #f (memv 10 '(1 2 3)))
  (test '(#\b #\c) (memv #\b '(#\a #\b #\c)))
  (test '(x y z) (memv 'x '(a b x y z)))
  (test '(1 2 3) (memv 1 '(1 2 3)))
  (test '(3) (memv 3 '(1 2 3))))

(test-group "member"
  (test '((2) (3)) (member '(2) '((1) (2) (3))))
  (test '("hello" "foo") (member "hello" '("world" "hello" "foo")))
  (test #f (member '(4) '((1) (2) (3))))
  (test #f (member "bar" '("foo" "baz")))
  (test '((2 3) (3 4)) (member '(2 3) '((1 2) (2 3) (3 4))))
  (test '(42 100) (member 42 '(1 42 100)))
  (test #f (member 'x '()))
  (test '(#\b #\c) (member #\b '(#\a #\b #\c))))

;; R7RS 6.4: optional compare procedure
(test-group "member with compare"
  (test '(2 3) (member 2.0 '(1 2 3) =))
  (test #f (member 5 '(1 2 3) =))
  (test '("B" "c") (member "B" '("a" "B" "c") string=?))
  (test '("B" "C") (member "b" '("A" "B" "C") string-ci=?))
  (test #f (member "d" '("A" "B" "C") string-ci=?))
  (test '(3 4) (member 2 '(1 2 3 4) (lambda (obj elem) (> elem obj))))
  (test #f (member 'x '(a b c) (lambda (a b) #f)))
  (test '(a b c) (member 'x '(a b c) (lambda (a b) #t)))
  (test #f (member 1 '() =))
  (test '(b c) (member 'b '(a b c) eq?)))

;; ── assq, assv, assoc ────────────────────────────────────────────

(test-group "assq"
  (test '(#t 2) (assq #t '((#f 1) (#t 2))))
  (test #f (assq #t '((#f 1))))
  (test #f (assq #t '()))
  (test '(b 2) (assq 'b '((a 1) (b 2) (c 3))))
  (test #f (assq 'd '((a 1) (b 2) (c 3))))
  (test '(#f no) (assq #f '((#t yes) (#f no))))
  (test '(a 1) (assq 'a '((a 1) (a 2) (a 3)))))

(test-group "assv"
  (test '(2 b) (assv 2 '((1 a) (2 b) (3 c))))
  (test #f (assv 4 '((1 a) (2 b) (3 c))))
  (test #f (assv 5 '((1 one) (2 two))))
  (test '(#\b beta) (assv #\b '((#\a alpha) (#\b beta) (#\c gamma))))
  (test '(0 zero) (assv 0 '((0 zero) (1 one) (0 another-zero))))
  (test '(x ecks) (assv 'x '((y why) (x ecks) (z zee)))))

(test-group "assoc"
  (test '((1 2) found) (assoc '(1 2) '(((1 2) found) ((3 4) other))))
  (test #f (assoc '(5 6) '(((1 2) a) ((3 4) b))))
  (test '("hello" found) (assoc "hello" '(("hello" found) ("world" other))))
  (test '("hello" 2) (assoc "hello" '(("world" 1) ("hello" 2))))
  (test #f (assoc '(5 6) '(((1 2) a) ((3 4) b))))
  (test #f (assoc 'x '()))
  (test '(42 b) (assoc 42 '((1 a) (42 b) (100 c))))
  (test '(#\y why) (assoc #\y '((#\a alpha) (#\y why)))))

;; R7RS 6.4: optional compare procedure
(test-group "assoc with compare"
  (test '(2 two) (assoc 2.0 '((1 one) (2 two) (3 three)) =))
  (test #f (assoc 5 '((1 one) (2 two) (3 three)) =))
  (test '("B" beta) (assoc "B" '(("a" alpha) ("B" beta) ("c" gamma)) string=?))
  (test '("B" beta) (assoc "b" '(("A" alpha) ("B" beta) ("C" gamma)) string-ci=?))
  (test #f (assoc "d" '(("A" alpha) ("B" beta)) string-ci=?))
  (test '(3 three) (assoc 2 '((1 one) (2 two) (3 three) (4 four)) (lambda (obj key) (> key obj))))
  (test #f (assoc 'x '((a 1) (b 2) (c 3)) (lambda (a b) #f)))
  (test '(a 1) (assoc 'x '((a 1) (b 2) (c 3)) (lambda (a b) #t)))
  (test #f (assoc 1 '() =))
  (test '(b 2) (assoc 'b '((a 1) (b 2) (c 3)) eq?))
  (test '(#\b beta) (assoc #\b '((#\a alpha) (#\b beta) (#\c gamma)) char=?)))

;; ── list-copy ────────────────────────────────────────────────────

(test-group "list-copy"
  (test '() (list-copy '()))
  (test '(1) (list-copy '(1)))
  (test '(1 2 3) (list-copy '(1 2 3)))
  (test '((1 2) (3 4)) (list-copy '((1 2) (3 4))))
  (test '(1 . 2) (list-copy (cons 1 2)))
  (test '(1 2 . 3) (list-copy '(1 2 . 3)))
  ;; a non-pair is returned as-is
  (test 42 (list-copy 42))
  (test "hello" (list-copy "hello"))
  (test 'foo (list-copy 'foo))
  (test #t (list-copy #t)))

(test-group "list-copy spine independence"
  (test 2 (let ((orig (list 1 2 3)))
            (let ((copy (list-copy orig)))
              (set-cdr! copy '(99))
              (cadr orig)))))

(test-group "list-copy element sharing"
  ;; car elements are shared, not deep-copied
  (test #t (let ((inner (list 1 2)))
             (let ((orig (list inner 3)))
               (let ((copy (list-copy orig)))
                 (eq? (car orig) (car copy))))))
  ;; R7RS 6.4: the copy shares the improper tail. A string tail is observable
  ;; by identity, pinning the tail-sharing contract.
  (test #t (let ((tail "x"))
             (let ((orig (cons 1 (cons 2 tail))))
               (eq? (cddr (list-copy orig)) tail)))))

;; ── memq, memv, member: non-list second argument ─────────────────

(test-group "memq errors"
  (test-error (memq 1 42))
  (test-error (memq 'a "hello"))
  (test-error (memq 1 #(1 2 3)))
  (test-error (memq 'a #t)))

(test-group "memv errors"
  (test-error (memv 1 42))
  (test-error (memv 1 "hello"))
  (test-error (memv 1 #(1 2 3))))

(test-group "member errors"
  (test-error (member 1 42))
  (test-error (member 'a "hello"))
  (test-error (member 1 #(1 2 3))))

;; ── assq, assv, assoc: non-list and malformed alists ─────────────

(test-group "assq errors"
  (test-error (assq 'a 42))
  (test-error (assq 'a "hello"))
  (test-error (assq 'a '(not-a-pair))))

(test-group "assv errors"
  (test-error (assv 1 42))
  (test-error (assv 1 "hello"))
  (test-error (assv 1 '(not-a-pair))))

(test-group "assoc errors"
  (test-error (assoc 'a 42))
  (test-error (assoc 'a "hello"))
  (test-error (assoc 'a '(not-a-pair))))

;; ── append: non-list non-last argument ───────────────────────────

(test-group "append errors"
  (test-error (append 42 '(1)))
  (test-error (append '(1) 42 '(3)))
  (test-error (append "hello" '(1))))

;; ── rest-arg buffer aliasing ─────────────────────────────────────
;; The variadic rest-arg list is backed by a buffer reused across calls; list
;; must copy the spine or the second call corrupts the first result.

(test-group "rest-arg buffer aliasing"
  (test '((1 2 3) (4 5 6))
        (let ((first (list 1 2 3))
              (second (list 4 5 6)))
          (list first second))))

(test-end)
(test-exit)
