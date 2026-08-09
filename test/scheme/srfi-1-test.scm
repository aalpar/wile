;;; srfi-1-test.scm - SRFI-1, the list library.
;;;
;;; SRFI-1 shipped with no Scheme-level suite at all. This is the first one.
;;;
;;; COVERAGE BOUNDARY, and what covers the rest:
;;;
;;;   - This suite exercises BEHAVIOUR. Whether (srfi 1)'s export list matches
;;;     SRFI-1's procedure index is diffed by TestLibraryExportDiff
;;;     (pkg/wile/library_export_diff_test.go); a name missing from the .sld
;;;     fails there, not here.
;;;   - The (only (srfi 1) ...) import below is the regression arm for the
;;;     2026-08-09 fix, and it is an import rather than a test-group on purpose.
;;;     SRFI-1's export paragraph elides `car cdr ... cddddr`, so (srfi 1)
;;;     shipped without the 28 c...r compositions and the 20 R5RS list
;;;     procedures it re-exports, and (import (only (srfi 1) caar)) raised
;;;     "identifier caar not exported by (srfi 1)". CALLING those names proves
;;;     nothing — they are ambient bindings and resolve with or without the
;;;     export — so only an (only ...) import, which resolves strictly through
;;;     the library's own export set, can fail when the export is missing. A
;;;     missing name aborts this file at expand time.
;;;   - It does not cover the linear-update (!) variants beyond checking they
;;;     are callable, because SRFI-1 leaves their destructiveness unspecified.
;;;   - It does not cover circular-list traversal, which SRFI-1 defines only for
;;;     the procedures explicitly documented to accept circular arguments.

;; The regression arm. Every name here is one the export paragraph elided; an
;; (only ...) import resolves strictly through (srfi 1)'s own export set, so a
;; name missing from the .sld aborts this file rather than silently falling
;; through to the ambient binding.
(import (only (srfi 1)
              cons car cdr set-car! set-cdr! pair? null? list length append
              reverse list-ref map for-each member memq memv assoc assq assv
              caar cadr cdar cddr
              caaar caadr cadar caddr cdaar cdadr cddar cdddr
              caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
              cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))

(import (scheme base)
        (scheme write)
        (srfi 1)
        (chibi test))

(test-begin "srfi-1")

;; ── Re-exported surface: the names the export paragraph elided ────
(test-group "re-exported R5RS and c...r surface"
  (test 1 (car '(1 2 3)))
  (test '(2 3) (cdr '(1 2 3)))
  (test 1 (caar '((1) 2)))
  (test 2 (cadr '(1 2)))
  (test '(3 4) (cddr '(1 2 3 4)))
  (test 1 (caaar '(((1)))))
  (test 4 (cadddr '(1 2 3 4)))
  (test 5 (car (cddddr '(1 2 3 4 5))))
  (test '(1 2 3) (append '(1) '(2 3)))
  (test 3 (length '(a b c)))
  (test '(3 2 1) (reverse '(1 2 3)))
  (test 'b (list-ref '(a b c) 1))
  (test '(b c) (member 'b '(a b c)))
  (test '(x . 2) (assq 'x '((x . 2) (y . 3))))
  (test '(y . 3) (assv 'y '((x . 2) (y . 3))))
  (test '(x . 2) (assoc 'x '((x . 2))))
  (test #t (pair? '(1)))
  (test #t (null? '()))
  (test '(2 4) (map (lambda (n) (* n 2)) '(1 2))))

;; ── Constructors ──────────────────────────────────────────────────
(test-group "constructors"
  (test '(b . a) (xcons 'a 'b))
  (test '(1 2 . 3) (cons* 1 2 3))
  ;; SRFI-1: with a single argument cons* returns it unchanged, not a list.
  (test 1 (cons* 1))
  (test '(x x x) (make-list 3 'x))
  (test '(0 2 4) (list-tabulate 3 (lambda (i) (* 2 i))))
  (test '(1 2 3) (list-copy '(1 2 3)))
  (test '(0 1 2 3) (iota 4))
  (test '(2 4 6) (iota 3 2 2)))

;; ── Predicates ────────────────────────────────────────────────────
(test-group "predicates"
  (test #t (proper-list? '(1 2 3)))
  (test #f (proper-list? '(1 . 2)))
  (test #t (dotted-list? '(1 . 2)))
  (test #t (not-pair? 'x))
  (test #f (not-pair? '(x)))
  (test #t (null-list? '()))
  (test #f (null-list? '(1)))
  (test #t (list= eq? '(a b) '(a b)))
  (test #f (list= eq? '(a b) '(a c))))

;; ── Selectors ─────────────────────────────────────────────────────
(test-group "selectors"
  (test 1 (first '(1 2 3 4 5 6 7 8 9 10)))
  (test 5 (fifth '(1 2 3 4 5 6 7 8 9 10)))
  (test 10 (tenth '(1 2 3 4 5 6 7 8 9 10)))
  (test '(1 2) (take '(1 2 3) 2))
  (test '(3) (drop '(1 2 3) 2))
  (test '(2 3) (take-right '(1 2 3) 2))
  (test '(1) (drop-right '(1 2 3) 2))
  (test '(3) (last-pair '(1 2 3)))
  (test 3 (last '(1 2 3)))
  (test 3 (length+ '(1 2 3)))
  (test '(1 (2 3))
        (call-with-values (lambda () (car+cdr '(1 2 3))) list))
  (test '((1) (2 3))
        (call-with-values (lambda () (split-at '(1 2 3) 1)) list)))

;; ── Fold, unfold, map ─────────────────────────────────────────────
(test-group "fold and map"
  (test 6 (fold + 0 '(1 2 3)))
  (test '(3 2 1) (fold cons '() '(1 2 3)))
  (test '(1 2 3) (fold-right cons '() '(1 2 3)))
  (test 6 (reduce + 0 '(1 2 3)))
  (test 0 (reduce + 0 '()))
  (test 6 (reduce-right + 0 '(1 2 3)))
  (test '(1 4 9) (unfold (lambda (n) (> n 3))
                         (lambda (n) (* n n))
                         (lambda (n) (+ n 1))
                         1))
  (test '(1 4 9) (unfold-right (lambda (n) (< n 1))
                               (lambda (n) (* n n))
                               (lambda (n) (- n 1))
                               3))
  (test '(1 1 3 3) (append-map (lambda (n) (list n n)) '(1 3)))
  (test '(2 4) (filter-map (lambda (n) (and (even? n) n)) '(1 2 3 4)))
  (test '(1 2 3) (map-in-order (lambda (n) n) '(1 2 3)))
  (test 3 (let ((n 0))
            (pair-for-each (lambda (p) (set! n (+ n 1))) '(1 2 3))
            n))
  (test '((1 2 3) (2 3) (3)) (pair-fold-right cons '() '(1 2 3))))

;; ── Filtering and deletion ────────────────────────────────────────
(test-group "filtering and deletion"
  (test '(2 4) (filter even? '(1 2 3 4)))
  (test '(1 3) (remove even? '(1 2 3 4)))
  (test '((2 4) (1 3))
        (call-with-values (lambda () (partition even? '(1 2 3 4))) list))
  (test '(1 3) (delete 2 '(1 2 3 2)))
  (test '(1 2 3) (delete-duplicates '(1 2 1 3 2)))
  (test 2 (count even? '(1 2 3 4))))

;; ── Searching ─────────────────────────────────────────────────────
(test-group "searching"
  (test 4 (find even? '(1 3 4 5)))
  (test #f (find even? '(1 3 5)))
  (test '(4 5) (find-tail even? '(1 3 4 5)))
  (test #t (any even? '(1 3 4)))
  (test #f (any even? '(1 3 5)))
  (test #t (every odd? '(1 3 5)))
  (test #f (every odd? '(1 2 3)))
  (test 2 (list-index even? '(1 3 4 5)))
  (test '(1 3) (take-while odd? '(1 3 4 5)))
  (test '(4 5) (drop-while odd? '(1 3 4 5)))
  (test '((1 3) (4 5))
        (call-with-values (lambda () (span odd? '(1 3 4 5))) list))
  (test '((1 3) (4 5))
        (call-with-values (lambda () (break even? '(1 3 4 5))) list)))

;; ── Zipping and reversal ──────────────────────────────────────────
(test-group "zipping and reversal"
  (test '((1 a) (2 b)) (zip '(1 2) '(a b)))
  (test '(1 2) (unzip1 '((1 a) (2 b))))
  (test '(1 2 3 4) (concatenate '((1 2) (3 4))))
  (test '(3 2 1 4 5) (append-reverse '(1 2 3) '(4 5)))
  (test '(3 2 1) (reverse! (list 1 2 3))))

;; ── Association lists ─────────────────────────────────────────────
(test-group "association lists"
  (test '((k . v)) (alist-cons 'k 'v '()))
  (test '((a . 1) (b . 2)) (alist-copy '((a . 1) (b . 2))))
  (test '((b . 2)) (alist-delete 'a '((a . 1) (b . 2)))))

;; ── Set operations on lists ───────────────────────────────────────
(test-group "lset"
  (test #t (lset<= eq? '(a) '(a b)))
  (test #t (lset= eq? '(a b) '(b a)))
  (test '(c a b) (lset-adjoin eq? '(a b) 'c))
  (test #t (lset= eq? '(a b c) (lset-union eq? '(a b) '(b c))))
  (test #t (lset= eq? '(b) (lset-intersection eq? '(a b) '(b c))))
  (test #t (lset= eq? '(a) (lset-difference eq? '(a b) '(b c))))
  (test #t (lset= eq? '(a c) (lset-xor eq? '(a b) '(b c)))))

(test-end)
(test-exit)
