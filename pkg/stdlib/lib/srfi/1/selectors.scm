;; selectors.scm -- extended list selectors
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define first car)
(define second cadr)
(define (third ls)
  "Return the third element of LS.\n\nExamples:\n  (third '(a b c d))  => c\n\nParameters:\n  ls : list\nReturns: any\nCategory: srfi-1"
  (car (cdr (cdr ls))))
(define (fourth ls)
  "Return the fourth element of LS.\n\nExamples:\n  (fourth '(a b c d e))  => d\n\nParameters:\n  ls : list\nReturns: any\nCategory: srfi-1"
  (car (cdr (cdr (cdr ls)))))
(define (fifth ls)
  "Return the fifth element of LS.\n\nExamples:\n  (fifth '(a b c d e))  => e\n\nParameters:\n  ls : list\nReturns: any\nCategory: srfi-1"
  (car (cdr (cdr (cdr (cdr ls))))))
(define (sixth ls)
  "Return the sixth element of LS.\n\nExamples:\n  (sixth (iota 10))  => 5\n\nParameters:\n  ls : list\nReturns: any\nCategory: srfi-1"
  (car (cdr (cdr (cdr (cdr (cdr ls)))))))
(define (seventh ls)
  "Return the seventh element of LS.\n\nExamples:\n  (seventh (iota 10))  => 6\n\nParameters:\n  ls : list\nReturns: any\nCategory: srfi-1"
  (car (cdr (cdr (cdr (cdr (cdr (cdr ls))))))))
(define (eighth ls)
  "Return the eighth element of LS.\n\nExamples:\n  (eighth (iota 10))  => 7\n\nParameters:\n  ls : list\nReturns: any\nCategory: srfi-1"
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr ls)))))))))
(define (ninth ls)
  "Return the ninth element of LS.\n\nExamples:\n  (ninth (iota 10))  => 8\n\nParameters:\n  ls : list\nReturns: any\nCategory: srfi-1"
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr ls))))))))))
(define (tenth ls)
  "Return the tenth element of LS.\n\nExamples:\n  (tenth (iota 10))  => 9\n\nParameters:\n  ls : list\nReturns: any\nCategory: srfi-1"
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr ls)))))))))))

(define (car+cdr x)
  "Return two values: the car and cdr of pair X.\n\nExamples:\n  (car+cdr '(a b c))  => a (b c)\n\nParameters:\n  x : pair\nReturns: any\nCategory: srfi-1"
  (values (car x) (cdr x)))

(define (take ls i)
  "Return a freshly allocated list of the first I elements of LS.\nIt is an error if LS has fewer than I elements.\n\nExamples:\n  (take '(a b c d e) 3)  => (a b c)\n  (take '(1 2 3) 0)      => ()\n\nParameters:\n  ls : list\n  i : integer\nReturns: list\nCategory: srfi-1\nKeywords: first n, prefix, head, slice\n\nSee also: `drop', `split-at', `take-right'."
  (let lp ((i i) (ls ls) (res '()))
    (if (<= i 0)
        (reverse! res)
        (lp (- i 1) (cdr ls) (cons (car ls) res)))))

(define (take! ls i)
  "Linear-update variant of take. May mutate LS to produce\nthe first I elements. The result may or may not share\nstructure with LS.\n\nExamples:\n  (take! (list 1 2 3 4) 2)  => (1 2)\n\nParameters:\n  ls : list\n  i : integer\nReturns: list\nCategory: srfi-1\n\nSee also: `take', `drop', `split-at!'."
  (if (<= i 0)
      '()
      (let ((tail (list-tail ls (- i 1))))
        (set-cdr! tail '())
        ls)))

(define (drop ls i)
  "Return the tail of LS after skipping the first I elements.\nShares structure with the tail of LS.\n\nExamples:\n  (drop '(a b c d e) 2)  => (c d e)\n  (drop '(1 2 3) 0)      => (1 2 3)\n\nParameters:\n  ls : list\n  i : integer\nReturns: list\nCategory: srfi-1\nKeywords: skip, tail from, remove prefix\n\nSee also: `take', `split-at', `drop-right'."
  (if (<= i 0) ls (drop (cdr ls) (- i 1))))

(define (take-right ls i)
  "Return the last I elements of LS. Shares structure with\nthe tail of LS.\n\nExamples:\n  (take-right '(a b c d e) 2)  => (d e)\n  (take-right '(1 2 3) 0)      => ()\n\nParameters:\n  ls : list\n  i : integer\nReturns: list\nCategory: srfi-1\n\nSee also: `take', `drop-right'."
  (drop ls (- (length+ ls) i)))

(define (drop-right ls i)
  "Return a freshly allocated list containing all but the last\nI elements of LS.\n\nExamples:\n  (drop-right '(a b c d e) 2)  => (a b c)\n  (drop-right '(1 2 3) 0)      => (1 2 3)\n\nParameters:\n  ls : list\n  i : integer\nReturns: list\nCategory: srfi-1\n\nSee also: `drop', `take-right'."
  (take ls (- (length+ ls) i)))

(define (drop-right! ls i)
  "Linear-update variant of drop-right. May mutate LS to\nremove the last I elements.\n\nExamples:\n  (drop-right! (list 1 2 3 4) 2)  => (1 2)\n\nParameters:\n  ls : list\n  i : integer\nReturns: list\nCategory: srfi-1\n\nSee also: `drop-right', `take!'."
  (take! ls (- (length+ ls) i)))

(define (split-at ls i)
  "Split LS at index I, returning two values: a freshly allocated\nlist of the first I elements and the remaining tail. Equivalent\nto (values (take ls i) (drop ls i)) but traverses only once.\n\nExamples:\n  (split-at '(a b c d e) 3)  => (a b c) (d e)\n\nParameters:\n  ls : list\n  i : integer\nReturns: list\nCategory: srfi-1\n\nSee also: `take', `drop'."
  (let lp ((i i) (ls ls) (res '()))
    (if (<= i 0)
        (values (reverse! res) ls)
        (lp (- i 1) (cdr ls) (cons (car ls) res)))))

(define (split-at! ls i)
  "Linear-update variant of split-at. May mutate LS to split\nat index I. Returns two values: the prefix and the suffix.\n\nExamples:\n  (split-at! (list 1 2 3 4 5) 2)  => (1 2) (3 4 5)\n\nParameters:\n  ls : list\n  i : integer\nReturns: list\nCategory: srfi-1"
  (if (<= i 0)
      (values '() ls)
      (let* ((tail (list-tail ls (- i 1)))
             (right (cdr tail)))
        (set-cdr! tail '())
        (values ls right))))

(define (last-pair ls)
  "Return the last pair of non-empty list LS. Terminates on the\nfirst pair whose cdr is not itself a pair, so dotted (improper)\nlists are handled per SRFI-1.\n\nExamples:\n  (last-pair '(a b c))    => (c)\n  (last-pair '(a b . c))  => (b . c)\n\nParameters:\n  ls : pair\nReturns: pair\nCategory: srfi-1"
  (if (pair? (cdr ls)) (last-pair (cdr ls)) ls))
(define (last ls)
  "Return the last element of non-empty list LS. For a dotted\n(improper) list, this is the car of the last pair (the final\nproper element), not the dotted tail.\n\nExamples:\n  (last '(a b c))    => c\n  (last '(a b . c))  => b\n\nParameters:\n  ls : pair\nReturns: any\nCategory: srfi-1"
  (car (last-pair ls)))

