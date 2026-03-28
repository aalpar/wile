;; fold.scm -- list fold/reduce utilities
;; Copyright (c) 2009-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (fold kons knil ls . lists)
  "Accumulate across LS by applying KONS to each element and\nthe running accumulator, starting from KNIL. For a single list,\ncomputes (kons eN ... (kons e1 knil)) left-to-right.\nFor multiple lists, KONS receives one element from each list\nplus the accumulator. Stops at the shortest list."
  (if (null? lists)
      (let lp ((ls ls) (acc knil))
        (if (pair? ls) (lp (cdr ls) (kons (car ls) acc)) acc))
      (let lp ((lists (cons ls lists)) (acc knil))
        (if (every pair? lists)
            (lp (map cdr lists) (apply kons (map-onto car lists (list acc))))
            acc))))

(define (fold-right kons knil ls . lists)
  "Accumulate across LS right-to-left by applying KONS to each\nelement and the recursive result, with KNIL as the base case.\nFor a single list, computes (kons e1 (kons e2 ... knil)).\nFor multiple lists, stops at the shortest list."
  (if (null? lists)
      (let lp ((ls ls))
        (if (pair? ls) (kons (car ls) (lp (cdr ls))) knil))
      (let lp ((lists (cons ls lists)))
        (if (every pair? lists)
            (apply kons (map-onto car lists (list (lp (map cdr lists)))))
            knil))))

(define (pair-fold kons knil ls . lists)
  "Like fold, but KONS receives the successive tail pairs of LS\nrather than the elements. For a single list, applies\n(kons pair acc) for each pair left-to-right.\nFor multiple lists, KONS receives each tail pair from every\nlist plus the accumulator."
  (if (null? lists)
      (let lp ((ls ls) (acc knil))
        (if (pair? ls) (lp (cdr ls) (kons ls acc)) acc))
      (let lp ((lists (cons ls lists)) (acc knil))
        (if (every pair? lists)
            (lp (map cdr lists) (apply kons (append lists (list acc))))
            acc))))

(define (pair-fold-right kons knil ls . lists)
  "Like fold-right, but KONS receives the successive tail pairs\nof LS rather than the elements. Processes pairs right-to-left\nwith KNIL as the base case."
  (if (null? lists)
      (let lp ((ls ls))
        (if (pair? ls) (kons ls (lp (cdr ls))) knil))
      (let lp ((lists (cons ls lists)))
        (if (every pair? lists)
            (apply kons (append lists (list (lp (map cdr lists)))))
            knil))))

(define (reduce f identity ls)
  "Reduce LS using binary function F with IDENTITY as the\ndefault for the empty list. For non-empty lists, uses the\nfirst element as the initial accumulator and folds over the\nrest, so F need not be applied to IDENTITY."
  (if (null? ls) identity (fold f (car ls) (cdr ls))))

(define (reduce-right f identity ls)
  "Right-associative variant of reduce. For non-empty lists,\nfolds right using F with IDENTITY as the base. Returns\nIDENTITY for the empty list."
  (if (null? ls) identity (fold-right f identity ls)))

(define (unfold p f g seed . o)
  "Build a list by iterating from SEED. P is the stop predicate,\nF maps the seed to a list element, G produces the next seed.\nWhen P returns true, the optional tail-gen argument (default '())\nis called on the final seed to produce the tail.\n(unfold null? car cdr lis) copies a list."
  (let lp ((seed seed))
    (if (p seed)
        (if (pair? o) ((car o) seed) '())
        (cons (f seed) (lp (g seed))))))

(define (unfold-right p f g seed . o)
  "Build a list right-to-left by iterating from SEED. P is the\nstop predicate, F maps seed to a list element, G produces the\nnext seed. The optional TAIL argument (default '()) is the\ninitial tail. Elements are consed onto the tail as iteration\nproceeds, producing a reversed accumulation."
  (let lp ((seed seed) (res (if (pair? o) (car o) '())))
    (if (p seed) res (lp (g seed) (cons (f seed) res)))))

(define (append-map-helper append f ls lists)
  "Internal helper for append-map and append-map!. Uses the\ngiven APPEND procedure to concatenate the results of applying\nF to elements of LS and LISTS."
  (if (null? lists)
      (if (null? ls)
          '()
          (let lp ((ls (reverse ls)) (res '()))
            (if (null? ls) res (lp (cdr ls) (append (f (car ls)) res)))))
      (if (and (pair? ls) (every pair? lists))
          (let lp ((lists (cons ls lists)))
            (let ((vals (apply f (map car lists)))
                  (cdrs (map cdr lists)))
              (if (every pair? cdrs) (append vals (lp cdrs)) vals)))
          '())))

(define (append-map f ls . lists)
  "Apply F to each element of LS (and corresponding elements of\nadditional LISTS) and append the resulting lists together.\nEquivalent to (concatenate (map f ls)) but may be more\nefficient."
  (append-map-helper append f ls lists))

(define (append-map! f ls . lists)
  "Linear-update variant of append-map. May destructively\nappend the result lists produced by F."
  (append-map-helper append! f ls lists))

(define map! map)
(define map-in-order map)

(define (pair-for-each f ls . lists)
  "Apply F to each successive tail pair of LS for its side effects.\nLike for-each, but passes the pair (not the element) to F.\nFor multiple lists, passes corresponding tail pairs."
  (if (pair? lists)
      (apply pair-fold (lambda args (apply f (drop-right args 1))) #f ls lists)
      (pair-fold (lambda (x _) (f x)) #f ls)))

(define (filter-map f ls . lists)
  "Apply F to each element of LS and collect the true (non-#f)\nresults into a list. Like (filter values (map f ls)) but\nmore efficient, avoiding the intermediate list.\nFor multiple lists, applies F to corresponding elements."
  (if (null? lists)
      (let lp ((ls ls) (res '()))
        (if (pair? ls)
            (let ((x (f (car ls)))) (lp (cdr ls) (if x (cons x res) res)))
            (reverse res)))
      (filter (lambda (x) x) (apply map f ls lists))))

(define (take-up-to-reverse from to init)
  "Internal helper: copy elements from list FROM up to (but not\nincluding) the pair TO, consing them in reverse onto INIT."
  (if (eq? from to)
      init
      (take-up-to-reverse (cdr from) to (cons (car from) init))))

(define (remove pred ls)
  "Return a list containing every element of LS that does not\nsatisfy PRED. The result shares the longest trailing sublist\nof LS where no element satisfies PRED. Preserves element order."
  (let lp ((ls ls) (rev '()))
    (let ((tail (find-tail pred ls)))
      (if tail
          (lp (cdr tail) (take-up-to-reverse ls tail rev))
          (if (pair? rev) (append-reverse rev ls) ls)))))

(define (filter pred ls)
  "Return a list containing every element of LS that satisfies\nPRED. Preserves element order. The complement of remove."
  (remove (lambda (x) (not (pred x))) ls))

(define (partition pred ls)
  "Partition LS into two lists: elements satisfying PRED and\nelements that do not. Returns two values preserving the\noriginal order within each partition."
  (let lp ((ls ls) (good '()) (bad '()))
    (cond ((null? ls) (values (reverse! good) (reverse! bad)))
          ((pred (car ls)) (lp (cdr ls) (cons (car ls) good) bad))
          (else (lp (cdr ls) good (cons (car ls) bad))))))

(define filter! filter)
(define remove! remove)
(define partition! partition)

