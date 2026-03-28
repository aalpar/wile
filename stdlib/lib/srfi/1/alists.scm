;; alist.scm -- association list utilities
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (alist-cons key value ls)
  "Cons a new association (KEY . VALUE) onto the front of\nassociation list LS. Equivalent to (cons (cons key value) ls)."
  (cons (cons key value) ls))

(define (alist-copy ls)
  "Return a fresh copy of association list LS. Each association\npair is freshly allocated, but keys and values are shared."
  (map (lambda (x) (cons (car x) (cdr x))) ls))

(define (alist-delete key ls . o)
  "Remove all associations from LS whose key is equal to KEY.\nThe optional third argument is the equality predicate,\ndefaulting to equal?."
  (let ((eq (if (pair? o) (car o) equal?)))
    (remove (lambda (x) (eq key (car x))) ls)))

(define alist-delete! alist-delete)

