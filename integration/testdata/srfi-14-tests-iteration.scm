;; SRFI-14 Phase 3: iteration / query layer integration tests.
;; Covers char-set-fold, char-set-for-each, char-set-map, char-set-filter,
;; char-set-filter!, char-set-count, char-set-every, char-set-any,
;; char-set-unfold, char-set-unfold!.

(import (scheme base) (chibi test) (srfi 14))

(test-begin "srfi-14-iteration")

(test-group "char-set-fold"
  ;; Sum codepoints
  (test (+ 97 98 99)
        (char-set-fold (lambda (ch acc) (+ acc (char->integer ch)))
                       0 (char-set #\a #\b #\c)))
  ;; Empty
  (test 'init (char-set-fold (lambda (c acc) c) 'init (char-set))))

(test-group "char-set-for-each"
  ;; Side effect: collect into list (reverse-cons gives reversed-ascending)
  (test '(#\c #\b #\a)
        (let ((collected '()))
          (char-set-for-each (lambda (c) (set! collected (cons c collected)))
                             (char-set #\c #\a #\b))
          collected))
  ;; Empty
  (test #f
        (let ((called #f))
          (char-set-for-each (lambda (c) (set! called #t)) (char-set))
          called)))

(test-group "char-set-map"
  ;; Map codepoints through a function (returns char-set, not list)
  (test #t (char-set= (char-set #\b #\c #\d)
                      (char-set-map (lambda (c) (integer->char (+ 1 (char->integer c))))
                                    (char-set #\a #\b #\c)))))

(test-group "char-set-filter"
  (test #t (char-set= (char-set #\b)
                      (char-set-filter (lambda (c) (char=? c #\b))
                                       (char-set #\a #\b #\c)))))

(test-group "char-set-filter!"
  (test #t (char-set= (char-set #\b)
                      (char-set-filter! (lambda (c) (char=? c #\b))
                                        (char-set #\a #\b #\c)))))

(test-group "char-set-count"
  (test 0 (char-set-count (lambda (c) (char=? c #\z)) (char-set #\a #\b #\c)))
  (test 1 (char-set-count (lambda (c) (char=? c #\b)) (char-set #\a #\b #\c)))
  (test 3 (char-set-count (lambda (c) #t) (char-set #\a #\b #\c))))

(test-group "char-set-every / any short-circuit"
  (test #t (char-set-every (lambda (c) #t) (char-set #\a #\b)))
  (test #f (char-set-every (lambda (c) (char=? c #\a)) (char-set #\a #\b)))
  (test #t (char-set-any  (lambda (c) (char=? c #\b)) (char-set #\a #\b #\c)))
  (test #f (char-set-any  (lambda (c) (char=? c #\z)) (char-set #\a #\b #\c)))
  ;; Vacuous: every on empty = #t, any on empty = #f
  (test #t (char-set-every (lambda (c) #f) (char-set)))
  (test #f (char-set-any  (lambda (c) #t) (char-set))))

(test-group "char-set-unfold"
  ;; Unfold codepoints 65..69 (A..E)
  (test #t
    (char-set= (char-set #\A #\B #\C #\D #\E)
               (char-set-unfold
                 (lambda (n) (integer->char n))                 ; mapper
                 (lambda (n) (= n 70))                          ; stop?
                 (lambda (n) (+ 1 n))                           ; successor
                 65)))                                          ; seed

  ;; ! alias
  (test #t
    (char-set= (char-set #\A)
               (char-set-unfold!
                 integer->char (lambda (n) (= n 66)) (lambda (n) (+ 1 n)) 65))))

(test-end "srfi-14-iteration")

(test-exit)
