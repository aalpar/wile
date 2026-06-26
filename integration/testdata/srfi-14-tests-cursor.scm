;; SRFI-14 Phase 6: cursor protocol, char-set-hash, char-set-diff+intersection.
;; Covers the low-level cursor iteration interface (char-set-cursor,
;; char-set-ref, char-set-cursor-next, end-of-char-set?), the stable bounded
;; hash, and the combined difference/intersection multiple-values procedure.
;; Includes the surrogate-block defensive-skip behavior exercised by the
;; cursor when walking ranges that straddle U+D800..U+DFFF.

(import (scheme base) (chibi test) (srfi 14))

(test-begin "srfi-14-cursor")

;; Collect every character a cursor walks, as a string.
(define (cursor->string cs)
  (let loop ((cur (char-set-cursor cs)) (acc '()))
    (if (end-of-char-set? cur)
        (list->string (reverse acc))
        (loop (char-set-cursor-next cs cur)
              (cons (char-set-ref cs cur) acc)))))

;; Count via cursor (independent of char-set-size).
(define (cursor-count cs)
  (let loop ((cur (char-set-cursor cs)) (n 0))
    (if (end-of-char-set? cur)
        n
        (loop (char-set-cursor-next cs cur) (+ n 1)))))

(test-group "cursor walk"
  ;; Walks all members exactly once, in ascending order.
  (test "abc" (cursor->string (char-set #\a #\b #\c)))
  ;; Out-of-order construction still walks in ascending order.
  (test "abc" (cursor->string (char-set #\c #\a #\b)))
  ;; Duplicates collapse (set semantics).
  (test "a"   (cursor->string (char-set #\a #\a)))
  ;; Multi-range set: digits then letters.
  (test "09az" (cursor->string (char-set #\0 #\9 #\a #\z)))
  ;; Cursor count matches char-set-size.
  (test 3 (cursor-count (char-set #\a #\b #\c)))
  (test (char-set-size (char-set #\a #\b #\c))
        (cursor-count (char-set #\a #\b #\c))))

(test-group "empty-set cursor"
  (test #t (end-of-char-set? (char-set-cursor (char-set))))
  (test "" (cursor->string (char-set))))

(test-group "cursor surrogate-block skip"
  ;; A half-open ucs range straddling the surrogate block. Inclusive members
  ;; are D7FE, D7FF, E000, E001 -- the surrogate block D800..DFFF is skipped.
  (test 4 (cursor-count (ucs-range->char-set #xD7FE #xE002)))
  (test '(55294 55295 57344 57345)
        (map char->integer (string->list (cursor->string (ucs-range->char-set #xD7FE #xE002)))))
  ;; A range entirely inside the surrogate block has no walkable members.
  (test #t (end-of-char-set? (char-set-cursor (ucs-range->char-set #xD800 #xDC00 #f))))
  ;; Walking char-set:full does not raise on the surrogate block; first member
  ;; is U+0000.
  (test 0 (char->integer (char-set-ref char-set:full (char-set-cursor char-set:full)))))

(test-group "char-set-hash stability and bound"
  ;; Content-determined: char-set= sets hash equal regardless of construction.
  (test #t (= (char-set-hash (char-set #\a #\b))
              (char-set-hash (char-set #\b #\a))))
  (test #t (= (char-set-hash (char-set #\a #\b #\c))
              (char-set-hash (string->char-set "cba"))))
  ;; Distinct sets (very likely) hash differently -- sanity, not guaranteed.
  (test #t (not (= (char-set-hash (char-set #\a))
                   (char-set-hash (char-set #\b)))))
  ;; Bounded: result is in [0, bound).
  (test #t (let ((h (char-set-hash char-set:full 100))) (and (>= h 0) (< h 100))))
  (test #t (let ((h (char-set-hash char-set:letter 7))) (and (>= h 0) (< h 7))))
  ;; Default bound yields a non-negative exact integer.
  (test #t (let ((h (char-set-hash char-set:digit))) (and (exact-integer? h) (>= h 0))))
  ;; Empty and full are both hashable.
  (test #t (exact-integer? (char-set-hash (char-set))))
  (test #t (exact-integer? (char-set-hash char-set:full))))

(test-group "char-set-diff+intersection"
  ;; Returns two values: difference then intersection.
  (test '("a" "bc")
        (call-with-values
          (lambda () (char-set-diff+intersection (char-set #\a #\b #\c)
                                                 (char-set #\b #\c #\d)))
          (lambda (diff inter)
            (list (char-set->string diff) (char-set->string inter)))))
  ;; Agrees with calling difference / intersection separately.
  (test #t
        (call-with-values
          (lambda () (char-set-diff+intersection (char-set #\a #\b #\c #\d)
                                                 (char-set #\b #\d)))
          (lambda (diff inter)
            (and (char-set= diff (char-set-difference (char-set #\a #\b #\c #\d)
                                                      (char-set #\b #\d)))
                 (char-set= inter (char-set-intersection (char-set #\a #\b #\c #\d)
                                                         (char-set #\b #\d)))))))
  ;; Multiple other char-sets fold via union.
  (test '("a" "bc")
        (call-with-values
          (lambda () (char-set-diff+intersection (char-set #\a #\b #\c)
                                                 (char-set #\b)
                                                 (char-set #\c #\d)))
          (lambda (diff inter)
            (list (char-set->string diff) (char-set->string inter)))))
  ;; ! alias behaves identically.
  (test '("a" "bc")
        (call-with-values
          (lambda () (char-set-diff+intersection! (char-set #\a #\b #\c)
                                                  (char-set #\b #\c #\d)))
          (lambda (diff inter)
            (list (char-set->string diff) (char-set->string inter))))))

(test-end "srfi-14-cursor")

(test-exit)
