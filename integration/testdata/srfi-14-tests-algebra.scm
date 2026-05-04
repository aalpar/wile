;; SRFI-14 Phase 2: set algebra integration tests.
;; Covers all binary set algebra ops, !-aliases, derived adjoin/delete,
;; and key algebraic identities (commutativity, associativity, complement
;; involution, DeMorgan).

(import (scheme base) (chibi test) (srfi 14))

(test-begin "srfi-14-algebra")

(test-group "binary set algebra"
  (test "ab" (char-set->string (char-set-union (char-set #\a) (char-set #\b))))
  (test "b"  (char-set->string (char-set-intersection (char-set #\a #\b) (char-set #\b #\c))))
  (test "a"  (char-set->string (char-set-difference  (char-set #\a #\b) (char-set #\b))))
  (test "ac" (char-set->string (char-set-xor          (char-set #\a #\b) (char-set #\b #\c)))))

(test-group "complement"
  (test #t (char-set-contains? (char-set-complement (char-set #\a)) #\b))
  (test #f (char-set-contains? (char-set-complement (char-set #\a)) #\a)))

(test-group "char-set-adjoin / delete"
  (test "ab"  (char-set->string (char-set-adjoin (char-set #\a) #\b)))
  (test "b"   (char-set->string (char-set-delete (char-set #\a #\b) #\a)))
  (test "abc" (char-set->string (char-set-adjoin (char-set #\a) #\b #\c)))
  (test "a"   (char-set->string (char-set-delete (char-set #\a #\b #\c) #\b #\c))))

(test-group "! aliases (always allocate fresh per design Q2-A)"
  (test "ab" (char-set->string (char-set-adjoin! (char-set #\a) #\b)))
  (test "b"  (char-set->string (char-set-delete! (char-set #\a #\b) #\a)))
  (test "ab" (char-set->string (char-set-union! (char-set #\a) (char-set #\b))))
  (test "b"  (char-set->string (char-set-intersection! (char-set #\a #\b) (char-set #\b #\c))))
  (test "a"  (char-set->string (char-set-difference! (char-set #\a #\b) (char-set #\b))))
  (test "ac" (char-set->string (char-set-xor! (char-set #\a #\b) (char-set #\b #\c))))
  (test #t (char-set-contains? (char-set-complement! (char-set #\a)) #\b)))

(test-group "algebraic identities"
  ;; Commutativity of union
  (test #t (char-set= (char-set-union (char-set #\a) (char-set #\b))
                      (char-set-union (char-set #\b) (char-set #\a))))
  ;; Associativity of union
  (test #t (char-set= (char-set-union (char-set-union (char-set #\a) (char-set #\b)) (char-set #\c))
                      (char-set-union (char-set #\a) (char-set-union (char-set #\b) (char-set #\c)))))
  ;; Complement involution
  (test #t (char-set= (char-set #\a)
                      (char-set-complement (char-set-complement (char-set #\a)))))
  ;; DeMorgan
  (test #t (char-set= (char-set-complement (char-set-union (char-set #\a) (char-set #\b)))
                      (char-set-intersection (char-set-complement (char-set #\a))
                                             (char-set-complement (char-set #\b))))))

(test-end "srfi-14-algebra")

(test-exit)
