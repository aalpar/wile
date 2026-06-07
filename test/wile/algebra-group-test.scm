;;; algebra-group-test.scm — Group tests

(import (scheme base)
        (srfi 1)
        (chibi test)
        (wile algebra setoid)
        (wile algebra monoid)
        (wile algebra group))

(test-begin "groups")

(define int-add-group (make-group + 0 -))

(test-group "construction"
  (test #t (group? int-add-group))
  (test #f (group? 42)))

(test-group "operations"
  (test 5  (group-op int-add-group 2 3))
  (test 0  (group-identity int-add-group))
  (test -3 (group-inverse int-add-group 3)))

(test-group "group->monoid"
  (let ((m (group->monoid int-add-group)))
    (test #t (monoid? m))
    (test 0  (monoid-identity m))
    (test 5  (monoid-op m 2 3))
    ;; fold works
    (test 10 (monoid-fold m '(1 2 3 4)))))

(test-group "with-group"
  (test 0 (with-group int-add-group (op identity inverse)
            (op 3 (inverse 3)))))

(test-group "validate-group"
  (test #t (validate-group int-add-group '(-2 -1 0 1 2))))

(test-group "assert-group"
  ;; Valid group — returns (no error).
  (assert-group int-add-group '(-2 -1 0 1 2))
  ;; Broken group — inverse is identity, so a + inverse(a) ≠ 0. Must error.
  (let ((fake (make-group + 0 (lambda (x) x))))
    (test-error (assert-group fake '(1 2 3)))))

;; §5.4 — extended <group> record with optional metadata fields

(test-group "extended <group> — Z/3Z with full metadata"
  (let ((Z3 (make-group
              (lambda (a b) (modulo (+ a b) 3))    ; op
              0                                    ; identity
              (lambda (k) (modulo (- 3 k) 3))      ; inverse
              `(element? . ,integer?)
              `(setoid . ,(numeric-setoid))
              '(order . 3)
              '(elements . (0 1 2))
              '(generators . (1)))))
    (test #t (group? Z3))
    (test 0  (group-identity Z3))
    (test 0  (group-op Z3 1 2))
    (test 2  (group-inverse Z3 1))
    (test #t ((group-element? Z3) 0))
    (test #t (group-equal? Z3 0 0))
    (test #f (group-equal? Z3 0 1))
    (test 3  (group-order Z3))
    (test '(0 1 2) (group-elements Z3))
    (test '(1) (group-generators Z3))
    (test #t (finite-group? Z3))
    (test #t (finitely-generated-group? Z3))))

(test-group "trivial-group"
  (let ((T (trivial-group)))
    (test 1 (group-order T))
    (test #t (finite-group? T))
    (test (group-identity T)
          (group-op T (group-identity T) (group-identity T)))
    (test '() (group-generators T))
    (test '(e) (group-elements T))))

(test-group "cyclic-group"
  (let ((Z5 (cyclic-group 5)))
    (test 5 (group-order Z5))
    (test 1 (group-op Z5 2 4))
    (test 3 (group-inverse Z5 2))
    (test '(1) (group-generators Z5))
    (test #t (finitely-generated-group? Z5))
    (test #t (finite-group? Z5))
    (test '(0 1 2 3 4) (group-elements Z5))))

(test-group "cyclic-group/validation"
  (test-error (cyclic-group 0))
  (test-error (cyclic-group -1))
  (test-error (cyclic-group 'not-an-integer)))

(test-group "symmetric-group"
  (let ((S3 (symmetric-group 3)))
    (test 6 (group-order S3))
    (test #t (equal? #(0 1 2) (group-identity S3)))
    (test #t (not (equal? (group-op S3 #(1 0 2) #(0 2 1))
                          (group-op S3 #(0 2 1) #(1 0 2)))))
    (test #t (every (group-element? S3) (group-elements S3)))
    (test 6 (length (delete-duplicates (group-elements S3) equal?)))
    (let ((p #(2 0 1)))
      (test #t (equal? (group-identity S3)
                       (group-op S3 p (group-inverse S3 p)))))))

(test-group "symmetric-group/edge-cases"
  (test 1 (group-order (symmetric-group 0)))
  (test 1 (group-order (symmetric-group 1)))
  (test 2 (group-order (symmetric-group 2)))
  (test '() (group-generators (symmetric-group 1)))
  (test 1 (length (group-generators (symmetric-group 2))))
  (test 2 (length (group-generators (symmetric-group 3)))))

(test-group "product-group"
  (let ((Z2xZ3 (product-group (cyclic-group 2) (cyclic-group 3))))
    (test 6 (group-order Z2xZ3))
    (test #t (equal? '(0 0) (group-identity Z2xZ3)))
    (test '(1 0) (group-op Z2xZ3 '(1 2) '(0 1)))
    (test #t (every (lambda (e) (and (list? e) (= (length e) 2)))
                    (group-elements Z2xZ3)))
    (test 6 (length (group-elements Z2xZ3)))))

(test-group "product-group/edge-cases"
  (test #t (eq? (trivial-group) (product-group)))
  (let ((Z3 (cyclic-group 3)))
    (test #t (eq? Z3 (product-group Z3))))
  (let ((triple (product-group (cyclic-group 2)
                               (cyclic-group 3)
                               (cyclic-group 5))))
    (test 30 (group-order triple))
    (test #t (equal? '(0 0 0) (group-identity triple)))
    (test 3 (length (group-generators triple)))))

(test-group "subgroup-generated"
  (let ((Z6 (cyclic-group 6)))
    (let ((H (subgroup-generated Z6 '(2))))
      (test 3 (group-order H))
      (test #t (not (not (memv 0 (group-elements H)))))
      (test #t (not (not (memv 2 (group-elements H)))))
      (test #t (not (not (memv 4 (group-elements H))))))))

(test-group "subgroup?"
  (let* ((Z6 (cyclic-group 6))
         (H  (subgroup-generated Z6 '(2))))
    (test #t (subgroup? H Z6))
    ;; Z_5 is not a subgroup of Z_6 — different operation
    (test #f (subgroup? (cyclic-group 5) Z6))))

(test-group "subgroup?/detects op disagreement"
  ;; Construct fake-H with same elements and domain as Z_3 but a
  ;; subtraction-mod-3 operation. Elements-containment passes (both ⊆ {0,1,2})
  ;; but op-agreement fails: (H-op 1 2) = -1 mod 3 = 2, (G-op 1 2) = 0.
  (let* ((Z3 (cyclic-group 3))
         (fake-H (make-group
                   (lambda (a b) (modulo (- a b) 3))
                   0
                   (lambda (k) (modulo (- 3 k) 3))
                   (cons 'element? (lambda (k) (and (integer? k) (<= 0 k) (< k 3))))
                   (cons 'setoid (numeric-setoid))
                   (cons 'order 3)
                   (cons 'elements '(0 1 2))
                   (cons 'generators '(1)))))
    (test #f (subgroup? fake-H Z3))))

(test-group "enumerate-finite-group"
  (let ((Z6-gens (make-group
                   (lambda (a b) (modulo (+ a b) 6))
                   0
                   (lambda (k) (modulo (- 6 k) 6))
                   (cons 'element? (lambda (k) (and (integer? k) (<= 0 k) (< k 6))))
                   (cons 'setoid (numeric-setoid))
                   '(generators . (1)))))
    (test #f (finite-group? Z6-gens))
    (test #t (finitely-generated-group? Z6-gens))
    (let ((Z6 (enumerate-finite-group Z6-gens)))
      (test #t (finite-group? Z6))
      (test 6 (group-order Z6))
      (test #t (every (lambda (k) (not (not (memv k (group-elements Z6)))))
                      (iota 6))))))

(test-group "enumerate-finite-group/idempotent"
  (let* ((Z5  (cyclic-group 5))
         (Z5* (enumerate-finite-group Z5)))
    (test #t (eq? Z5 Z5*))))

(test-group "enumerate-finite-group/max-size-cap"
  (let ((Z100-gens (make-group
                     (lambda (a b) (modulo (+ a b) 100))
                     0
                     (lambda (k) (modulo (- 100 k) 100))
                     (cons 'element? (lambda (k) (and (integer? k) (<= 0 k) (< k 100))))
                     (cons 'setoid (numeric-setoid))
                     '(generators . (1)))))
    (test-error (enumerate-finite-group Z100-gens '(max-size . 10)))))

(test-group "enumerate-finite-group/no-generators"
  (let ((R (make-group + 0 -
                       (cons 'element? real?)
                       (cons 'setoid (numeric-setoid)))))
    (test-error (enumerate-finite-group R))))

(test-group "subgroup-generated on symmetric-group (vector elements)"
  (let* ((S3 (symmetric-group 3))
         (H  (subgroup-generated S3 (list #(1 0 2)))))
    ;; ⟨(0 1)⟩ is a 2-element subgroup
    (test 2 (group-order H))
    (test #t (subgroup? H S3))))

(test-group "make-group/rejects unknown opt keys"
  ;; 'elements? is a typo for 'element? — must error, not silently ignore.
  (test-error (make-group + 0 - (cons 'elements? integer?)))
  ;; 'ordre is a typo for 'order.
  (test-error (make-group + 0 - (cons 'ordre 5)))
  ;; Known keys still work.
  (test #t (group? (make-group + 0 -
                               (cons 'element? integer?)
                               (cons 'order 0)
                               (cons 'elements '())
                               (cons 'generators '())))))

(test-group "make-group/procedure? validation"
  ;; Non-procedure op, inverse must error at construction, not at use site.
  (test-error (make-group 'not-a-proc 0 -))
  (test-error (make-group + 0 'not-a-proc))
  ;; element? if supplied must be a procedure
  (test-error (make-group + 0 - (cons 'element? 'not-a-proc))))

(test-group "make-group-action/procedure? validation"
  ;; Non-procedure set-element?, act must error at construction.
  (let ((G (cyclic-group 3)))
    (test-error (make-group-action G 'not-a-proc (lambda (g x) x)))
    (test-error (make-group-action G integer? 'not-a-proc))))

(test-group "group-action — record and trivial action"
  (let* ((Z3 (cyclic-group 3))
         (A  (trivial-action Z3 integer?)))
    (test #t (group-action? A))
    (test #t (eq? Z3 (group-action-group A)))
    (test 42 (group-action-act A 1 42))
    (test #t ((group-action-set-element? A) 7)))
  ;; make-group-action rejects non-groups
  (test-error (make-group-action 'not-a-group integer? (lambda (g x) x))))

;; Inline permutation action (permutation-action preset lands in Phase 7)
(define (%perm-action G)
  (make-group-action G integer? (lambda (p i) (vector-ref p i))))

(test-group "orbit — S_2 on {0, 1}"
  (let* ((S2 (symmetric-group 2))
         (A  (%perm-action S2))
         (o  (orbit A 0)))
    (test 2 (length o))
    (test #t (not (not (member 0 o))))
    (test #t (not (not (member 1 o))))))

(test-group "orbit-stabilizer theorem on S_3"
  (let* ((S3 (symmetric-group 3))
         (A  (%perm-action S3)))
    (let ((o (orbit A 0))
          (s (stabilizer A 0)))
      ;; |orbit(0)| · |stab(0)| = |S_3| = 6
      (test (group-order S3) (* (length o) (length s))))))

(test-group "orbit on infinite group acting on finite set (Z on Z/12Z)"
  (let* ((Z (make-group + 0 -
                        (cons 'element? integer?)
                        (cons 'setoid (numeric-setoid))
                        '(generators . (1))))
         (Z/12Z? (lambda (x) (and (integer? x) (<= 0 x) (< x 12))))
         (A (make-group-action Z Z/12Z?
                               (lambda (k x) (modulo (+ x k) 12)))))
    (test #f (finite-group? Z))
    (test #t (finitely-generated-group? Z))
    (let ((o (orbit A 0)))
      (test 12 (length o))
      (test #t (every (lambda (k) (not (not (member k o)))) (iota 12))))))

(test-group "fixed-points"
  (let* ((S3 (symmetric-group 3))
         (A  (%perm-action S3)))
    ;; identity fixes all 3 points
    (test 3 (length (fixed-points A #(0 1 2) '(0 1 2))))
    ;; transposition (0 1) fixes only 2
    (test 1 (length (fixed-points A #(1 0 2) '(0 1 2))))
    (test '(2) (fixed-points A #(1 0 2) '(0 1 2)))))

(test-group "orbit errors on unusable groups"
  ;; No generators, no elements → orbit should error
  (let ((G (make-group + 0 -
                       (cons 'element? integer?)
                       (cons 'setoid (numeric-setoid)))))
    (test-error (orbit (make-group-action G integer? (lambda (g x) (+ x g))) 0))))

(test-group "orbit on group with only elements enumeration (no generators)"
  ;; Covers the elts-fallback branch of orbit: G carries elements but not generators.
  (let* ((Z3-elts (make-group
                    (lambda (a b) (modulo (+ a b) 3))
                    0
                    (lambda (k) (modulo (- 3 k) 3))
                    (cons 'element? (lambda (k) (and (integer? k) (<= 0 k) (< k 3))))
                    (cons 'setoid (numeric-setoid))
                    (cons 'order 3)
                    (cons 'elements '(0 1 2))))
         (A (make-group-action Z3-elts integer?
                               (lambda (k x) (modulo (+ x k) 3)))))
    (test #f (group-generators Z3-elts))
    (test #t (finite-group? Z3-elts))
    (let ((o (orbit A 0)))
      (test 3 (length o))
      (test #t (every (lambda (k) (and (member k o) #t)) '(0 1 2))))))

(test-group "orbit-representative"
  ;; S_2 swapping pair components — canonical form is the sorted pair.
  (let* ((S2 (symmetric-group 2))
         (A  (make-group-action
               S2
               (lambda (p) (and (pair? p) (not (pair? (cdr p))) (not (null? (cdr p)))))
               (lambda (perm pr)
                 (if (= (vector-ref perm 0) 0)
                     pr
                     (cons (cdr pr) (car pr))))))
         (pair<? (lambda (p q)
                   (or (< (car p) (car q))
                       (and (= (car p) (car q)) (< (cdr p) (cdr q)))))))
    (test '(1 . 3) (orbit-representative A '(3 . 1) pair<?))
    (test '(1 . 3) (orbit-representative A '(1 . 3) pair<?))))

(test-group "orbit-representative/tie-breaker determinism"
  ;; less? that compares only by car — all elements with car=0 tie.
  (let* ((Z4 (cyclic-group 4))
         (A  (make-group-action
               Z4
               pair?
               (lambda (k p) (cons (car p) (modulo (+ (cdr p) k) 4)))))
         (car<? (lambda (a b) (< (car a) (car b)))))
    (let ((r1 (orbit-representative A '(0 . 0) car<?))
          (r2 (orbit-representative A '(0 . 0) car<?)))
      (test #t (equal? r1 r2)))))

(test-group "burnside-count — 2-colourings of a 4-cycle modulo Z_4"
  (let* ((Z4 (cyclic-group 4))
         ;; Build all 2^4 = 16 binary strings of length 4 as lists.
         (colourings
           (let build ((n 4) (acc '(())))
             (if (= n 0)
                 acc
                 (build (- n 1)
                        (append (map (lambda (c) (cons 0 c)) acc)
                                (map (lambda (c) (cons 1 c)) acc))))))
         (rotate-by
           (lambda (k c)
             (let loop ((i 0) (c c))
               (if (= i k) c
                   (loop (+ i 1) (append (cdr c) (list (car c))))))))
         (A (make-group-action Z4 list? rotate-by)))
    (test 16 (length colourings))
    ;; Classic necklace count: 0000, 0001, 0011, 0101, 0111, 1111
    (test 6 (burnside-count A colourings))))

(test-group "burnside-count errors on non-finite groups"
  ;; G has generators but no elements enumeration — burnside-count needs
  ;; finite G and must say so.
  (let* ((Z (make-group + 0 -
                        (cons 'element? integer?)
                        (cons 'setoid (numeric-setoid))
                        '(generators . (1))))
         (A (make-group-action Z integer? (lambda (k x) (+ x k)))))
    (test #f (finite-group? Z))
    (test-error (burnside-count A '(0 1 2)))))

(test-group "burnside-count detects malformed actions"
  ;; Not a valid action: g=1 maps 3 → 2 (idempotent-ish, not involutive).
  ;; Fixed points: |X^0| = 4, |X^1| = 3 (fixes 0,1,2 but not 3). Sum = 7.
  ;; |G| = 2, so 7/2 is not integer — divisibility check fires.
  (let* ((Z2 (cyclic-group 2))
         (A-bad (make-group-action
                  Z2
                  integer?
                  (lambda (g x)
                    (cond
                      ((= g 0) x)
                      ((< x 3) x)
                      (else (- x 1)))))))
    (test-error (burnside-count A-bad '(0 1 2 3)))))

(test-group "permutation-action on S_3"
  (let ((A (permutation-action (symmetric-group 3) 3)))
    (test 2 (group-action-act A #(2 0 1) 0))
    (test 3 (length (orbit A 0)))))

(test-group "permutation-action/validates n"
  (let ((S3 (symmetric-group 3)))
    (test-error (permutation-action S3 'not-int))
    (test-error (permutation-action S3 -1))
    (test-error (permutation-action S3 0))))

(test-group "symmetric-group/elements enumeration cap"
  ;; For small n (≤ 8), elements is computed eagerly.
  (test 6 (length (group-elements (symmetric-group 3))))
  (test 24 (length (group-elements (symmetric-group 4))))
  ;; Beyond n=8, elements is omitted (n! allocation cost prohibitive).
  ;; Order and generators still present.
  (test #f (group-elements (symmetric-group 9)))
  (test 362880 (group-order (symmetric-group 9)))
  (test 2 (length (group-generators (symmetric-group 9))))
  (test #f (finite-group? (symmetric-group 9)))
  (test #t (finitely-generated-group? (symmetric-group 9))))

(test-group "regular-action on Z_4"
  (let ((A (regular-action (cyclic-group 4))))
    (test 3 (group-action-act A 1 2))
    ;; regular action is transitive: orbit of any element is all of G
    (test 4 (length (orbit A 0)))
    ;; stabilizer of any element is trivial (just identity)
    (test 1 (length (stabilizer A 0)))))

(test-group "regular-action/requires element? or elements"
  ;; Bare 3-arg group has neither — regular-action must not silently
  ;; construct a liar predicate that accepts anything.
  (let ((G (make-group + 0 -)))
    (test-error (regular-action G))))

(test-group "regular-action/derives predicate from elements"
  ;; Group carries elements but not element? — derive membership from elements.
  (let* ((Z3 (make-group
               (lambda (a b) (modulo (+ a b) 3)) 0
               (lambda (k) (modulo (- 3 k) 3))
               (cons 'setoid (numeric-setoid))
               (cons 'order 3)
               (cons 'elements '(0 1 2))))
         (A (regular-action Z3)))
    (test #t ((group-action-set-element? A) 0))
    (test #t ((group-action-set-element? A) 2))
    (test #f ((group-action-set-element? A) 42))
    (test #f ((group-action-set-element? A) 'bogus))))

(test-group "conjugation-action/requires element? or elements"
  (let ((G (make-group + 0 -)))
    (test-error (conjugation-action G))))

(test-group "conjugation-action/derives predicate from elements"
  (let* ((Z3 (make-group
               (lambda (a b) (modulo (+ a b) 3)) 0
               (lambda (k) (modulo (- 3 k) 3))
               (cons 'setoid (numeric-setoid))
               (cons 'order 3)
               (cons 'elements '(0 1 2))))
         (A (conjugation-action Z3)))
    (test #t ((group-action-set-element? A) 1))
    (test #f ((group-action-set-element? A) 'bogus))))

(test-group "conjugation-action on S_3"
  (let ((A (conjugation-action (symmetric-group 3))))
    ;; S_3 has 3 conjugacy classes: {id}, 3 transpositions, 2 three-cycles.
    ;; Orbit of id = {id}.
    (test 1 (length (orbit A #(0 1 2))))
    ;; Orbit of a transposition = all 3 transpositions.
    (test 3 (length (orbit A #(1 0 2))))
    ;; Burnside on conjugation = number of conjugacy classes.
    (test 3 (burnside-count A (group-elements (symmetric-group 3))))))

(test-group "product-action"
  (let* ((A2 (permutation-action (symmetric-group 2) 2))
         (A3 (permutation-action (symmetric-group 3) 3))
         (A  (product-action A2 A3)))
    (test #t (group-action? A))
    (test 12 (group-order (group-action-group A)))
    (let ((result (group-action-act A (list #(1 0) #(2 0 1)) '(0 0))))
      (test #t (list? result))
      (test 2 (length result)))))

(test-group "product-action/componentwise value"
  ;; Verify the action actually composes componentwise: not just shape.
  ;; S_2 on {0,1} with p = (0 1) sends 0 → 1.
  ;; S_3 on {0,1,2} with q = (2 0 1) sends 1 → 0.
  ;; Product acting on (0, 1): (group-action-act A (list p q) '(0 1)) = (1 0).
  (let* ((A2 (permutation-action (symmetric-group 2) 2))
         (A3 (permutation-action (symmetric-group 3) 3))
         (A  (product-action A2 A3)))
    (test '(1 0) (group-action-act A (list #(1 0) #(2 0 1)) '(0 1)))
    (test '(0 1) (group-action-act A (list #(0 1) #(0 1 2)) '(0 1)))))

(test-group "product-action edge cases"
  (let ((A (permutation-action (symmetric-group 3) 3)))
    (test #t (eq? A (product-action A)))
    (test #t (group-action? (product-action)))))

(test-group "product-action/empty — predicate accepts trivial-group's element"
  ;; (product-action) should act on trivial-group, whose sole element is 'e.
  ;; The set-element? predicate must accept 'e, not a different sentinel.
  (let ((A (product-action)))
    (test #t (eq? (trivial-group) (group-action-group A)))
    (test #t ((group-action-set-element? A) 'e))
    (test 'e (group-action-act A 'e 'e))))

;; Action axiom checker: verifies (unit) and (compatibility) directly.
;;   unit:          (act identity x) = x  for all x ∈ samples
;;   compatibility: (act (op g h) x) = (act g (act h x))  for all g,h ∈ G-elts, x ∈ samples
;; Returns #t on success, or a descriptor list on first violation (lazy-ish).
(define (%action-laws-hold? A G-elts samples)
  (let* ((G   (group-action-group A))
         (act (group-action-act-fn A))
         (e   (group-identity G))
         (op  (lambda (g h) (group-op G g h))))
    (and (every (lambda (x) (equal? x (act e x))) samples)
         (every (lambda (g)
                  (every (lambda (h)
                           (every (lambda (x)
                                    (equal? (act (op g h) x)
                                            (act g (act h x))))
                                  samples))
                         G-elts))
                G-elts))))

(test-group "action laws — trivial-action"
  (let* ((G (cyclic-group 3))
         (A (trivial-action G integer?)))
    (test #t (%action-laws-hold? A (group-elements G) '(0 7 42)))))

(test-group "action laws — permutation-action on S_3"
  (let* ((S3 (symmetric-group 3))
         (A  (permutation-action S3 3)))
    (test #t (%action-laws-hold? A (group-elements S3) (iota 3)))))

(test-group "action laws — regular-action on Z_4"
  (let* ((Z4 (cyclic-group 4))
         (A  (regular-action Z4)))
    (test #t (%action-laws-hold? A (group-elements Z4) (group-elements Z4)))))

(test-group "action laws — conjugation-action on S_3"
  (let* ((S3 (symmetric-group 3))
         (A  (conjugation-action S3)))
    (test #t (%action-laws-hold? A (group-elements S3) (group-elements S3)))))

(test-group "action laws — product-action on S_2 × S_3"
  (let* ((A2 (permutation-action (symmetric-group 2) 2))
         (A3 (permutation-action (symmetric-group 3) 3))
         (A  (product-action A2 A3))
         (G-elts (group-elements (group-action-group A)))
         (samples (list '(0 0) '(0 1) '(0 2) '(1 0) '(1 1) '(1 2))))
    (test #t (%action-laws-hold? A G-elts samples))))

(test-group "backward compatibility — 3-arg make-group"
  (let ((Z (make-group + 0 -)))
    (test #t (group? Z))
    (test 0  (group-identity Z))
    (test 5  (group-op Z 2 3))
    (test -3 (group-inverse Z 3))
    (test #f (group-element? Z))
    (test #f (group-order Z))
    (test #f (group-elements Z))
    (test #f (group-generators Z))
    (test #t (setoid? (group-setoid Z)))
    (test #f (finite-group? Z))
    (test #f (finitely-generated-group? Z))))

(test-group "subgroup-generated / orbit — infinite-group cap (no silent hang)"
  ;; Z (integers under +) is infinite yet finitely generated by 1, so the BFS
  ;; closure would loop forever. The same cap-check fires whether the bound is
  ;; an explicit (max-size . N) or the large default, so an explicit small N
  ;; exercises the guard quickly. Before the cap these calls hung silently.
  (let* ((Z  (make-group + 0 - '(generators . (1))))
         (AZ (make-group-action Z integer? (lambda (g x) (+ g x)))))
    (test-error (subgroup-generated Z '(1) '(max-size . 100)))
    (test-error (enumerate-finite-group Z '(max-size . 100)))
    (test-error (orbit AZ 0 '(max-size . 100)))
    ;; A finite orbit of an infinite group (Z acting on Z/12Z) still enumerates.
    (let ((A12 (make-group-action Z integer? (lambda (g x) (modulo (+ g x) 12)))))
      (test 12 (length (orbit A12 0))))))

(test-end)
(test-exit)
