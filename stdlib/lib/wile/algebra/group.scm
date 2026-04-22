;;; (wile algebra group) — Groups
;;;
;;; A group (G, ⊕, e, ⁻¹) is a monoid with inverses:
;;; a ⊕ a⁻¹ = a⁻¹ ⊕ a = e.
;;;
;;; The <group> record carries the three mandatory fields (op, identity,
;;; inverse) and five optional metadata fields (element?, setoid, order,
;;; elements, generators) used by later §5.4 machinery (subgroup closure,
;;; orbit enumeration, Burnside counting). Absent optional fields default
;;; to #f except setoid, which defaults to (default-setoid) wrapping
;;; R7RS equal?.

(define-record-type <group>
  (%make-group op-fn identity inverse-fn
               element? setoid order elements generators)
  group?
  (op-fn       group-op-fn)
  (identity    group-identity)
  (inverse-fn  group-inverse-fn)
  (element?    group-element?)
  (setoid      group-setoid)
  (order       group-order)
  (elements    group-elements)
  (generators  group-generators))

(define (%assv-or opts key fallback)
  (let ((p (assv key opts)))
    (if p (cdr p) fallback)))

(define (make-group op identity inverse . opts)
  "Construct a group from binary operation OP, IDENTITY element, and INVERSE function.\nOP must be associative, IDENTITY must be neutral for OP, and\nINVERSE must return an element such that OP of any element\nwith its inverse yields IDENTITY.\n\nOptional trailing alist entries specify extended metadata:\n  (element? . P) — membership predicate\n  (setoid . S) — equivalence relation (defaults to default-setoid)\n  (order . N) — group order (cardinality)\n  (elements . LIST) — full enumeration for finite groups\n  (generators . LIST) — generating set\n\nExamples:\n  (let ((G (make-group + 0 -))) (group-identity G))  => 0\n  (let ((G (make-group * 1 (lambda (x) (/ 1 x)))))\n    (group-op G 3 4))  => 12\n\nParameters:\n  op : procedure\n  identity : any\n  inverse : procedure\n  opts : alist\nReturns: any\nCategory: algebra\nKeywords: group, inverse, abelian, symmetry, algebraic structure\n\nSee also: `group->monoid', `validate-group'."
  (%make-group op identity inverse
               (%assv-or opts 'element?   #f)
               (%assv-or opts 'setoid     (default-setoid))
               (%assv-or opts 'order      #f)
               (%assv-or opts 'elements   #f)
               (%assv-or opts 'generators #f)))

(define (group-equal? G a b)
  "Test A and B for equivalence under group G's setoid.\n\nExamples:\n  (group-equal? (make-group + 0 -) 3 3)  => #t\n\nParameters:\n  G : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\nKeywords: equality, setoid, equivalence"
  (setoid-equiv? (group-setoid G) a b))

(define (finite-group? G)
  "Return #t if group G carries both an order and an elements enumeration.\n\nParameters:\n  G : any\nReturns: boolean\nCategory: algebra\nKeywords: finite, enumerate, cardinality"
  (and (group-order G) (group-elements G) #t))

(define (finitely-generated-group? G)
  "Return #t if group G carries a generating set.\n\nParameters:\n  G : any\nReturns: boolean\nCategory: algebra\nKeywords: finitely generated, generators"
  (and (group-generators G) #t))

;;; -- §5.4 preset groups ---------------------------------------------------

(define %the-trivial-group
  (make-group
    (lambda (a b) 'e)
    'e
    (lambda (g) 'e)
    (cons 'element? (lambda (x) (eq? x 'e)))
    (cons 'setoid (eqv-setoid))
    (cons 'order 1)
    (cons 'elements '(e))
    (cons 'generators '())))

(define (trivial-group)
  "Return the trivial group — the one-element group with element 'e.\nThe returned record is cached: (eq? (trivial-group) (trivial-group)) is #t.\n\nExamples:\n  (group-order (trivial-group))  => 1\n\nReturns: any\nCategory: algebra\nKeywords: trivial, unit group, one-element group\n\nSee also: `cyclic-group', `symmetric-group'."
  %the-trivial-group)

;; Internal — vector-permutation helpers used by symmetric-group.
;; Permutations are vectors of length n where perm[i] = image of i.

(define (%factorial n)
  (if (<= n 1) 1 (* n (%factorial (- n 1)))))

(define (%permutation-vector? v)
  (and (vector? v)
       (let* ((n    (vector-length v))
              (seen (make-vector n #f)))
         (let check ((i 0))
           (cond
             ((= i n) #t)
             (else
              (let ((x (vector-ref v i)))
                (cond
                  ((not (integer? x)) #f)
                  ((or (< x 0) (>= x n)) #f)
                  ((vector-ref seen x) #f)
                  (else
                   (vector-set! seen x #t)
                   (check (+ i 1)))))))))))

(define (%vector-permutation-op p q)
  ;; (p∘q)[i] = p[q[i]]
  (let* ((n (vector-length p))
         (r (make-vector n)))
    (let loop ((i 0))
      (cond
        ((= i n) r)
        (else
         (vector-set! r i (vector-ref p (vector-ref q i)))
         (loop (+ i 1)))))))

(define (%vector-permutation-inverse p)
  ;; r[p[i]] = i
  (let* ((n (vector-length p))
         (r (make-vector n)))
    (let loop ((i 0))
      (cond
        ((= i n) r)
        (else
         (vector-set! r (vector-ref p i) i)
         (loop (+ i 1)))))))

(define (%all-permutations n)
  ;; Lexicographic enumeration of all n! permutations of (0 .. n-1) as vectors.
  (cond
    ((= n 0) '(#()))
    (else
     (let permute-from ((prefix '()) (remaining (iota n)))
       (cond
         ((null? remaining)
          (list (list->vector (reverse prefix))))
         (else
          (apply append
                 (map (lambda (x)
                        (permute-from
                          (cons x prefix)
                          (remove (lambda (y) (= x y)) remaining)))
                      remaining))))))))

(define (cyclic-group n)
  "Return the cyclic group Z/nZ of order N — integers 0..n-1 under addition mod N.\nN must be a positive integer.\n\nExamples:\n  (group-op (cyclic-group 5) 2 4)       => 1\n  (group-inverse (cyclic-group 5) 2)    => 3\n  (group-generators (cyclic-group 5))   => (1)\n\nParameters:\n  n : integer\nReturns: any\nCategory: algebra\nKeywords: cyclic, Z mod n, modular arithmetic, rotation group\n\nSee also: `trivial-group', `symmetric-group'."
  (unless (and (integer? n) (positive? n))
    (error "cyclic-group: n must be a positive integer" n))
  (make-group
    (lambda (a b) (modulo (+ a b) n))
    0
    (lambda (k) (modulo (- n k) n))
    (cons 'element? (lambda (k) (and (integer? k) (<= 0 k) (< k n))))
    (cons 'setoid (numeric-setoid))
    (cons 'order n)
    (cons 'elements (iota n))
    (cons 'generators '(1))))

;; Internal — product-group helpers.

(define (%inject-at-index g i identities)
  ;; Return a list of length (length identities) with g at position i and
  ;; (list-ref identities j) at every other position j.
  (let loop ((j 0) (ids identities) (acc '()))
    (cond
      ((null? ids) (reverse acc))
      ((= j i)     (loop (+ j 1) (cdr ids) (cons g acc)))
      (else        (loop (+ j 1) (cdr ids) (cons (car ids) acc))))))

(define (%cartesian-product lists)
  ;; Cartesian product of a list of lists, preserving lex order of the
  ;; input. Result is a list of proper lists of length (length lists).
  (cond
    ((null? lists) '(()))
    (else
     (let ((tails (%cartesian-product (cdr lists))))
       (apply append
              (map (lambda (x) (map (lambda (t) (cons x t)) tails))
                   (car lists)))))))

(define (symmetric-group n)
  "Return the symmetric group S_n on {0, 1, ..., n-1}.\nElements are permutations represented as vectors of length N where v[i]\ngives the image of i. Composition is (p∘q)[i] = p[q[i]].\n\nFor n ≤ 1 the group is trivial; for n = 2 the single generator is the\ntransposition (0 1); for n ≥ 3 the generators are the transposition\n(0 1) together with the n-cycle (0 1 2 ... n-1).\n\nExamples:\n  (group-order (symmetric-group 3))      => 6\n  (group-identity (symmetric-group 3))   => #(0 1 2)\n\nParameters:\n  n : non-negative integer\nReturns: any\nCategory: algebra\nKeywords: symmetric group, permutation group, S_n, permutations\n\nSee also: `cyclic-group', `trivial-group'."
  (unless (and (integer? n) (>= n 0))
    (error "symmetric-group: n must be a non-negative integer" n))
  (let* ((id      (list->vector (iota n)))
         (trans01 (and (>= n 2)
                       (let ((v (list->vector (iota n))))
                         (vector-set! v 0 1)
                         (vector-set! v 1 0)
                         v)))
         (n-cycle (and (>= n 2)
                       (list->vector (append (cdr (iota n)) (list 0)))))
         (all     (%all-permutations n))
         (valid?  (lambda (v) (and (vector? v)
                                    (= (vector-length v) n)
                                    (%permutation-vector? v))))
         (gens    (cond
                    ((<= n 1) '())
                    ((= n 2)  (list trans01))
                    (else     (list trans01 n-cycle)))))
    (make-group
      %vector-permutation-op
      id
      %vector-permutation-inverse
      (cons 'element? valid?)
      (cons 'setoid (default-setoid))
      (cons 'order (%factorial n))
      (cons 'elements all)
      (cons 'generators gens))))

(define (product-group . groups)
  "Return the direct product of GROUPS. Variadic: accepts 0 or more groups.\nElements are proper lists of length n where the i-th component is drawn\nfrom the i-th input group. Order, elements, and generators are derived\ncomponentwise when every input group carries them; otherwise the\ncorresponding field on the product is #f.\n\nSpecial cases:\n  (product-group)        => trivial-group\n  (product-group G)      => G unchanged (eq?)\n\nExamples:\n  (group-order (product-group (cyclic-group 2) (cyclic-group 3)))  => 6\n  (group-op (product-group (cyclic-group 2) (cyclic-group 3))\n            '(1 2) '(0 1))                                          => (1 0)\n\nParameters:\n  groups : list of groups (variadic)\nReturns: any\nCategory: algebra\nKeywords: direct product, cartesian product, componentwise group\n\nSee also: `trivial-group', `cyclic-group'."
  (cond
    ((null? groups) (trivial-group))
    ((null? (cdr groups)) (car groups))
    (else
     (let* ((n          (length groups))
            (identities (map group-identity groups))
            (elts-pred?   (lambda (elt)
                            (and (list? elt)
                                 (= (length elt) n)
                                 (every (lambda (G e)
                                          (let ((p (group-element? G)))
                                            (or (not p) (p e))))
                                        groups elt))))
            (inv-fn     (lambda (elt)
                          (map (lambda (G e) ((group-inverse-fn G) e))
                               groups elt)))
            (op-fn      (lambda (a b)
                          (map (lambda (G e1 e2) ((group-op-fn G) e1 e2))
                               groups a b)))
            (orders     (map group-order groups))
            (all-elts   (map group-elements groups))
            (all-gens   (map group-generators groups))
            (order      (and (every (lambda (o) o) orders)
                             (apply * orders)))
            (elements   (and (every (lambda (e) e) all-elts)
                             (%cartesian-product all-elts)))
            (generators (and (every (lambda (g) g) all-gens)
                             (apply append
                                    (map (lambda (i gens-i)
                                           (map (lambda (g)
                                                  (%inject-at-index g i identities))
                                                gens-i))
                                         (iota n)
                                         all-gens)))))
       (make-group op-fn
                   identities
                   inv-fn
                   (cons 'element? elts-pred?)
                   (cons 'setoid (default-setoid))
                   (cons 'order order)
                   (cons 'elements elements)
                   (cons 'generators generators))))))

(define (group-op G a b)
  "Apply group G's binary operation to A and B.\n\nExamples:\n  (let ((G (make-group + 0 -))) (group-op G 2 3))  => 5\n\nParameters:\n  G : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: binary operation, group operation, combine, oplus, composition"
  ((group-op-fn G) a b))

(define (group-inverse G a)
  "Return the inverse of A in group G.\nThe inverse is the unique element such that combining A with\nits inverse (in either order) yields G's identity element.\n\nExamples:\n  (let ((G (make-group + 0 -))) (group-inverse G 5))   => -5\n  (let ((G (make-group + 0 -))) (group-inverse G -3))  => 3\n\nParameters:\n  G : any\n  a : any\nReturns: any\nCategory: algebra\nKeywords: inverse, negate, reciprocal, invert, opposite, minus\n\nSee also: `group-op', `group-identity'."
  ((group-inverse-fn G) a))

(define (group->monoid G)
  "Project group G to its underlying monoid by forgetting the inverse.\nThe resulting monoid has the same binary operation and identity\nelement as G.\n\nExamples:\n  (let* ((G (make-group + 0 -))\n         (M (group->monoid G)))\n    (monoid-op M 2 3))  => 5\n\nParameters:\n  G : any\nReturns: any\nCategory: algebra\nKeywords: forgetful functor, projection, underlying monoid, forget inverse\n\nSee also: `make-group', `make-monoid'."
  (make-monoid (group-op-fn G) (group-identity G)))

(define-syntax with-group
  (syntax-rules ()
    ((with-group G (op identity inverse) body ...)
     (let ((tmp G))
       (let ((op      (lambda (a b) (group-op tmp a b)))
             (identity (group-identity tmp))
             (inverse  (lambda (a) (group-inverse tmp a))))
         body ...)))))

(define (validate-group G samples)
  "Spot-check that G satisfies the group laws on SAMPLES.\nTests left and right identity, left and right inverse, and\nassociativity for all elements and triples in SAMPLES. Returns\n#t if all laws hold, or a list of (violation-type element ...)\nentries describing failures.\n\nExamples:\n  (validate-group (make-group + 0 -) '(1 2 3))  => #t\n\nParameters:\n  G : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: identity, inverse, associativity, law checking, validation\n\nSee also: `make-group', `group-op', `group-inverse'."
  (let ((violations '())
        (e (group-identity G)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Monoid laws + inverse
    (for-each
      (lambda (a)
        (unless (equal? (group-op G e a) a)
          (fail! 'left-identity a))
        (unless (equal? (group-op G a e) a)
          (fail! 'right-identity a))
        ;; Inverse
        (unless (equal? (group-op G a (group-inverse G a)) e)
          (fail! 'right-inverse a))
        (unless (equal? (group-op G (group-inverse G a) a) e)
          (fail! 'left-inverse a))
        ;; Associativity
        (for-each
          (lambda (b)
            (for-each
              (lambda (c)
                (unless (equal? (group-op G (group-op G a b) c)
                                (group-op G a (group-op G b c)))
                  (fail! 'associativity a b c)))
              samples))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))
