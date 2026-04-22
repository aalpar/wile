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

(define (%validate-opts-keys site opts known-keys)
  ;; Reject unrecognized opts keys so typos (e.g. 'elements? for 'element?)
  ;; surface at construction instead of silently returning the fallback.
  (for-each
    (lambda (pair)
      (unless (and (pair? pair) (memv (car pair) known-keys))
        (error (string-append site ": unknown option key") pair known-keys)))
    opts))

(define (make-group op identity inverse . opts)
  "Construct a group from binary operation OP, IDENTITY element, and INVERSE function.\nOP must be associative, IDENTITY must be neutral for OP, and\nINVERSE must return an element such that OP of any element\nwith its inverse yields IDENTITY.\n\nOptional trailing alist entries specify extended metadata:\n  (element? . P) — membership predicate\n  (setoid . S) — equivalence relation (defaults to default-setoid)\n  (order . N) — group order (cardinality)\n  (elements . LIST) — full enumeration for finite groups\n  (generators . LIST) — generating set\n\nExamples:\n  (let ((G (make-group + 0 -))) (group-identity G))  => 0\n  (let ((G (make-group * 1 (lambda (x) (/ 1 x)))))\n    (group-op G 3 4))  => 12\n\nParameters:\n  op : procedure\n  identity : any\n  inverse : procedure\n  opts : alist\nReturns: any\nCategory: algebra\nKeywords: group, inverse, abelian, symmetry, algebraic structure\n\nSee also: `group->monoid', `validate-group'."
  (unless (procedure? op)
    (error "make-group: op must be a procedure" op))
  (unless (procedure? inverse)
    (error "make-group: inverse must be a procedure" inverse))
  (%validate-opts-keys "make-group" opts
                       '(element? setoid order elements generators))
  (let ((element? (%assv-or opts 'element? #f)))
    (unless (or (not element?) (procedure? element?))
      (error "make-group: element? must be a procedure" element?))
    (%make-group op identity inverse
                 element?
                 (%assv-or opts 'setoid     (default-setoid))
                 (%assv-or opts 'order      #f)
                 (%assv-or opts 'elements   #f)
                 (%assv-or opts 'generators #f))))

(define (group-equal? G a b)
  "Test A and B for equivalence under group G's setoid.\n\nExamples:\n  (group-equal? (make-group + 0 -) 3 3)  => #t\n\nParameters:\n  G : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\nKeywords: equality, setoid, equivalence"
  (setoid-equiv? (group-setoid G) a b))

(define (finite-group? G)
  "Return #t if group G carries both an order and an elements enumeration.\n\nParameters:\n  G : any\nReturns: boolean\nCategory: algebra\nKeywords: finite, enumerate, cardinality"
  (and (group-order G) (group-elements G) #t))

(define (finitely-generated-group? G)
  "Return #t if group G carries a generating set.\n\nParameters:\n  G : any\nReturns: boolean\nCategory: algebra\nKeywords: finitely generated, generators"
  (and (group-generators G) #t))

;;; -- §5.4 finite-group enumeration / subgroup closure --------------------
;;;
;;; Membership uses alist lookup keyed by the group's <setoid>. We do not
;;; use Wile hashtables because make-hashtable only supports atomic keys
;;; (not *Pair, *Vector, or ()) — see stdlib/lib/wile/algebra/incidence.scm
;;; for the same workaround in that library. Complexity is O(n²) in the
;;; closure order; acceptable for the §5.4 v1 target of small orbits and
;;; subgroups.

(define (%setoid-member? S x xs)
  (let loop ((xs xs))
    (cond
      ((null? xs) #f)
      ((setoid-equiv? S x (car xs)) #t)
      (else (loop (cdr xs))))))

(define (%symmetrize-generators gens inverse S)
  ;; Return gens ∪ {inverse(g) : g ∈ gens}, dedup'd under setoid S.
  ;; Shared by subgroup-generated and (in Phase 5) orbit.
  (let accum ((src gens) (acc '()))
    (cond
      ((null? src) (reverse acc))
      (else
       (let* ((g    (car src))
              (g-1  (inverse g))
              (acc1 (if (%setoid-member? S g acc) acc (cons g acc)))
              (acc2 (if (%setoid-member? S g-1 acc1) acc1 (cons g-1 acc1))))
         (accum (cdr src) acc2))))))

(define (subgroup-generated G generators . opts)
  "Return the subgroup of G generated by GENERATORS via BFS closure under G's\noperation. Optional trailing alist entries:\n  (max-size . N) — abort with an error if the closure exceeds N elements\n\nThe returned group carries the full elements enumeration, G's setoid, G's\nelement? predicate, and GENERATORS as its generating set.\n\nExamples:\n  (group-order (subgroup-generated (cyclic-group 6) '(2)))  => 3\n\nParameters:\n  G : group\n  generators : list\n  opts : alist\nReturns: any\nCategory: algebra\nKeywords: subgroup, closure, BFS, generated subgroup\n\nSee also: `enumerate-finite-group', `subgroup?'."
  (let* ((op       (group-op-fn G))
         (inverse  (group-inverse-fn G))
         (S        (group-setoid G))
         (id       (group-identity G))
         (gens+    (%symmetrize-generators generators inverse S))
         (_        (%validate-opts-keys "subgroup-generated" opts '(max-size)))
         (max-size (%assv-or opts 'max-size #f)))
    ;; Track closure size as an integer accumulator so the max-size check
    ;; and final order field are O(1) per step instead of O(n) (length seen).
    (let loop ((seen (list id)) (frontier (list id)) (size 1))
      (cond
        ((null? frontier)
         (make-group op id inverse
                     (cons 'element?   (group-element? G))
                     (cons 'setoid     S)
                     (cons 'order      size)
                     (cons 'elements   (reverse seen))
                     (cons 'generators generators)))
        (else
         (let ((current (car frontier)))
           (let scan ((gs gens+) (seen seen) (frontier (cdr frontier)) (size size))
             (cond
               ((null? gs) (loop seen frontier size))
               (else
                (let ((new-elt (op current (car gs))))
                  (cond
                    ((%setoid-member? S new-elt seen)
                     (scan (cdr gs) seen frontier size))
                    ((and max-size (>= size max-size))
                     (error "subgroup-generated: closure exceeded max-size"
                            max-size))
                    (else
                     (scan (cdr gs)
                           (cons new-elt seen)
                           (cons new-elt frontier)
                           (+ size 1))))))))))))))

(define (subgroup? H G)
  "Return #t if H is a subgroup of G: every element of H is in G, and H's\noperation agrees with G's on H's elements. Both must be finite.\n\nExamples:\n  (subgroup? (subgroup-generated (cyclic-group 6) '(2)) (cyclic-group 6))\n    => #t\n\nParameters:\n  H : group\n  G : group\nReturns: boolean\nCategory: algebra\nKeywords: subgroup, containment, algebraic substructure"
  (and (finite-group? H) (finite-group? G)
       (let ((S-G    (group-setoid G))
             (op-H   (group-op-fn H))
             (op-G   (group-op-fn G))
             (H-elts (group-elements H))
             (G-elts (group-elements G)))
         (and (every (lambda (h) (%setoid-member? S-G h G-elts)) H-elts)
              (every (lambda (a)
                       (every (lambda (b)
                                (setoid-equiv? S-G (op-H a b) (op-G a b)))
                              H-elts))
                     H-elts)))))

(define (enumerate-finite-group G . opts)
  "Promote a finitely-generated group to a finite group by enumerating its\nelements via BFS closure. Idempotent: if G already carries an elements\nenumeration, G is returned unchanged. Otherwise G must carry a generating\nset; subgroup-generated is invoked with OPTS (supporting (max-size . N)).\n\nRaises if G has neither elements nor generators.\n\nExamples:\n  (group-order (enumerate-finite-group\n                 (make-group (lambda (a b) (modulo (+ a b) 6)) 0\n                             (lambda (k) (modulo (- 6 k) 6))\n                             '(generators . (1)))))  => 6\n\nParameters:\n  G : group\n  opts : alist\nReturns: any\nCategory: algebra\nKeywords: enumerate, finite group, closure, BFS\n\nSee also: `subgroup-generated', `finite-group?'."
  (cond
    ((group-elements G) G)
    ((group-generators G)
     (apply subgroup-generated G (group-generators G) opts))
    (else
     (error "enumerate-finite-group: group has neither elements nor generators"
            G))))

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
  ;; Cap eager enumeration at n <= 8 (40320 elements). For n > 8, n!
  ;; allocation is prohibitive; omit elements and let callers opt in via
  ;; (enumerate-finite-group (symmetric-group n) '(max-size . K)) with
  ;; generators BFS bounded at K.
  (let* ((id      (list->vector (iota n)))
         (trans01 (and (>= n 2)
                       (let ((v (list->vector (iota n))))
                         (vector-set! v 0 1)
                         (vector-set! v 1 0)
                         v)))
         (n-cycle (and (>= n 2)
                       (list->vector (append (cdr (iota n)) (list 0)))))
         (all     (and (<= n 8) (%all-permutations n)))
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

(define (assert-group G samples)
  "Raise an error if G fails any group law on SAMPLES; return unspecified on\nsuccess. Thin raising variant of `validate-group' for callers that prefer\nexceptions to return-value dispatch on a polymorphic #t/list result.\n\nExamples:\n  (assert-group (make-group + 0 -) '(-2 -1 0 1 2))  ; no error\n\nParameters:\n  G : group\n  samples : list\nReturns: unspecified\nCategory: algebra\nKeywords: assert, validate, group laws, raise\n\nSee also: `validate-group'."
  (let ((result (validate-group G samples)))
    (unless (eq? result #t)
      (error "assert-group: group law violations" result))))

;;; -- §5.4 group actions ---------------------------------------------------
;;;
;;; A group action is a triple (G, X, ·) where G acts on a set X such that
;;;   (identity(G) · x) = x                                 (unit)
;;;   ((g · h) · x) = (g · (h · x))                         (compatibility)
;;; We represent X implicitly by a membership predicate set-element? and
;;; the action by a 2-argument procedure act : G × X → X.

(define-record-type <group-action>
  (%make-group-action group set-element? act-fn)
  group-action?
  (group        group-action-group)
  (set-element? group-action-set-element?)
  (act-fn       group-action-act-fn))

(define (group-action-act A g x)
  "Apply action A: return the image of set element X under group element G.\nEquivalent to ((group-action-act-fn A) g x); callers prefer this wrapper\nfor symmetry with `group-op'/`group-inverse'.\n\nExamples:\n  (let* ((Z3 (cyclic-group 3))\n         (A  (trivial-action Z3 integer?)))\n    (group-action-act A 1 42))  => 42\n\nParameters:\n  A : group-action\n  g : any\n  x : any\nReturns: any\nCategory: algebra\nKeywords: action, apply, act, group action application\n\nSee also: `make-group-action', `orbit', `stabilizer'."
  ((group-action-act-fn A) g x))

(define (make-group-action G set-element? act)
  "Construct a group action — a group G acting on a set (identified by\nSET-ELEMENT? membership predicate) via ACT : G × X → X. ACT takes a\ngroup element and a set element, returning a set element.\n\nExamples:\n  (let* ((Z3 (cyclic-group 3))\n         (A (make-group-action Z3 integer?\n                               (lambda (k x) (modulo (+ x k) 3)))))\n    (group-action-act A 1 2))  => 0\n\nParameters:\n  G : group\n  set-element? : procedure\n  act : procedure\nReturns: any\nCategory: algebra\nKeywords: group action, G-set, action, permutation representation\n\nSee also: `trivial-action', `orbit', `stabilizer'."
  (unless (group? G)
    (error "make-group-action: expected <group>" G))
  (unless (procedure? set-element?)
    (error "make-group-action: set-element? must be a procedure" set-element?))
  (unless (procedure? act)
    (error "make-group-action: act must be a procedure" act))
  (%make-group-action G set-element? act))

(define (trivial-action G set-element?)
  "Return the trivial action of G on the set (SET-ELEMENT?): every group\nelement fixes every set element.\n\nExamples:\n  (let ((A (trivial-action (cyclic-group 3) integer?)))\n    (group-action-act A 2 42))  => 42\n\nParameters:\n  G : group\n  set-element? : procedure\nReturns: any\nCategory: algebra\nKeywords: trivial action, fixed action, identity action\n\nSee also: `make-group-action'."
  (make-group-action G set-element? (lambda (g x) x)))

;;; -- §5.4 orbits, stabilizers, fixed-points ------------------------------
;;;
;;; Strategy (per Q4 resolution): prefer BFS from (group-generators G)
;;; symmetrized under inversion. Fall back to iterating (group-elements G)
;;; when only an enumeration is available. This handles infinite groups
;;; acting with finite orbits (e.g., Z on Z/12Z) correctly.
;;;
;;; Equality on the set X is Scheme equal? (v1 design). The group's setoid
;;; governs comparison of group elements only.

(define (orbit action x)
  "Return the orbit of X under ACTION as a list — all set elements reachable\nfrom X by applying group elements. BFS from group generators when\navailable; iterate-all over group elements otherwise. Errors if the\ngroup has neither.\n\nSet elements are compared with equal?.\n\nExamples:\n  (let* ((S2 (symmetric-group 2))\n         (A (make-group-action S2 integer?\n                               (lambda (p i) (vector-ref p i)))))\n    (length (orbit A 0)))  => 2\n\nParameters:\n  action : group-action\n  x : any\nReturns: list\nCategory: algebra\nKeywords: orbit, G-orbit, transitive, reachable, orbit equation\n\nSee also: `stabilizer', `burnside-count', `orbit-representative'."
  (let* ((G    (group-action-group action))
         (act  (group-action-act-fn action))
         (gens (group-generators G))
         (elts (group-elements G)))
    (cond
      (gens
       (let* ((inverse  (group-inverse-fn G))
              (S        (group-setoid G))
              (gens*    (%symmetrize-generators gens inverse S)))
         (let bfs ((seen (list x)) (frontier (list x)))
           (cond
             ((null? frontier) (reverse seen))
             (else
              (let ((current (car frontier)))
                (let scan ((gs gens*) (seen seen) (frontier (cdr frontier)))
                  (cond
                    ((null? gs) (bfs seen frontier))
                    (else
                     (let ((z (act (car gs) current)))
                       (cond
                         ((member z seen)
                          (scan (cdr gs) seen frontier))
                         (else
                          (scan (cdr gs)
                                (cons z seen)
                                (cons z frontier))))))))))))))
      (elts
       (let iter ((gs elts) (seen '()))
         (cond
           ((null? gs) (reverse seen))
           (else
            (let ((z (act (car gs) x)))
              (cond
                ((member z seen) (iter (cdr gs) seen))
                (else            (iter (cdr gs) (cons z seen)))))))))
      (else
       (error "orbit: group has neither generators nor element enumeration"
              G)))))

(define (stabilizer action x)
  "Return the stabilizer of X under ACTION as a list — all group elements\nthat fix X. Requires G to carry an elements enumeration.\n\nSet elements are compared with equal?.\n\nExamples:\n  (let* ((S3 (symmetric-group 3))\n         (A  (make-group-action S3 integer?\n                                (lambda (p i) (vector-ref p i)))))\n    (length (stabilizer A 0)))  => 2\n\nParameters:\n  action : group-action\n  x : any\nReturns: list\nCategory: algebra\nKeywords: stabilizer, point stabilizer, fixing subgroup, isotropy group\n\nSee also: `orbit', `fixed-points'."
  (let ((G   (group-action-group action))
        (act (group-action-act-fn action)))
    (unless (group-elements G)
      (error "stabilizer: group must have an elements enumeration" G))
    (filter (lambda (g) (equal? (act g x) x))
            (group-elements G))))

(define (orbit-representative action x less?)
  "Return the LESS?-minimum element of the orbit of X under ACTION.\n\nWhen LESS? is not strictly total on the orbit (i.e., neither (less? y best)\nnor (less? best y) holds for some pair), ties are broken by discovery\norder in (orbit action x): the earlier-discovered element is kept. This\nis deterministic within a given Wile binary but implementation-dependent\nacross versions. Callers needing cross-implementation stability must\nsupply a strictly total <?.\n\nExamples:\n  (let* ((S2 (symmetric-group 2))\n         (A (make-group-action S2 pair?\n              (lambda (p pr)\n                (if (= (vector-ref p 0) 0) pr (cons (cdr pr) (car pr)))))))\n    (orbit-representative A '(3 . 1)\n                          (lambda (a b) (< (car a) (car b)))))\n    => (1 . 3)\n\nParameters:\n  action : group-action\n  x : any\n  less? : procedure\nReturns: any\nCategory: algebra\nKeywords: canonical form, representative, orbit minimum, normalization\n\nSee also: `orbit', `burnside-count'."
  (let ((o (orbit action x)))
    (when (null? o)
      (error "orbit-representative: orbit is empty" x))
    (fold (lambda (y best) (if (less? y best) y best))
          (car o) (cdr o))))

(define (fixed-points action g X-elements)
  "Return all elements of X-ELEMENTS fixed by the group element G under\nACTION, as a list. Caller supplies X-ELEMENTS explicitly to support sets\nlarger or differently-structured than the group.\n\nSet elements are compared with equal?.\n\nExamples:\n  (let* ((S3 (symmetric-group 3))\n         (A  (make-group-action S3 integer?\n                                (lambda (p i) (vector-ref p i)))))\n    (length (fixed-points A #(0 1 2) '(0 1 2))))  => 3\n\nParameters:\n  action : group-action\n  g : any\n  X-elements : list\nReturns: list\nCategory: algebra\nKeywords: fixed points, fixed set, invariant elements\n\nSee also: `stabilizer', `burnside-count'."
  (let ((act (group-action-act-fn action)))
    (filter (lambda (x) (equal? (act g x) x)) X-elements)))

(define (burnside-count action X-elements)
  "Count orbits of ACTION on X-ELEMENTS via Burnside's lemma:\n  |X/G| = (1/|G|) Σ_{g ∈ G} |X^g|\nwhere X^g is the set of points fixed by g. The group G must be finite\n(carry an elements enumeration).\n\nRaises if the sum is not divisible by |G| — that condition proves\nthe provided act is not a group action (violates unit or compatibility\naxioms).\n\nExamples:\n  ;; 2-colourings of a 2-bead cycle modulo rotation = 3 necklaces\n  (let* ((Z2 (cyclic-group 2))\n         (cols '((0 0) (0 1) (1 0) (1 1)))\n         (rotate (lambda (k c)\n                   (if (= k 0) c (list (cadr c) (car c)))))\n         (A (make-group-action Z2 list? rotate)))\n    (burnside-count A cols))  => 3\n\nParameters:\n  action : group-action\n  X-elements : list\nReturns: integer\nCategory: algebra\nKeywords: Burnside, orbit counting, Cauchy-Frobenius, Pólya enumeration\n\nSee also: `orbit', `fixed-points', `enumerate-finite-group'."
  (let* ((G (group-action-group action))
         (n (group-order G)))
    (unless (finite-group? G)
      (error (string-append
               "burnside-count: group is not finite (requires both order "
               "and elements enumeration). If the group is finitely "
               "generated and you believe it is finite, use "
               "(enumerate-finite-group G) to promote it first.")
             G))
    (let* ((sum (fold (lambda (g acc)
                        (+ acc (length (fixed-points action g X-elements))))
                      0
                      (group-elements G)))
           (q (quotient sum n)))
      (unless (= (* q n) sum)
        (error "burnside-count: sum not divisible by |G| — act is not a group action"
               'sum sum '|G| n))
      q)))

;;; -- §5.4 preset actions --------------------------------------------------

(define (%group-action-set-predicate G site)
  ;; Cascade element? → elements → error. Prevents the silent liar predicate
  ;; (lambda (x) #t) that would widen the action's domain to everything.
  ;;
  ;; The derived-from-elements branch uses equal?, not G's setoid, because
  ;; the predicate must accept arbitrary off-type inputs without crashing
  ;; (e.g., numeric-setoid's = would raise on a symbol argument).
  (cond
    ((group-element? G))
    ((group-elements G)
     (let ((elts (group-elements G)))
       (lambda (x) (and (member x elts) #t))))
    (else
     (error
       (string-append site ": group must carry element? or elements "
                      "to derive the set-membership predicate")
       G))))

(define (permutation-action Sn n)
  "Natural action of the symmetric group S_n on {0, 1, ..., n-1}: a\npermutation vector P acts on an index i by returning P[i].\n\nExamples:\n  (let ((A (permutation-action (symmetric-group 3) 3)))\n    (group-action-act A #(2 0 1) 0))  => 2\n\nParameters:\n  Sn : symmetric group\n  n : positive integer\nReturns: any\nCategory: algebra\nKeywords: permutation action, natural action, S_n action\n\nSee also: `regular-action', `conjugation-action'."
  (unless (and (integer? n) (positive? n))
    (error "permutation-action: n must be a positive integer" n))
  (make-group-action
    Sn
    (lambda (x) (and (integer? x) (<= 0 x) (< x n)))
    (lambda (perm x) (vector-ref perm x))))

(define (regular-action G)
  "Left regular action of G on itself: each group element G acts on X ∈ G\nby left multiplication (op g x). Transitive on G's elements; the\nstabilizer of any point is trivial.\n\nExamples:\n  (let ((A (regular-action (cyclic-group 4))))\n    (group-action-act A 1 2))  => 3\n\nParameters:\n  G : group\nReturns: any\nCategory: algebra\nKeywords: regular action, left regular representation, Cayley action\n\nSee also: `permutation-action', `conjugation-action'."
  (make-group-action G
                     (%group-action-set-predicate G "regular-action")
                     (group-op-fn G)))

(define (conjugation-action G)
  "Conjugation action of G on itself: g · x = g · x · g⁻¹.\nOrbits are the conjugacy classes of G.\n\nExamples:\n  (let ((A (conjugation-action (symmetric-group 3))))\n    (length (orbit A #(1 0 2))))  => 3   ; three transpositions in S_3\n\nParameters:\n  G : group\nReturns: any\nCategory: algebra\nKeywords: conjugation, inner automorphism, conjugacy class\n\nSee also: `regular-action', `orbit'."
  (let ((op      (group-op-fn G))
        (inverse (group-inverse-fn G)))
    (make-group-action
      G
      (%group-action-set-predicate G "conjugation-action")
      (lambda (g x) (op (op g x) (inverse g))))))

(define (product-action . actions)
  "Return the direct product of ACTIONS. Variadic: accepts 0 or more actions.\nGroup elements and set elements are both proper lists of length n;\ncomponentwise application.\n\nSpecial cases:\n  (product-action)        => trivial action on the trivial group\n  (product-action A)      => A unchanged (eq?)\n\nExamples:\n  (let* ((A2 (permutation-action (symmetric-group 2) 2))\n         (A3 (permutation-action (symmetric-group 3) 3))\n         (A  (product-action A2 A3)))\n    (group-order (group-action-group A)))  => 12\n\nParameters:\n  actions : list of group-actions (variadic)\nReturns: any\nCategory: algebra\nKeywords: product action, direct product, componentwise action\n\nSee also: `product-group', `make-group-action'."
  (cond
    ((null? actions)
     (trivial-action (trivial-group) (group-element? (trivial-group))))
    ((null? (cdr actions)) (car actions))
    (else
     (let* ((G        (apply product-group (map group-action-group actions)))
            (acts     (map group-action-act-fn actions))
            (set-elts (map group-action-set-element? actions))
            (n        (length actions))
            (set-elt? (lambda (elt)
                        (and (list? elt)
                             (= (length elt) n)
                             (every (lambda (se e) (or (not se) (se e)))
                                    set-elts elt))))
            (act      (lambda (g-list elt-list)
                        (map (lambda (a g e) (a g e))
                             acts g-list elt-list))))
       (make-group-action G set-elt? act)))))
