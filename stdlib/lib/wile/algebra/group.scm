;;; (wile algebra group) — Groups
;;;
;;; A group (G, ⊕, e, ⁻¹) is a monoid with inverses:
;;; a ⊕ a⁻¹ = a⁻¹ ⊕ a = e.

(define-record-type <group>
  (make-group* op-fn identity inverse-fn)
  group?
  (op-fn      group-op-fn)
  (identity   group-identity)
  (inverse-fn group-inverse-fn))

(define (make-group op identity inverse)
  "Construct a group from binary operation OP, IDENTITY element, and INVERSE function.\nOP must be associative, IDENTITY must be neutral for OP, and\nINVERSE must return an element such that OP of any element\nwith its inverse yields IDENTITY.\n\nExamples:\n  (let ((G (make-group + 0 -))) (group-identity G))  => 0\n  (let ((G (make-group * 1 (lambda (x) (/ 1 x)))))\n    (group-op G 3 4))  => 12\n\nParameters:\n  op : procedure\n  identity : any\n  inverse : procedure\nReturns: any\nCategory: algebra\nKeywords: group, inverse, abelian, symmetry, algebraic structure\n\nSee also: `group->monoid', `validate-group'."
  (make-group* op identity inverse))

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
