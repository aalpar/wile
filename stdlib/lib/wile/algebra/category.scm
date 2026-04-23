;;; (wile algebra category) — Categories
;;;
;;; A category consists of objects, morphisms between objects, an
;;; associative composition operation, and an identity morphism for
;;; each object.  Formally: for morphisms f : A → B and g : B → C,
;;; compose(g, f) : A → C, and id_A : A → A with id ∘ f = f ∘ id = f.

;; ─── Record type ─────────────────────────────

(define-record-type <category>
  (make-category* compose-fn identity-fn equiv-fn)
  category?
  (compose-fn  category-compose-fn)
  (identity-fn category-identity-fn)
  (equiv-fn    category-equiv-fn))

(define (make-category compose identity equiv?)
  "Construct a category from COMPOSE, IDENTITY, and EQUIV? functions.\nCOMPOSE takes two morphisms f and g and returns f ∘ g (apply g\nfirst, then f — mathematical convention). IDENTITY takes an\nobject and returns its identity morphism. EQUIV? tests whether\ntwo morphisms are considered equal.\n\nExamples:\n  (category? (make-category\n    (lambda (f g) (lambda (x) (f (g x))))\n    (lambda (obj) (lambda (x) x))\n    equal?))  => #t\n\nParameters:\n  compose : procedure\n  identity : procedure\n  equiv? : procedure\nReturns: any\nCategory: algebra\nKeywords: category, morphism, composition, identity, functor, arrow\n\nSee also: `category-compose', `category-identity', `validate-category'."
  (assert-procedure "make-category" compose)
  (assert-procedure "make-category" identity)
  (assert-procedure "make-category" equiv?)
  (make-category* compose identity equiv?))

;; ─── Core operations ─────────────────────────

(define (category-compose C f g)
  "Compose morphisms F and G in category C, yielding F ∘ G.\nFollows mathematical convention: apply G first, then F.\nBoth morphisms must be composable (codomain of G = domain of F).\n\nExamples:\n  (let* ((C (procedure-category))\n         (f (lambda (x) (* x 2)))\n         (g (lambda (x) (+ x 1)))\n         (fg (category-compose C f g)))\n    (fg 3))  => 8\n\nParameters:\n  C : any\n  f : any\n  g : any\nReturns: any\nCategory: algebra\nKeywords: compose, composition, circle, morphism, arrow, pipeline\n\nSee also: `category-identity', `category-equiv?'."
  ((category-compose-fn C) f g))

(define (category-identity C obj)
  "Return the identity morphism on OBJ in category C.\nThe identity morphism satisfies id ∘ f = f and f ∘ id = f\nfor all composable morphisms f.\n\nExamples:\n  (let* ((C (procedure-category))\n         (id (category-identity C 'any)))\n    (id 42))  => 42\n\nParameters:\n  C : any\n  obj : any\nReturns: any\nCategory: algebra\nKeywords: identity, unit, neutral, id, morphism\n\nSee also: `category-compose', `category-equiv?'."
  ((category-identity-fn C) obj))

(define (category-equiv? C f g)
  "Test whether morphisms F and G are equivalent in category C.\n\nExamples:\n  (let ((C (procedure-category)))\n    (category-equiv? C + +))  => #t\n\nParameters:\n  C : any\n  f : any\n  g : any\nReturns: boolean\nCategory: algebra\nKeywords: equivalence, equality, equal, morphism comparison"
  ((category-equiv-fn C) f g))

;; ─── Projection ──────────────────────────────

(define (category->endomorphism-monoid C obj)
  "Extract the monoid of endomorphisms on OBJ in category C.\nEndomorphisms are morphisms from an object to itself. Their\ncomposition is associative and the identity morphism serves as\nthe monoid identity. This makes explicit that every monoid is\na one-object category.\n\nExamples:\n  (let* ((C (procedure-category))\n         (M (category->endomorphism-monoid C 'any)))\n    ((monoid-op M (lambda (x) (* x 2)) (lambda (x) (+ x 1))) 3))  => 8\n\nParameters:\n  C : any\n  obj : any\nReturns: any\nCategory: algebra\nKeywords: endomorphism, monoid, one-object category, forgetful functor, projection\n\nSee also: `make-monoid', `category-compose', `category-identity'."
  (make-monoid (category-compose-fn C)
               ((category-identity-fn C) obj)))

;; ─── Pre-built instances ─────────────────────

(define (procedure-category)
  "Construct the category of Scheme procedures.\nMorphisms are procedures, composition is function composition\n(f ∘ g means apply g first, then f), and identity is the\nidentity function. Equivalence uses equal?.\n\nExamples:\n  (let* ((C (procedure-category))\n         (f (lambda (x) (* x 2)))\n         (g (lambda (x) (+ x 1))))\n    ((category-compose C f g) 3))  => 8\n\nReturns: any\nCategory: algebra\nKeywords: procedure, function, lambda, composition, identity function, Set\n\nSee also: `make-category', `category->endomorphism-monoid'."
  (make-category
    (lambda (f g)
      (lambda (x)
        (f (g x))))
    (lambda (obj)
      (lambda (x)
        x))
    equal?))

;; ─── with-category macro ─────────────────────

(define-syntax with-category
  (syntax-rules ()
    ((with-category C (compose identity equiv?) body ...)
     (let ((tmp C))
       (let ((compose (lambda (f g) (category-compose tmp f g)))
             (identity (lambda (obj) (category-identity tmp obj)))
             (equiv? (lambda (f g) (category-equiv? tmp f g))))
         body ...)))))

;; ─── Validation ──────────────────────────────

(define (validate-category C morphism-triples identity-morphisms)
  "Spot-check that C satisfies the category laws.\nMORPHISM-TRIPLES is a list of (f g h) triples for testing\nassociativity: (f ∘ g) ∘ h = f ∘ (g ∘ h). IDENTITY-MORPHISMS\nis a list of (obj f) pairs where f is an endomorphism on obj,\nfor testing identity laws: id ∘ f = f and f ∘ id = f.\nReturns #t if all laws hold, or a list of (violation-type ...)\nentries describing failures.\n\nExamples:\n  ;; Alist-based category on {0,1,2}:\n  ;; (validate-category alist-cat triples identities)  => #t\n\nParameters:\n  C : any\n  morphism-triples : list\n  identity-morphisms : list\nReturns: any\nCategory: algebra\nKeywords: associativity, identity, law checking, validation, morphism\n\nSee also: `make-category', `category-compose', `category-identity'."
  (let ((fail! (make-violation-reporter)))
    ;; Associativity: (f ∘ g) ∘ h = f ∘ (g ∘ h)
    (for-each
      (lambda (triple)
        (let ((f (car triple))
              (g (cadr triple))
              (h (caddr triple)))
          (let ((lhs (category-compose C (category-compose C f g) h))
                (rhs (category-compose C f (category-compose C g h))))
            (unless (category-equiv? C lhs rhs)
              (fail! 'associativity f g h)))))
      morphism-triples)
    ;; Identity laws: id ∘ f = f, f ∘ id = f
    (for-each
      (lambda (pair)
        (let ((obj (car pair))
              (f (cadr pair)))
          (let ((id (category-identity C obj)))
            (unless (category-equiv? C (category-compose C id f) f)
              (fail! 'left-identity obj f))
            (unless (category-equiv? C (category-compose C f id) f)
              (fail! 'right-identity obj f)))))
      identity-morphisms)
    (fail!)))
