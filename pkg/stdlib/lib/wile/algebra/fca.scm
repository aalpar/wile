;;; (wile algebra fca) — Formal Concept Analysis
;;;
;;; Discovers formal concepts (closed object-attribute pairs) from a
;;; binary relation via the NextClosure algorithm (Ganter, 1984).
;;; Concept lattices are the mathematical dual of Galois connections
;;; applied to finite contexts.

;;; ── Local utilities ─────────────────────────────────────

(define (filter-map f lst)
  (let loop ((xs lst) (acc '()))
    (if (null? xs) (reverse acc)
      (let ((v (f (car xs))))
        (loop (cdr xs) (if v (cons v acc) acc))))))

(define (member? x lst) (and (member x lst) #t))

;;; ── Sorted string sets ─────────────────────────────────

;; Add element to a sorted string list, maintaining sort and uniqueness.
(define (set-add elem sorted)
  "Add an element to a sorted string list, maintaining sort and uniqueness.\n\nParameters:\n  elem : string\n  sorted : list\nReturns: list\nCategory: algebra"
  (cond ((null? sorted) (list elem))
        ((string<? elem (car sorted)) (cons elem sorted))
        ((string=? elem (car sorted)) sorted)
        (else (cons (car sorted) (set-add elem (cdr sorted))))))

;; Insertion sort with dedup. Returns a sorted list of unique strings.
(define (sort-strings lst)
  "Sort a list of strings, removing duplicates.\n\nParameters:\n  lst : list\nReturns: list\nCategory: algebra"
  (let loop ((xs lst) (acc '()))
    (if (null? xs) acc
      (loop (cdr xs) (set-add (car xs) acc)))))

;; Intersection of two sorted string lists.
(define (set-intersect a b)
  "Intersection of two sorted string lists.\n\nParameters:\n  a : list\n  b : list\nReturns: list\nCategory: algebra"
  (cond ((null? a) '())
        ((null? b) '())
        ((string<? (car a) (car b)) (set-intersect (cdr a) b))
        ((string<? (car b) (car a)) (set-intersect a (cdr b)))
        (else (cons (car a) (set-intersect (cdr a) (cdr b))))))

;; Membership test with early exit on sorted list.
(define (set-member? elem sorted)
  "Test membership in a sorted string list with early exit.\n\nParameters:\n  elem : string\n  sorted : list\nReturns: boolean\nCategory: algebra"
  (cond ((null? sorted) #f)
        ((string<? elem (car sorted)) #f)
        ((string=? elem (car sorted)) #t)
        (else (set-member? elem (cdr sorted)))))

;; Union of two sorted string lists.
(define (set-union a b)
  "Union of two sorted string lists.\n\nParameters:\n  a : list\n  b : list\nReturns: list\nCategory: algebra"
  (cond ((null? a) b)
        ((null? b) a)
        ((string<? (car a) (car b))
         (cons (car a) (set-union (cdr a) b)))
        ((string<? (car b) (car a))
         (cons (car b) (set-union a (cdr b))))
        (else (cons (car a) (set-union (cdr a) (cdr b))))))

;; Subset test on sorted string lists.
(define (set-subset? a b)
  "Test whether sorted string list A is a subset of B.\n\nParameters:\n  a : list\n  b : list\nReturns: boolean\nCategory: algebra"
  (cond ((null? a) #t)
        ((null? b) #f)
        ((string<? (car a) (car b)) #f)
        ((string=? (car a) (car b)) (set-subset? (cdr a) (cdr b)))
        (else (set-subset? a (cdr b)))))

;; Elements strictly before a given element in sorted order.
(define (set-before elem sorted)
  "Return elements strictly before ELEM in a sorted string list.\n\nParameters:\n  elem : string\n  sorted : list\nReturns: list\nCategory: algebra"
  (cond ((null? sorted) '())
        ((string<? (car sorted) elem)
         (cons (car sorted) (set-before elem (cdr sorted))))
        (else '())))

;;; ── Context (record type) ──────────────────────────────

(define-record-type <fca-context>
  (make-fca-context objects attributes obj->attrs attr->objs)
  fca-context?
  (objects    context-objects)
  (attributes context-attributes)
  (obj->attrs fca-context-obj->attrs)
  (attr->objs fca-context-attr->objs))

;;; ── Context construction ───────────────────────────────

;; Build an FCA context from objects, attributes, and an incidence function.
;; incidence: (lambda (obj attr) -> boolean)
;; Lookup tables are hash tables for O(1) access in intent/extent.
(define (make-context objects attributes incidence)
  "Build an FCA context from objects, attributes, and an incidence function.\nINCIDENCE is (lambda (obj attr) -> boolean). Lookup tables are hash\ntables for O(1) access in intent/extent.\n\nParameters:\n  objects : list\n  attributes : list\n  incidence : procedure\nReturns: fca-context\nCategory: algebra"
  (let* ((objs (sort-strings objects))
         (attrs (sort-strings attributes))
         (obj->attrs (make-hashtable))
         (attr->objs (make-hashtable)))
    (for-each
      (lambda (o)
        (hashtable-set! obj->attrs o
          (let loop ((as attrs))
            (cond ((null? as) '())
                  ((incidence o (car as))
                   (cons (car as) (loop (cdr as))))
                  (else (loop (cdr as)))))))
      objs)
    (for-each
      (lambda (a)
        (hashtable-set! attr->objs a
          (let loop ((os objs))
            (cond ((null? os) '())
                  ((incidence (car os) a)
                   (cons (car os) (loop (cdr os))))
                  (else (loop (cdr os)))))))
      attrs)
    (make-fca-context objs attrs obj->attrs attr->objs)))

;; Convenience: build context from an association list.
;; Each entry is (object attr1 attr2 ...).
(define (context-from-alist entries)
  "Build an FCA context from an association list.\nEach entry is (object attr1 attr2 ...).\n\nParameters:\n  entries : list\nReturns: fca-context\nCategory: algebra\n\nExamples:\n  (context-from-alist '((\"f1\" \"A.x\" \"B.y\") (\"f2\" \"A.x\")))"
  ;; The entries ARE the obj->attrs relation already indexed by object, so
  ;; read it off directly rather than handing make-context an O(objects)
  ;; `assoc` incidence closure it would invoke objects*attributes times
  ;; (that composition is O(objects^2 * attributes)). This is linear in the
  ;; size of the relation. Semantics preserved exactly: first entry wins per
  ;; duplicate object key, and the attribute set is the union over ALL
  ;; entries (a duplicate object's later attributes still join the attribute
  ;; universe even though they are not incident to that object).
  (let ((obj->attrs (make-hashtable))
        (attr->objs (make-hashtable))
        (obj-acc '())
        (attr-acc '()))
    (for-each
      (lambda (entry)
        (let ((o (car entry))
              (as (cdr entry)))
          (set! attr-acc (append as attr-acc))
          (unless (hashtable-ref obj->attrs o #f)
            (hashtable-set! obj->attrs o (sort-strings as))
            (set! obj-acc (cons o obj-acc)))))
      entries)
    (let ((objs (sort-strings obj-acc))
          (attrs (sort-strings attr-acc)))
      ;; Seed every attribute bucket so attributes with empty extent carry an
      ;; explicit '() entry, matching make-context's exhaustive construction.
      (for-each (lambda (a) (hashtable-set! attr->objs a '())) attrs)
      ;; Invert obj->attrs. Iterate objects in ascending order, prepend, then
      ;; reverse, so each attribute bucket comes out sorted ascending.
      (for-each
        (lambda (o)
          (for-each
            (lambda (a)
              (hashtable-set! attr->objs a
                (cons o (hashtable-ref attr->objs a '()))))
            (hashtable-ref obj->attrs o '())))
        objs)
      (for-each
        (lambda (a)
          (hashtable-set! attr->objs a (reverse (hashtable-ref attr->objs a '()))))
        attrs)
      (make-fca-context objs attrs obj->attrs attr->objs))))

;;; ── Derivation operators (Galois connection) ───────────

;; Attributes shared by ALL objects in object-set.
;; Empty object-set -> all attributes (vacuous truth).
(define (intent ctx object-set)
  "Galois connection: return attributes shared by all objects in OBJECT-SET.\nEmpty object-set returns all attributes (vacuous truth).\n\nParameters:\n  ctx : fca-context\n  object-set : list\nReturns: list\nCategory: algebra"
  (if (null? object-set)
    (context-attributes ctx)
    (let ((ht (fca-context-obj->attrs ctx)))
      (let loop ((rest (cdr object-set))
                 (acc (hashtable-ref ht (car object-set) '())))
        (if (null? rest) acc
          (loop (cdr rest)
                (set-intersect acc (hashtable-ref ht (car rest) '()))))))))

;; Objects having ALL attributes in attribute-set.
;; Empty attribute-set -> all objects (vacuous truth).
(define (extent ctx attribute-set)
  "Galois connection: return objects having all attributes in ATTRIBUTE-SET.\nEmpty attribute-set returns all objects (vacuous truth).\n\nParameters:\n  ctx : fca-context\n  attribute-set : list\nReturns: list\nCategory: algebra"
  (if (null? attribute-set)
    (context-objects ctx)
    (let ((ht (fca-context-attr->objs ctx)))
      (let loop ((rest (cdr attribute-set))
                 (acc (hashtable-ref ht (car attribute-set) '())))
        (if (null? rest) acc
          (loop (cdr rest)
                (set-intersect acc (hashtable-ref ht (car rest) '()))))))))

;;; ── Concept lattice (NextClosure, Ganter 1984) ─────────

;; Concept accessors: a concept is (extent . intent).
(define (concept-extent c)
  "Extract the extent (object set) from a concept.\n\nParameters:\n  c : pair\nReturns: list\nCategory: algebra"
  (car c))

(define (concept-intent c)
  "Extract the intent (attribute set) from a concept.\n\nParameters:\n  c : pair\nReturns: list\nCategory: algebra"
  (cdr c))

;; Closure operator: attribute set -> closed attribute set.
(define (fca-close ctx attrs)
  (intent ctx (extent ctx attrs)))

;; Next closure in lectic order.
;; Returns the next closed set after current, or #f if done.
(define (next-closure current attrs close)
  (let ((attr-vec (list->vector attrs))
        (n (length attrs)))
    (let loop ((i (- n 1)))
      (if (< i 0) #f
        (let ((ai (vector-ref attr-vec i)))
        (if (set-member? ai current)
          (loop (- i 1))
          (let* ((prefix (set-before ai current))
                 (b-prime (set-add ai prefix))
                 (c (close b-prime)))
            (if (equal? (set-before ai c) prefix)
              c
              (loop (- i 1))))))))))

;; Build the full concept lattice.
;; Returns a list of concepts (extent . intent) in lectic order.
(define (concept-lattice ctx)
  "Build the full concept lattice via NextClosure (Ganter 1984).\nReturns concepts in lectic order, each as (extent . intent).\n\nParameters:\n  ctx : fca-context\nReturns: list\nCategory: algebra"
  (let* ((attrs (context-attributes ctx))
         (close (lambda (b) (fca-close ctx b)))
         (first (close '())))
    (let loop ((current first) (acc '()))
      (let ((concept (cons (extent ctx current) current)))
        (let ((next (next-closure current attrs close)))
          (if next
            (loop next (cons concept acc))
            (reverse (cons concept acc))))))))

;;; ── Concept lattice -> algebra lattice ─────────────────

;; Find the concept in the lattice matching the given intent.
(define (find-concept-by-intent lattice int)
  (let loop ((cs lattice))
    (cond ((null? cs) #f)
          ((equal? (concept-intent (car cs)) int) (car cs))
          (else (loop (cdr cs))))))

;; Construct a (wile algebra lattice) from an FCA concept lattice.
;; ctx: the FCA context (needed for Galois connection operations)
;; concepts: the list of concepts from (concept-lattice ctx)
;;
;; Lattice ordering: C1 <= C2 iff E1 <= E2 (equiv. I2 <= I1)
;; Join: concept whose intent = closure(I1 /\ I2)
;; Meet: concept whose intent = closure(I1 \/ I2)
(define (concept-lattice->algebra-lattice ctx concepts)
  "Construct a (wile algebra lattice) from an FCA concept lattice.\nCTX is the FCA context, CONCEPTS is the list from (concept-lattice ctx).\nThe resulting lattice has join/meet via the Galois closure operator.\n\nParameters:\n  ctx : fca-context\n  concepts : list\nReturns: lattice\nCategory: algebra"
  (if (null? concepts)
    (error "concept-lattice->algebra-lattice: concepts list is empty"))
  (let* ((all-attrs (context-attributes ctx))
         ;; Closure operator: cl(A) = intent(extent(A))
         (cl (make-closure-operator
               (lambda (attrs) (intent ctx (extent ctx attrs)))
               (powerset-lattice all-attrs)))
         ;; Top: concept with cl('()) as intent (shared by all objects)
         (top-intent (closure-close cl '()))
         (top-concept (find-concept-by-intent concepts top-intent))
         ;; Bottom: concept with cl(all-attrs) as intent
         (bottom-intent (closure-close cl all-attrs))
         (bottom-concept (find-concept-by-intent concepts bottom-intent)))
    (make-lattice
      ;; join: least upper bound — intent = cl(I1 /\ I2)
      (lambda (c1 c2)
        (let ((int (closure-close cl (set-intersect (concept-intent c1) (concept-intent c2)))))
          (or (find-concept-by-intent concepts int)
              (cons (extent ctx int) int))))
      ;; meet: greatest lower bound — intent = cl(I1 \/ I2)
      (lambda (c1 c2)
        (let ((int (closure-close cl (set-union (concept-intent c1) (concept-intent c2)))))
          (or (find-concept-by-intent concepts int)
              (cons (extent ctx int) int))))
      ;; bottom
      bottom-concept
      ;; top
      top-concept
      ;; leq: C1 <= C2 iff I2 <= I1 (more attributes = lower in lattice)
      (lambda (c1 c2)
        (set-subset? (concept-intent c2) (concept-intent c1))))))

;;; ── Concept relationship ───────────────────────────────

;; Determine the relationship between two concepts.
;; Returns one of: 'subconcept, 'superconcept, 'equal, 'incomparable
(define (concept-relationship c1 c2)
  "Classify the lattice relationship between two concepts.\nReturns: subconcept (C1 <= C2), superconcept (C1 >= C2), equal, or incomparable.\n\nParameters:\n  c1 : pair\n  c2 : pair\nReturns: symbol\nCategory: algebra"
  (let ((i1 (concept-intent c1))
        (i2 (concept-intent c2)))
    (let ((i2-sub-i1 (set-subset? i2 i1))
          (i1-sub-i2 (set-subset? i1 i2)))
      (cond ((and i1-sub-i2 i2-sub-i1) 'equal)
            (i2-sub-i1 'subconcept)    ;; I2 <= I1 means E1 <= E2, C1 <= C2
            (i1-sub-i2 'superconcept)  ;; I1 <= I2 means E2 <= E1, C2 <= C1
            (else 'incomparable)))))
