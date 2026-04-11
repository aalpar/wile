;;; (wile algebra symbolic) — Named axioms, theories, and theory combinators
;;;
;;; A theory groups named axioms with metadata for three roles:
;;;   - Operational: the axiom procedure drives rewriting
;;;   - Equational: the general-form string documents the algebraic law
;;;   - Explanatory: the name provides human-readable labeling for traces
;;;
;;; Theory combinators (filter, exclude, prioritize, merge) produce new
;;; theories without mutating the originals.  This supports projections
;;; from algebraic structures (monoid->theory, lattice->theory, etc.)
;;; that select and reorder axioms for specific normalization strategies.
;;;
;;; See plans/2026-04-10-symbolic-algebra-design.md for design context.

;; ─── Local helpers ─────────────────────────
;;
;; We need filter/partition behavior but (scheme base) does not include
;; SRFI-1's filter.  These local definitions avoid an external dependency.

(define (keep pred lst)
  "Return elements of LST for which PRED returns true.\nLocal helper — equivalent to SRFI-1 filter.\n\nParameters:\n  pred : procedure\n  lst : list\nReturns: list\nCategory: algebra"
  (let loop ((remaining lst) (acc '()))
    (cond
      ((null? remaining) (reverse acc))
      ((pred (car remaining))
       (loop (cdr remaining) (cons (car remaining) acc)))
      (else
       (loop (cdr remaining) acc)))))

(define (remove pred lst)
  "Return elements of LST for which PRED returns false.\nLocal helper — complement of keep.\n\nParameters:\n  pred : procedure\n  lst : list\nReturns: list\nCategory: algebra"
  (keep (lambda (x)
          (not (pred x)))
        lst))

;; ─── Named axiom ──────────────────────────

(define-record-type <named-axiom>
  (make-named-axiom name general-form axiom)
  named-axiom?
  (name         named-axiom-name)
  (general-form named-axiom-general-form)
  (axiom        named-axiom-axiom))

;; ─── Theory ───────────────────────────────

(define-record-type <theory>
  (make-theory axioms associative-ops)
  theory?
  (axioms         theory-axioms)
  (associative-ops theory-associative-ops))

;; ─── Theory combinators ───────────────────

(define (named-axiom-name-in? names)
  "Return a predicate that tests whether a named-axiom's name\nis a member of NAMES (compared with equal?).\n\nParameters:\n  names : list\nReturns: procedure\nCategory: algebra"
  (lambda (na)
    (and (member (named-axiom-name na) names) #t)))

(define (theory-filter theory names)
  "Return a new theory containing only the named axioms whose\nnames appear in NAMES.  Preserves associative-ops unchanged.\n\nExamples:\n  (theory-filter th '(\"identity\"))\n    => theory with only the identity axiom\n\nParameters:\n  theory : theory\n  names : list\nReturns: theory\nCategory: algebra\nKeywords: filter, project, select, subset"
  (if (not (theory? theory))
      (error "theory-filter: expected theory" theory))
  (for-each (lambda (n)
              (if (not (string? n))
                  (error "theory-filter: names must be strings" n)))
            names)
  (make-theory
    (keep (named-axiom-name-in? names)
          (theory-axioms theory))
    (theory-associative-ops theory)))

(define (theory-exclude theory names)
  "Return a new theory with named axioms whose names appear in\nNAMES removed.  Preserves associative-ops unchanged.\n\nExamples:\n  (theory-exclude th '(\"commutativity\"))\n    => theory without commutativity\n\nParameters:\n  theory : theory\n  names : list\nReturns: theory\nCategory: algebra\nKeywords: exclude, remove, drop"
  (if (not (theory? theory))
      (error "theory-exclude: expected theory" theory))
  (for-each (lambda (n)
              (if (not (string? n))
                  (error "theory-exclude: names must be strings" n)))
            names)
  (make-theory
    (remove (named-axiom-name-in? names)
            (theory-axioms theory))
    (theory-associative-ops theory)))

(define (theory-prioritize theory names)
  "Return a new theory with axioms whose names appear in NAMES\nmoved to the front, preserving relative order within each group.\nThis controls rule application order in the normalizer.\n\nExamples:\n  (theory-prioritize th '(\"commutativity\"))\n    => theory with commutativity tried first\n\nParameters:\n  theory : theory\n  names : list\nReturns: theory\nCategory: algebra\nKeywords: prioritize, reorder, rule order, strategy"
  (if (not (theory? theory))
      (error "theory-prioritize: expected theory" theory))
  (for-each (lambda (n)
              (if (not (string? n))
                  (error "theory-prioritize: names must be strings" n)))
            names)
  (let ((pred (named-axiom-name-in? names))
        (axs  (theory-axioms theory)))
    (make-theory
      (append (keep pred axs)
              (remove pred axs))
      (theory-associative-ops theory))))

(define (theory-merge theory1 theory2)
  "Combine two theories by appending their axiom lists and\nassociative-ops lists.  Does not deduplicate.\n\nExamples:\n  (theory-merge plus-theory times-theory)\n    => theory with axioms from both\n\nParameters:\n  theory1 : theory\n  theory2 : theory\nReturns: theory\nCategory: algebra\nKeywords: merge, combine, union, compose"
  (if (not (theory? theory1))
      (error "theory-merge: expected theory" theory1))
  (if (not (theory? theory2))
      (error "theory-merge: expected theory" theory2))
  (make-theory
    (append (theory-axioms theory1)
            (theory-axioms theory2))
    (append (theory-associative-ops theory1)
            (theory-associative-ops theory2))))

;; ─── Rewrite step ─────────────────────────

(define-record-type <rewrite-step>
  (make-rewrite-step rule-name general-form before after)
  rewrite-step?
  (rule-name    step-rule-name)
  (general-form step-general-form)
  (before       step-before)
  (after        step-after))

;; Sentinel for fuel-exhausted steps — identity-unique like *no-match*.
(define *fuel-exhausted-name* (list 'fuel-exhausted))

(define (fuel-exhausted-step? step)
  "Test whether STEP is a fuel-exhaustion marker.\nThe recursive normalizer appends this step when the iteration\nlimit is reached before a fixed point.  Use this predicate\ninstead of comparing step-rule-name to a string.\n\nParameters:\n  step : rewrite-step\nReturns: boolean\nCategory: algebra\nKeywords: fuel, exhaustion, normalizer, limit"
  (and (rewrite-step? step)
       (eq? (step-rule-name step) *fuel-exhausted-name*)))

;; ─── Term protocol ────────────────────────

(define (sexp-term-protocol compare)
  "Construct a term protocol for S-expression terms.
Compound terms are pairs (op arg ...). Atoms are leaves.
COMPARE orders atoms for commutativity normalization.
COMPARE must handle all atom types that appear in terms —
if terms contain numbers or strings alongside symbols,
COMPARE must dispatch on type (e.g., via cond on symbol?,
number?, string?) rather than assuming all atoms are symbols.

Parameters:
  compare : procedure
Returns: any
Category: algebra"
  (make-term-protocol
    pair?
    car
    cdr
    (lambda (term new-args)
      (cons (car term) new-args))
    compare))

;; ─── Recursive normalizer ─────────────────

(define make-recursive-normalizer
  (case-lambda
    ((theory proto)
     (make-recursive-normalizer theory proto 100))
    ((theory proto fuel)
     (if (not (theory? theory))
         (error "make-recursive-normalizer: expected theory" theory))
     (if (not (term-protocol? proto))
         (error "make-recursive-normalizer: expected term-protocol" proto))
     (if (not (and (integer? fuel) (> fuel 0)))
         (error "make-recursive-normalizer: fuel must be a positive integer" fuel))
     ;; Precompile rules once at construction time — each entry is
     ;; (named-axiom . compiled-rule-list), avoiding per-term allocation.
     (let ((compiled
             (map (lambda (na)
                    (cons na (axiom->rules (named-axiom-axiom na) proto)))
                  (theory-axioms theory))))

       (define (try-named-rules term)
         (let na-loop ((entries compiled))
           (if (null? entries)
               #f
               (let ((na (caar entries)))
                 (let rule-loop ((rules (cdar entries)))
                   (if (null? rules)
                       (na-loop (cdr entries))
                       (let ((result ((car rules) term)))
                         (if (no-match? result)
                             (rule-loop (cdr rules))
                             (cons na result)))))))))

       ;; normalize-once threads fuel: each rewrite step decrements it.
       ;; Returns (values term trace remaining-fuel).
       (define (normalize-once term remaining)
         (if (or (not (term-compound? proto term))
                 (<= remaining 0))
             (values term '() remaining)
             (let ((operands (term-get-operands proto term)))
               (let child-loop ((remaining-ops operands)
                                (rev-children '())
                                (rev-trace '())
                                (fuel-left remaining))
                 (if (or (null? remaining-ops) (<= fuel-left 0))
                     ;; Copy any unprocessed children as-is when fuel exhausted
                     (let* ((done (append (reverse rev-children)
                                          remaining-ops))
                            (rebuilt (term-make-term proto term done)))
                       (if (<= fuel-left 0)
                           (values rebuilt (reverse rev-trace) fuel-left)
                           (let ((hit (try-named-rules rebuilt)))
                             (if hit
                                 (let ((na (car hit))
                                       (rewritten (cdr hit)))
                                   (values rewritten
                                           (reverse
                                             (cons (make-rewrite-step
                                                     (named-axiom-name na)
                                                     (named-axiom-general-form na)
                                                     rebuilt
                                                     rewritten)
                                                   rev-trace))
                                           (- fuel-left 1)))
                                 (values rebuilt (reverse rev-trace) fuel-left)))))
                     (let-values (((norm-child sub-trace child-fuel)
                                   (normalize-once (car remaining-ops) fuel-left)))
                       (child-loop (cdr remaining-ops)
                                   (cons norm-child rev-children)
                                   (append (reverse sub-trace) rev-trace)
                                   child-fuel)))))))

       (lambda (term)
         (let loop ((current term) (rev-trace '()) (remaining fuel))
           (if (<= remaining 0)
               (values current
                       (reverse
                         (cons (make-rewrite-step
                                 *fuel-exhausted-name*
                                 "rewrite limit exceeded"
                                 current current)
                               rev-trace)))
               (let-values (((result trace new-fuel)
                             (normalize-once current remaining)))
                 (if (null? trace)
                     (values result (reverse rev-trace))
                     (loop result
                           (append (reverse trace) rev-trace)
                           new-fuel))))))))))

;; ─── Theory projections ──────────────────

(define (monoid->theory M op-symbol)
  "Project monoid M into a theory with identity and associativity axioms.
The identity predicate matches elements equal? to M's identity element.
General-form strings include OP-SYMBOL for readability.

Note: equal? is type-sensitive for numbers (0 and 0.0 are not equal?).
If terms mix exact and inexact numbers, construct the theory manually
with an appropriate predicate instead of using this projection.

Parameters:
  M : monoid
  op-symbol : symbol
Returns: theory
Category: algebra
Keywords: monoid, projection, theory, identity, associativity"
  (let ((e (monoid-identity M))
        (op-str (symbol->string op-symbol)))
    (make-theory
      (list
        (make-named-axiom "identity"
          (string-append op-str "(a, e) = a")
          (make-identity-axiom op-symbol
            (lambda (x)
              (equal? x e))))
        (make-named-axiom "associativity"
          (string-append op-str "(a, " op-str "(b, c)) = "
                         op-str "(" op-str "(a, b), c)")
          (make-associativity-axiom op-symbol)))
      (list op-symbol))))

(define (lattice->theory L join-sym meet-sym)
  "Project lattice L into a theory with 10 axioms covering identity,
commutativity, idempotence, absorption, and associativity for both
join and meet operations. Join identity is bottom, meet identity is top.

Note: identity predicates use equal?, which is type-sensitive for numbers.
See monoid->theory for details.

Parameters:
  L : lattice
  join-sym : symbol
  meet-sym : symbol
Returns: theory
Category: algebra
Keywords: lattice, projection, theory, absorption, idempotence, commutativity"
  (let ((bot (lattice-bottom L))
        (top (lattice-top L))
        (join-str (symbol->string join-sym))
        (meet-str (symbol->string meet-sym)))
    (make-theory
      (list
        ;; Identity axioms
        (make-named-axiom "identity-join"
          (string-append join-str "(a, bot) = a")
          (make-identity-axiom join-sym
            (lambda (x)
              (equal? x bot))))
        (make-named-axiom "identity-meet"
          (string-append meet-str "(a, top) = a")
          (make-identity-axiom meet-sym
            (lambda (x)
              (equal? x top))))
        ;; Commutativity axioms
        (make-named-axiom "commutativity-join"
          (string-append join-str "(a, b) = " join-str "(b, a)")
          (make-commutativity-axiom join-sym))
        (make-named-axiom "commutativity-meet"
          (string-append meet-str "(a, b) = " meet-str "(b, a)")
          (make-commutativity-axiom meet-sym))
        ;; Idempotence axioms
        (make-named-axiom "idempotence-join"
          (string-append join-str "(a, a) = a")
          (make-idempotence-axiom join-sym))
        (make-named-axiom "idempotence-meet"
          (string-append meet-str "(a, a) = a")
          (make-idempotence-axiom meet-sym))
        ;; Absorption axioms
        (make-named-axiom "absorption-join/meet"
          (string-append join-str "(a, " meet-str "(a, b)) = a")
          (make-absorption-axiom join-sym meet-sym))
        (make-named-axiom "absorption-meet/join"
          (string-append meet-str "(a, " join-str "(a, b)) = a")
          (make-absorption-axiom meet-sym join-sym))
        ;; Associativity axioms
        (make-named-axiom "associativity-join"
          (string-append join-str "(a, " join-str "(b, c)) = "
                         join-str "(" join-str "(a, b), c)")
          (make-associativity-axiom join-sym))
        (make-named-axiom "associativity-meet"
          (string-append meet-str "(a, " meet-str "(b, c)) = "
                         meet-str "(" meet-str "(a, b), c)")
          (make-associativity-axiom meet-sym)))
      (list join-sym meet-sym))))

(define (group->theory G op-symbol inv-symbol)
  "Project group G into a theory with identity, associativity, and
involution (inverse) axioms. OP-SYMBOL names the binary operation,
INV-SYMBOL names the inverse operation in the consumer's term language.

Parameters:
  G : group
  op-symbol : symbol
  inv-symbol : symbol
Returns: theory
Category: algebra
Keywords: group, projection, theory, inverse, involution"
  (let ((monoid-th (monoid->theory (group->monoid G) op-symbol))
        (inv-str (symbol->string inv-symbol)))
    (theory-merge
      monoid-th
      (make-theory
        (list (make-named-axiom "inverse-involution"
                (string-append inv-str "(" inv-str "(a)) = a")
                (make-involution-axiom inv-symbol)))
        '()))))

(define (semiring->theory S plus-sym times-sym)
  "Project semiring S into a theory with 6 axioms: identity and
associativity for both operations, commutativity for addition,
and absorbing element for multiplication.

Parameters:
  S : semiring
  plus-sym : symbol
  times-sym : symbol
Returns: theory
Category: algebra
Keywords: semiring, projection, theory, distributive, absorbing"
  (let ((z (semiring-zero S))
        (o (semiring-one S))
        (ps (symbol->string plus-sym))
        (ts (symbol->string times-sym)))
    (make-theory
      (list
        ;; Additive identity
        (make-named-axiom "identity-plus"
          (string-append ps "(a, 0) = a")
          (make-identity-axiom plus-sym
            (lambda (x) (equal? x z))))
        ;; Multiplicative identity
        (make-named-axiom "identity-times"
          (string-append ts "(a, 1) = a")
          (make-identity-axiom times-sym
            (lambda (x) (equal? x o))))
        ;; Additive commutativity
        (make-named-axiom "commutativity-plus"
          (string-append ps "(a, b) = " ps "(b, a)")
          (make-commutativity-axiom plus-sym))
        ;; Multiplicative absorbing element
        (make-named-axiom "absorbing-times"
          (string-append ts "(a, 0) = 0")
          (make-absorbing-axiom times-sym
            (lambda (x) (equal? x z))))
        ;; Additive associativity
        (make-named-axiom "associativity-plus"
          (string-append ps "(a, " ps "(b, c)) = "
                         ps "(" ps "(a, b), c)")
          (make-associativity-axiom plus-sym))
        ;; Multiplicative associativity
        (make-named-axiom "associativity-times"
          (string-append ts "(a, " ts "(b, c)) = "
                         ts "(" ts "(a, b), c)")
          (make-associativity-axiom times-sym)))
      (list plus-sym times-sym))))

(define (ring->theory R plus-sym times-sym neg-sym)
  "Project ring R into a theory with 7 axioms: the 6 semiring axioms
plus involution for negation.

Parameters:
  R : ring
  plus-sym : symbol
  times-sym : symbol
  neg-sym : symbol
Returns: theory
Category: algebra
Keywords: ring, projection, theory, negation, involution"
  (let ((semi-th (semiring->theory (ring->semiring R) plus-sym times-sym))
        (ns (symbol->string neg-sym)))
    (theory-merge
      semi-th
      (make-theory
        (list (make-named-axiom "negate-involution"
                (string-append ns "(" ns "(a)) = a")
                (make-involution-axiom neg-sym)))
        '()))))

(define (field->theory F plus-sym times-sym neg-sym recip-sym)
  "Project field F into a theory with 8 axioms: the 7 ring axioms
plus involution for reciprocal.

Parameters:
  F : field
  plus-sym : symbol
  times-sym : symbol
  neg-sym : symbol
  recip-sym : symbol
Returns: theory
Category: algebra
Keywords: field, projection, theory, reciprocal, involution"
  (let ((ring-th (ring->theory (field->ring F) plus-sym times-sym neg-sym))
        (rs (symbol->string recip-sym)))
    (theory-merge
      ring-th
      (make-theory
        (list (make-named-axiom "reciprocal-involution"
                (string-append rs "(" rs "(a)) = a")
                (make-involution-axiom recip-sym)))
        '()))))

(define (heyting->theory H join-sym meet-sym)
  "Project Heyting algebra H into a theory via its underlying lattice.
Produces the same 10 lattice axioms. Heyting implication is not
included as a rewrite axiom — it is a derived operation, not an
equational simplification rule.

Parameters:
  H : heyting-algebra
  join-sym : symbol
  meet-sym : symbol
Returns: theory
Category: algebra
Keywords: Heyting, projection, theory, intuitionistic, lattice"
  (lattice->theory (heyting->lattice H) join-sym meet-sym))

(define (boolean->theory B join-sym meet-sym comp-sym)
  "Project Boolean algebra B into a theory with 11 axioms: the 10 lattice
axioms from the underlying lattice plus complement involution.
Uses theory-merge to combine lattice theory with involution theory.

Parameters:
  B : boolean-algebra
  join-sym : symbol
  meet-sym : symbol
  comp-sym : symbol
Returns: theory
Category: algebra
Keywords: Boolean, projection, theory, complement, involution, lattice"
  (let ((lattice-th (lattice->theory (boolean->lattice B) join-sym meet-sym))
        (involution-th (make-theory
                         (list (make-named-axiom "complement-involution"
                                 (string-append (symbol->string comp-sym)
                                                "(" (symbol->string comp-sym)
                                                "(a)) = a")
                                 (make-involution-axiom comp-sym)))
                         '())))
    (theory-merge lattice-th involution-th)))

;; ─── Equivalence discovery ────────────────

(define (discover-equivalences theory proto term)
  "Find distinct normal forms by running TERM through the full theory
and each non-directional single-rule sub-theory.  Returns a list of
(normal-form . trace) pairs, deduplicated by equal? on normal-form.

Directional axioms (e.g. associativity) are not explored individually —
they would produce combinatorial bracketings without reducing term size.

Parameters:
  theory : theory
  proto : term-protocol
  term : any
Returns: list
Category: algebra
Keywords: equivalence, discovery, normal form, exploration"
  (if (not (theory? theory))
      (error "discover-equivalences: expected theory" theory))
  (if (not (term-protocol? proto))
      (error "discover-equivalences: expected term-protocol" proto))
  (let ((seen '())
        (results '()))
    (define (try-theory th)
      (let ((norm (make-recursive-normalizer th proto)))
        (let-values (((result trace) (norm term)))
          (unless (member result seen)
            (set! seen (cons result seen))
            (set! results (cons (cons result trace) results))))))
    ;; Full theory first
    (try-theory theory)
    ;; Each non-directional single-rule theory
    (for-each
      (lambda (na)
        (unless (directional-axiom? (named-axiom-axiom na))
          (try-theory (make-theory (list na)
                                   (theory-associative-ops theory)))))
      (theory-axioms theory))
    (reverse results)))

;; ─── Reporter ─────────────────────────────

(define (display-to-string val)
  "Write VAL to a string using display notation.

Parameters:
  val : any
Returns: string
Category: algebra"
  (let ((port (open-output-string)))
    (display val port)
    (get-output-string port)))

(define (format-trace trace)
  "Format a list of rewrite steps as human-readable strings.

Parameters:
  trace : list
Returns: list
Category: algebra"
  (map (lambda (step)
         (if (fuel-exhausted-step? step)
             (string-append "[fuel exhausted] "
               (display-to-string (step-before step)))
             (string-append
               (step-rule-name step)
               " (" (step-general-form step) "): "
               (display-to-string (step-before step))
               " → "
               (display-to-string (step-after step)))))
       trace))
