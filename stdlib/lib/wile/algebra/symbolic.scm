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
    compare
    ;; Operator constructor: lets head-changing rules (De Morgan) and the AC
    ;; complement-fold mint a compound with an arbitrary head operator.
    (lambda (operator operands)
      (cons operator operands))))

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
  "Project lattice L into a theory with 4 axioms: an AC (associative-commutative)
axiom for each of join and meet, plus the two absorption laws. Each AC axiom
flattens nested same-operator nodes, drops the identity, collapses to the
annihilator when present, dedups (idempotence), and sorts — subsuming
commutativity, associativity, idempotence, identity, and annihilation for that
operator in one terminating step. Join identity is bottom (annihilator top);
meet identity is top (annihilator bottom).

The annihilation laws (a join top = top, a meet bot = bot) require a *bounded*
lattice; make-lattice always carries bottom and top, so they always apply.

Replaces the earlier 10-axiom projection whose pairwise commutativity +
associativity did not terminate on terms with three or more leaves under one
operator. AC normalization sorts once and cannot ping-pong.

Note: identity/annihilator detection uses equal?, which is type-sensitive for
numbers. See monoid->theory for details.

Parameters:
  L : lattice
  join-sym : symbol
  meet-sym : symbol
Returns: theory
Category: algebra
Keywords: lattice, projection, theory, AC normalization, absorption, bounded"
  (let ((bot (lattice-bottom L))
        (top (lattice-top L))
        (join-str (symbol->string join-sym))
        (meet-str (symbol->string meet-sym)))
    (make-theory
      (list
        ;; Join: associative, commutative, idempotent; identity bot, annihilator top
        (make-named-axiom "ac-join"
          (string-append join-str ": AC, identity " join-str "(a, bot) = a, "
                         "annihilator " join-str "(a, top) = top")
          (make-ac-axiom join-sym #t bot top #f))
        ;; Meet: associative, commutative, idempotent; identity top, annihilator bot
        (make-named-axiom "ac-meet"
          (string-append meet-str ": AC, identity " meet-str "(a, top) = a, "
                         "annihilator " meet-str "(a, bot) = bot")
          (make-ac-axiom meet-sym #t top bot #f))
        ;; Absorption axioms (cross-operator)
        (make-named-axiom "absorption-join/meet"
          (string-append join-str "(a, " meet-str "(a, b)) = a")
          (make-absorption-axiom join-sym meet-sym))
        (make-named-axiom "absorption-meet/join"
          (string-append meet-str "(a, " join-str "(a, b)) = a")
          (make-absorption-axiom meet-sym join-sym)))
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
  (if (not (group? G))
      (error "group->theory: expected group" G))
  (if (not (symbol? op-symbol))
      (error "group->theory: op-symbol must be a symbol" op-symbol))
  (if (not (symbol? inv-symbol))
      (error "group->theory: inv-symbol must be a symbol" inv-symbol))
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
  "Project semiring S into a theory with 4 axioms: a single AC
(associative-commutative) axiom for addition, plus multiplicative identity,
associativity, and absorbing element.

Addition is associative AND commutative, so it collapses to one AC axiom that
flattens nested + nodes, drops the additive identity (semiring zero), and sorts
in a single terminating step — subsuming commutativity, associativity, and
identity for + at once. Multiplication is only associative (a semiring imposes
no commutativity on ×), so it keeps separate identity, associativity, and
absorbing axioms; associativity alone terminates, so there is no AC ping-pong to
collapse.

AC normalization is comparator-independent: it sorts once and cannot ping-pong,
unlike a pairwise commutativity + associativity pair, which fails to terminate
on three-or-more-leaf + terms under a comparator that orders compounds before
atoms.

Note: equal? is type-sensitive for numbers (0 and 0.0 are not equal?).
If terms mix exact and inexact numbers, construct the theory manually
with an appropriate predicate instead of using this projection.

Parameters:
  S : semiring
  plus-sym : symbol
  times-sym : symbol
Returns: theory
Category: algebra
Keywords: semiring, projection, theory, absorbing, AC normalization"
  (if (not (semiring? S))
      (error "semiring->theory: expected semiring" S))
  (if (not (symbol? plus-sym))
      (error "semiring->theory: plus-sym must be a symbol" plus-sym))
  (if (not (symbol? times-sym))
      (error "semiring->theory: times-sym must be a symbol" times-sym))
  (let ((z (semiring-zero S))
        (o (semiring-one S))
        (plus-str (symbol->string plus-sym))
        (times-str (symbol->string times-sym)))
    (make-theory
      (list
        ;; Addition: AC, identity +(a, 0) = a; not idempotent, no annihilator
        (make-named-axiom "ac-plus"
          (string-append plus-str ": AC, identity " plus-str "(a, 0) = a")
          (make-ac-axiom plus-sym #f z ac-absent #f))
        ;; Multiplicative identity
        (make-named-axiom "identity-times"
          (string-append times-str "(a, 1) = a")
          (make-identity-axiom times-sym
            (lambda (x) (equal? x o))))
        ;; Multiplicative absorbing element
        (make-named-axiom "absorbing-times"
          (string-append times-str "(a, 0) = 0")
          (make-absorbing-axiom times-sym
            (lambda (x) (equal? x z))))
        ;; Multiplicative associativity
        (make-named-axiom "associativity-times"
          (string-append times-str "(a, " times-str "(b, c)) = "
                         times-str "(" times-str "(a, b), c)")
          (make-associativity-axiom times-sym)))
      (list plus-sym times-sym))))

(define (ring->theory R plus-sym times-sym neg-sym)
  "Project ring R into a theory with 5 axioms: the 4 semiring axioms
plus involution for negation.

Parameters:
  R : ring
  plus-sym : symbol
  times-sym : symbol
  neg-sym : symbol
Returns: theory
Category: algebra
Keywords: ring, projection, theory, negation, involution"
  (if (not (ring? R))
      (error "ring->theory: expected ring" R))
  (if (not (symbol? neg-sym))
      (error "ring->theory: neg-sym must be a symbol" neg-sym))
  (let ((semi-th (semiring->theory (ring->semiring R) plus-sym times-sym))
        (neg-str (symbol->string neg-sym)))
    (theory-merge
      semi-th
      (make-theory
        (list (make-named-axiom "negate-involution"
                (string-append neg-str "(" neg-str "(a)) = a")
                (make-involution-axiom neg-sym)))
        '()))))

(define (field->theory F plus-sym times-sym neg-sym recip-sym)
  "Project field F into a theory with 6 axioms: the 5 ring axioms
plus involution for reciprocal.

Note: the reciprocal involution recip(recip(a)) = a is valid for all
nonzero elements.  Terms containing recip applied to zero are already
undefined in the field — the rewrite does not introduce unsoundness.
Callers must ensure terms are well-typed (no recip of zero).

Parameters:
  F : field
  plus-sym : symbol
  times-sym : symbol
  neg-sym : symbol
  recip-sym : symbol
Returns: theory
Category: algebra
Keywords: field, projection, theory, reciprocal, involution"
  (if (not (field? F))
      (error "field->theory: expected field" F))
  (if (not (symbol? recip-sym))
      (error "field->theory: recip-sym must be a symbol" recip-sym))
  (let ((ring-th (ring->theory (field->ring F) plus-sym times-sym neg-sym))
        (recip-str (symbol->string recip-sym)))
    (theory-merge
      ring-th
      (make-theory
        (list (make-named-axiom "reciprocal-involution"
                (string-append recip-str "(" recip-str "(a)) = a")
                (make-involution-axiom recip-sym)))
        '()))))

(define (heyting->theory H join-sym meet-sym)
  "Project Heyting algebra H into a theory via its underlying lattice.
Produces the same 4 AC lattice axioms (ac-join, ac-meet, and the two
absorption laws). Heyting implication is not included as a rewrite axiom —
it is a derived operation, not an equational simplification rule.

Parameters:
  H : heyting-algebra
  join-sym : symbol
  meet-sym : symbol
Returns: theory
Category: algebra
Keywords: Heyting, projection, theory, intuitionistic, lattice"
  (if (not (heyting-algebra? H))
      (error "heyting->theory: expected heyting-algebra" H))
  (if (not (symbol? join-sym))
      (error "heyting->theory: join-sym must be a symbol" join-sym))
  (if (not (symbol? meet-sym))
      (error "heyting->theory: meet-sym must be a symbol" meet-sym))
  (lattice->theory (heyting->lattice H) join-sym meet-sym))

(define (boolean->theory B join-sym meet-sym comp-sym)
  "Project Boolean algebra B into a theory with 7 axioms:

  ac-join   join is AC; identity bot, annihilator top, with complement folding
            (a join comp(a) = top) detected across the whole flat operand list
  ac-meet   meet is AC; identity top, annihilator bot, with complement folding
            (a meet comp(a) = bot)
  absorption-join/meet, absorption-meet/join   the two absorption laws
  negation  comp(comp(a)) = a, comp(bot) = top, comp(top) = bot
  de-morgan-join   comp(join(a,b)) = meet(comp(a), comp(b))
  de-morgan-meet   comp(meet(a,b)) = join(comp(a), comp(b))

Because the AC axioms fold complement detection over the *flattened* operand
list, n-way contradictions and tautologies reduce: a meet b meet comp(a) = bot,
a join b join comp(a) = top. De Morgan pushes negation toward the leaves
(negation normal form); the negation axiom then folds comp over bot/top.

This is NOT a Boolean decision procedure: there is no distributivity axiom, so
equivalences that require distributing (e.g. consensus, factoring) are not
detected. Use (wile algebra sat) when you need a decision procedure. Complement
and absorption are recognized between any operands sharing one flattened
operator, but cross-nesting that De Morgan or distribution would be needed to
expose is not.

Replaces the earlier 11-axiom projection (10 lattice + complement-involution);
the lattice fragment is now the 4-axiom AC lattice->theory shape, here rebuilt
with complement folding wired into the join/meet AC axioms.

Parameters:
  B : boolean-algebra
  join-sym : symbol
  meet-sym : symbol
  comp-sym : symbol
Returns: theory
Category: algebra
Keywords: Boolean, projection, theory, complement, De Morgan, negation normal form"
  (let ((bot (boolean-bottom B))
        (top (boolean-top B))
        (join-str (symbol->string join-sym))
        (meet-str (symbol->string meet-sym))
        (comp-str (symbol->string comp-sym)))
    (make-theory
      (list
        ;; AC join/meet with complement folding
        (make-named-axiom "ac-join"
          (string-append join-str ": AC, identity bot, annihilator top, "
                         join-str "(a, " comp-str "(a)) = top")
          (make-ac-axiom join-sym #t bot top comp-sym))
        (make-named-axiom "ac-meet"
          (string-append meet-str ": AC, identity top, annihilator bot, "
                         meet-str "(a, " comp-str "(a)) = bot")
          (make-ac-axiom meet-sym #t top bot comp-sym))
        ;; Absorption (cross-operator)
        (make-named-axiom "absorption-join/meet"
          (string-append join-str "(a, " meet-str "(a, b)) = a")
          (make-absorption-axiom join-sym meet-sym))
        (make-named-axiom "absorption-meet/join"
          (string-append meet-str "(a, " join-str "(a, b)) = a")
          (make-absorption-axiom meet-sym join-sym))
        ;; Boolean negation: double-negation + complement of the bounds
        (make-named-axiom "negation"
          (string-append comp-str "(" comp-str "(a)) = a; "
                         comp-str "(bot) = top; " comp-str "(top) = bot")
          (make-negation-axiom comp-sym bot top))
        ;; De Morgan (negation normal form)
        (make-named-axiom "de-morgan-join"
          (string-append comp-str "(" join-str "(a, b)) = "
                         meet-str "(" comp-str "(a), " comp-str "(b))")
          (make-de-morgan-axiom comp-sym join-sym meet-sym))
        (make-named-axiom "de-morgan-meet"
          (string-append comp-str "(" meet-str "(a, b)) = "
                         join-str "(" comp-str "(a), " comp-str "(b))")
          (make-de-morgan-axiom comp-sym meet-sym join-sym)))
      (list join-sym meet-sym))))

;; ─── Equivalence discovery ────────────────

(define discover-equivalences
  (case-lambda
    ((theory proto term)
     (discover-equivalences theory proto term 100))
    ((theory proto term fuel)
     (discover-equivalences* theory proto term fuel))))

(define (discover-equivalences* theory proto term fuel)
  "Find distinct normal forms by running TERM through the full theory
and each non-directional single-axiom sub-theory.  Returns a list of
(normal-form . trace) pairs, deduplicated by equal? on normal-form.

Directional axioms (e.g. associativity) are not explored individually —
they would produce combinatorial bracketings without reducing term size.

If a sub-theory normalizer exhausts its fuel, the partially-normalized
form is included in the results.  Callers can detect this by checking
the trace for fuel-exhausted-step? entries.

Parameters:
  theory : theory
  proto : term-protocol
  term : any
  fuel : integer
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
      (let ((norm (make-recursive-normalizer th proto fuel)))
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

;; ─── Boolean normalization facade ─────────
;;
;; Named entry points for a recursive-normalizer instance wired to the full
;; 7-axiom `boolean->theory`: AC-normalized join/meet (commutativity,
;; associativity, idempotence, identity, annihilation, and n-way complement
;; folding), the two absorption laws, Boolean negation (double-negation plus
;; complement of the bounds), and both De Morgan laws (negation normal form).
;;
;; Carrier choice: a two-element Boolean algebra over {#f, #t}. `boolean->theory`
;; reads only the carrier's bottom/top *values* (never its operations — the
;; projection is purely syntactic over opaque S-expression atoms), so the
;; carrier exists only to fix the bottom/top literals that surface in normal
;; forms. {#f, #t} makes a contradiction normalize to #f and a tautology to #t —
;; canonical Scheme booleans rather than the empty-list / singleton-set
;; artifacts a powerset carrier would leak. `#f` as a rewrite result is safe:
;; make-recursive-normalizer signals "no rewrite" with a private sentinel and
;; an empty trace, never with #f.
;;
;; Not a Boolean decision procedure: there is no distributivity axiom, so
;; distribution-dependent equivalences are not decided. See `boolean->theory`
;; for the precise boundary, and (wile algebra sat) for a decision procedure.
;;
;; Extracted from wile-goast's goast/boolean-simplify.scm, where this facade was
;; originally built for Go AST condition and belief selector normalization. The
;; wile-goast projections that convert Go AST nodes or belief selectors into
;; symbolic terms stay in wile-goast.

(define *symbolic-boolean-normalizer* #f)

(define (sexp-atom-compare a b)
  "Compare two S-expression atoms lexicographically by their printed form.
Serializes via `write` so any atom type (symbol, number, string, pair,
vector) orders consistently. Used as the commutativity tie-break for
`sexp-term-protocol` in the Boolean normalizer.

Parameters:
  a : any
  b : any
Returns: boolean
Category: algebra
Keywords: compare, lexicographic, atom, canonical order

See also: `symbolic-boolean-normalize', `sexp-term-protocol'."
  (let ((sa (let ((p (open-output-string))) (write a p) (get-output-string p)))
        (sb (let ((p (open-output-string))) (write b p) (get-output-string p))))
    (string<? sa sb)))

;; Default fuel for the cached Boolean normalizer. The recursive normalizer
;; charges roughly one unit per rewrite step, and a left-nested N-level term
;; needs ~N steps to flatten, so the budget bounds the term size/depth that
;; fully normalizes. 1000 covers far more than realistic Boolean conditions;
;; deeper terms partially normalize (still sound) — pass an explicit fuel to
;; raise the ceiling.
(define *symbolic-boolean-fuel* 1000)

(define (build-symbolic-boolean-normalizer fuel)
  "Construct a Boolean normalizer over the 2-element {#f, #t} Boolean algebra
with the given FUEL. boolean->theory reads only the carrier's bottom/top, so a
contradiction normalizes to #f and a tautology to #t.

Parameters:
  fuel : integer
Returns: procedure
Category: algebra"
  (let* ((B (make-boolean-algebra
              (lambda (a b)
                (or a b))                 ; join
              (lambda (a b)
                (and a b))                ; meet
              #f                          ; bottom
              #t                          ; top
              (lambda (a b)
                (or (not a) b))           ; leq: a <= b  (a implies b)
              (lambda (a)
                (not a))))                ; complement
         (th (boolean->theory B 'or 'and 'not))
         (proto (sexp-term-protocol sexp-atom-compare)))
    (make-recursive-normalizer th proto fuel)))

(define (ensure-symbolic-boolean-normalizer!)
  ;; Lazy singleton at the default fuel — built on first call, cached
  ;; thereafter. Partial failure during construction leaves the normalizer #f
  ;; so the next call retries.
  (unless *symbolic-boolean-normalizer*
    (set! *symbolic-boolean-normalizer*
          (build-symbolic-boolean-normalizer *symbolic-boolean-fuel*))))

(define (symbolic-boolean-normalize term . fuel-opt)
  "Normalize an S-expression boolean term under the Boolean-algebra
equational theory produced by `boolean->theory`. Treats `(and ...)`,
`(or ...)`, `(not ...)` as Boolean operators; every other form (symbol,
number, non-Boolean compound) is an opaque atom.

Applies the full `boolean->theory` (see that docstring): AC-normalized
`and`/`or` (commutativity, associativity, idempotence, identity,
annihilation, and n-way complement folding), absorption, Boolean negation
(double-negation and complement of the bounds), and both De Morgan laws.
A contradiction normalizes to #f (bottom); a tautology to #t (top).

This is NOT a decision procedure: there is no distributivity axiom, so
distribution-dependent equivalences are not detected. Use (wile algebra sat)
when you need one.

The optional FUEL argument bounds the rewrite iterations (default
*symbolic-boolean-fuel*, 1000). Deeply nested terms cost roughly one unit per
nesting level; if the budget is exhausted the result is partially normalized
(still truth-table sound) and the trace ends in a `fuel-exhausted-step?` entry.
Raise FUEL for very large terms.

Returns two values: the canonical normal form, and the rewrite trace
(a list of `<rewrite-step>` records documenting each rewrite applied).

Parameters:
  term : any
  fuel : integer (optional)
Returns: any
Category: algebra
Keywords: boolean, normalize, canonical form, simplify, negation normal form

Examples:
  (symbolic-boolean-normalize '(and x (or x y)))  ; absorption  => x
  (symbolic-boolean-normalize '(not (not x)))     ; negation    => x
  (symbolic-boolean-normalize '(and x (not x)))   ; complement  => #f
  (symbolic-boolean-normalize '(not (and x y)))   ; De Morgan   => (or (not x) (not y))

See also: `symbolic-boolean-equivalent?', `boolean->theory',
`make-recursive-normalizer'."
  (if (pair? fuel-opt)
      ((build-symbolic-boolean-normalizer (car fuel-opt)) term)
      (begin
        (ensure-symbolic-boolean-normalizer!)
        (*symbolic-boolean-normalizer* term))))

(define (symbolic-boolean-equivalent? term1 term2 . fuel-opt)
  "Test whether two S-expression boolean terms normalize to the same
canonical form under `symbolic-boolean-normalize`. Both terms are
normalized and compared with `equal?`.

This decides equivalence up to the `boolean->theory` axioms (see that
docstring) — including De Morgan and complement laws, so De Morgan duals
and shared contradictions/tautologies now compare equal. It is NOT full
Boolean-algebra equivalence: without distributivity, distribution-dependent
pairs may still compare `#f`. Use (wile algebra sat) for a decision procedure.

The optional FUEL argument is forwarded to both normalizations. A `#f` result
on two genuinely-equivalent very large terms can be a false negative caused by
fuel exhaustion (one side normalized further than the other before the budget
ran out); raise FUEL in that case.

Parameters:
  term1 : any
  term2 : any
  fuel : integer (optional)
Returns: boolean
Category: algebra
Keywords: boolean, equivalent, equational theory

Examples:
  (symbolic-boolean-equivalent? '(and a b) '(and b a))            => #t
  (symbolic-boolean-equivalent? '(not (and a b)) '(or (not a) (not b))) => #t
  (symbolic-boolean-equivalent? '(or x y) '(and x y))            => #f

See also: `symbolic-boolean-normalize'."
  (let ((norm (if (pair? fuel-opt)
                  (build-symbolic-boolean-normalizer (car fuel-opt))
                  (begin
                    (ensure-symbolic-boolean-normalizer!)
                    *symbolic-boolean-normalizer*))))
    (let-values (((n1 _t1) (norm term1))
                 ((n2 _t2) (norm term2)))
      (equal? n1 n2))))
