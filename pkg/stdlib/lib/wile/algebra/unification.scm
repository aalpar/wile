;;; unification.scm — AC-matching and AC-unification.
;;;
;;; Term protocol contract: term-compare is the boolean "sorts before"
;;; predicate (wile algebra rewrite) documents, and it must be a strict total
;;; order consistent with equal? modulo the AC-equivalence induced by the
;;; caller's theory. Equality of two atoms, or of an atom and a compound, is
;;; derived from it by antisymmetry (`term-equal?`): they are equal when
;;; neither sorts before the other. Two compounds are compared structurally,
;;; modulo AC, recursing down to atoms.

(define-record-type <pattern-var>
  (make-pattern-var name)
  pattern-var?
  (name pattern-var-name))

(define-record-type <substitution>
  (make-substitution bindings)
  substitution?
  (bindings substitution-bindings))

(define empty-substitution (make-substitution '()))

(define (substitution-lookup sub var)
  "Return the term bound to VAR in SUB, or #f if unbound. Var identity is
by pattern-var-name (symbols compared with eq?).

Parameters:
  sub : <substitution>
  var : <pattern-var>
Returns: term | #f
Category: algebra
Keywords: substitution, lookup, unification"
  (let loop ((xs (substitution-bindings sub)))
    (cond
      ((null? xs) #f)
      ((eq? (pattern-var-name (caar xs))
            (pattern-var-name var))
       (cdar xs))
      (else (loop (cdr xs))))))

(define (substitution-compose s1 s2)
  "Merge two substitutions. Returns a new <substitution> if bindings are
compatible, #f on conflict (same var bound to term-unequal values).
Does NOT perform occurs-check on binding targets; that is the caller's
responsibility via substitution-apply.

Parameters:
  s1 : <substitution>
  s2 : <substitution>
Returns: <substitution> | #f
Category: algebra
Keywords: substitution, compose, merge, unification"
  (let loop ((xs (substitution-bindings s2))
             (acc (substitution-bindings s1)))
    (cond
      ((null? xs) (make-substitution acc))
      (else
       (let* ((pair (car xs))
              (var (car pair))
              (val (cdr pair))
              (existing (substitution-lookup (make-substitution acc) var)))
         (cond
           ((not existing)
            (loop (cdr xs) (cons pair acc)))
           ;; equal?, like bind-or-check; term-equal? coincides with it on atoms
           ;; by its contract check, and differs on compounds only modulo AC.
           ((equal? existing val)
            (loop (cdr xs) acc))
           (else #f)))))))

(define (substitution-apply sub proto term)
  "Return TERM with each <pattern-var> leaf replaced by its binding in SUB,
rebuilding compound terms via the protocol's term-make-term. Unbound vars
are returned unchanged.

Parameters:
  sub : <substitution>
  proto : <term-protocol>
  term : any
Returns: term
Category: algebra
Keywords: substitution, apply, rewrite, unification"
  (cond
    ((pattern-var? term)
     (or (substitution-lookup sub term) term))
    ((term-compound? proto term)
     (term-make-term proto term
       (map (lambda (a) (substitution-apply sub proto a))
            (term-get-operands proto term))))
    (else term)))

(define (parse-pattern expr)
  "Convert EXPR from sexpr with ?-prefix convention to a pattern using
<pattern-var> records. Symbols starting with #\\? become pattern variables;
repeated ?-names intern to one record (name-based identity).

Parameters:
  expr : any
Returns: pattern (sexpr possibly containing <pattern-var> records)
Category: algebra
Keywords: pattern, parse, match, unification"
  (let ((interned '()))
    (define (var-name-of sym)
      (let* ((s (symbol->string sym))
             (n (string-length s)))
        (and (> n 1)
             (char=? #\? (string-ref s 0))
             (string->symbol (substring s 1 n)))))
    (define (walk x)
      (cond
        ((pair? x)
         (cons (walk (car x)) (walk (cdr x))))
        ((null? x) '())
        ((symbol? x)
         (let ((nm (var-name-of x)))
           (cond
             ((not nm) x)
             ((assq nm interned) => cdr)
             (else
              (let ((v (make-pattern-var nm)))
                (set! interned (cons (cons nm v) interned))
                v)))))
        (else x)))
    (walk expr)))

;;; -------------------------------------------------------------------------
;;; Diophantine basis: minimal non-negative integer solutions of a·u = b·v.
;;; Standalone primitive — no dependency on match/unify. Useful for AC
;;; matching/unification (Stickel 1981, Contejean–Devie 1994), Petri-net
;;; place invariants, and integer-programming feasibility kernels.
;;; -------------------------------------------------------------------------

(define (exact-nonneg-integer? x)
  "True iff X is an exact non-negative integer.

Parameters:
  x : any
Returns: boolean
Category: algebra
Keywords: integer, predicate, validation"
  (and (integer? x) (exact? x) (>= x 0)))

(define (diophantine-basis a b)
  "Enumerate minimal non-negative integer solutions of a·u = b·v, where
a∈ℕᵐ, b∈ℕⁿ. Returns a list of (u . v) pairs; u and v are integer lists
of length m and n respectively.

Algorithm: BFS over ℕ^(m+n) from the zero vector, prune by domination
and by sign of the residual a·u − b·v. Terminates via Dickson's lemma
(finitely many minimal ℕ-vectors). The 1×1 case reduces to a single
solution via gcd: x = b/g, y = a/g where g = gcd(a,b).

Parameters:
  a : list of non-negative integers (length m ≥ 1)
  b : list of non-negative integers (length n ≥ 1)
Returns: list of (u . v) where u, v are integer lists
Category: algebra
Keywords: diophantine, linear, basis, unification, combinatorics, Petri"
  (unless (and (list? a) (every exact-nonneg-integer? a))
    (error "diophantine-basis: expected non-negative integer list" a))
  (unless (and (list? b) (every exact-nonneg-integer? b))
    (error "diophantine-basis: expected non-negative integer list" b))
  (when (or (null? a) (null? b))
    (error "diophantine-basis: empty coefficient vector" a b))
  (let ((m (length a)) (n (length b)))
    (contejean-devie-bfs a b m n)))

;;; -------------------------------------------------------------------------
;;; AC matching (Phase 3): one-sided pattern-to-subject match modulo the
;;; associative-commutative axioms of a theory. Returns list<substitution>
;;; — one per solution in a CSU-like set (may be empty).
;;; -------------------------------------------------------------------------

;; Scan theory once, compute list of op symbols that are both commutative
;; and associative (the AC-ops).
(define (ac-ops-of theory)
  (filter
    (lambda (op)
      (let ((is-comm? #f) (is-assoc? #f))
        (for-each
          (lambda (na)
            (let ((ax (named-axiom-axiom na)))
              (cond
                ((and (commutativity-axiom? ax)
                      (eq? op (commutativity-axiom-op ax)))
                 (set! is-comm? #t))
                ((and (associativity-axiom? ax)
                      (eq? op (associativity-axiom-op ax)))
                 (set! is-assoc? #t)))))
          (theory-axioms theory))
        (and is-comm? is-assoc?)))
    (theory-associative-ops theory)))

;; Membership test against the AC-ops list.
(define (ac-op? op ac-ops) (and (memq op ac-ops) #t))

(define (ac-match pattern subject theory proto)
  "Match PATTERN against SUBJECT modulo the AC axioms of THEORY, using term
protocol PROTO. Returns a list of <substitution> — one per solution in a
CSU-like result set; empty list means no match. Pattern variables are
<pattern-var> records (see parse-pattern); other terms are protocol-level
terms. THEORY's associative-ops that also have a commutativity axiom are
treated as AC operators; all other compound terms match positionally.
Identical substitutions are returned once. Raises if any compound subterm of
PATTERN or SUBJECT has operands that are not a proper list, e.g. (g . a).

Parameters:
  pattern : term (may contain <pattern-var> records)
  subject : term (ground under PROTO)
  theory  : <theory>
  proto   : <term-protocol>
Returns: list of <substitution>
Category: algebra
Keywords: AC matching, pattern matching, associative, commutative, unification"
  (unless (theory? theory)
    (error "ac-match: expected theory" theory))
  (unless (term-protocol? proto)
    (error "ac-match: expected term-protocol" proto))
  (assert-proper-term "ac-match" pattern proto)
  (assert-proper-term "ac-match" subject proto)
  (let ((ac-ops (ac-ops-of theory)))
    (match-rec pattern subject empty-substitution ac-ops proto)))

;; Raise unless every compound subterm of TERM has a proper operand list and
;; no compound contains itself. Checked once per public entry, so the
;; recursion below can take operands with car/cdr unguarded and terminates:
;; every term it reaches is a subterm of a checked input, a binding to one,
;; or an (op . subset) it built from a proper list. list? refuses a circular
;; operand spine; ANCESTORS, the compounds from the root down to the current
;; node (an eq? table, so a deep chain stays linear), refuses a term that is
;; its own descendant, such as c = (g c) built by set-car!. Tracking the path
;; rather than every node visited keeps shared subterms legal.
(define (assert-proper-term site term proto)
  (let ((ancestors (make-eq-hashtable)))
    (let walk ((term term))
      (when (and (not (pattern-var? term)) (term-compound? proto term))
        (when (hashtable-contains? ancestors term)
          (error (string-append site ": operands are circular") term))
        (let ((operands (term-get-operands proto term)))
          (unless (list? operands)
            (error (string-append site ": operands are not a proper list") term))
          (hashtable-set! ancestors term #t)
          (for-each walk operands)
          (hashtable-delete! ancestors term))))))

;; Collapse nested (op ...) applications under an AC operator into a flat
;; list of operand terms. Non-compound terms return (list term); compound
;; terms with non-matching operator are left as a single element.
(define (flatten-ac term op proto)
  (cond
    ((not (term-compound? proto term)) (list term))
    ((eq? (term-get-operator proto term) op)
     (apply append
       (map (lambda (a) (flatten-ac a op proto))
            (term-get-operands proto term))))
    (else (list term))))

;; Core recursion. Dispatches on PATTERN shape. Compound terms under an
;; AC operator are flattened and matched as multisets via match-ac;
;; non-AC compounds match positionally.
(define (match-rec p s sub ac-ops proto)
  (cond
    ((pattern-var? p) (bind-or-check p s sub))
    ((term-compound? proto p)
     (cond
       ((not (term-compound? proto s)) '())
       ((not (eq? (term-get-operator proto p) (term-get-operator proto s))) '())
       ;; Deduplicated per AC node, before an enclosing node multiplies the
       ;; results: repeated subject operands yield identical assignments.
       ((ac-op? (term-get-operator proto p) ac-ops)
        (deduplicate-substitutions
          (match-ac (term-get-operator proto p)
                    (flatten-ac p (term-get-operator proto p) proto)
                    (flatten-ac s (term-get-operator proto p) proto)
                    sub ac-ops proto)))
       (else
        (match-positional (term-get-operands proto p)
                          (term-get-operands proto s)
                          sub ac-ops proto))))
    (else
     (if (term-equal? proto p s) (list sub) '()))))

;; AC-case: direct backtracking over assignments of pattern operands to
;; subject operands. Correct (enumerates all CSU elements) but exponential;
;; Phase 4 will add a matrix-permanent feasibility prune.
;;
;; Two binding strategies for a pattern-var head:
;;   Case A: bind head to a single subject element.
;;   Case B: bind head (if currently free) to a sub-multiset of the subject
;;           operands, re-wrapped as (op …). Only fires when there's enough
;;           subject-arity slack for the remaining pattern operands.
(define (match-ac op pat-ops subj-ops sub ac-ops proto)
  (cond
    ((null? pat-ops) (if (null? subj-ops) (list sub) '()))
    (else
     (let ((head (car pat-ops)) (rest (cdr pat-ops)))
       (cond
         ;; Non-var head with mismatched arity can't consume multiple
         ;; subject elements — bail when sizes don't match rest's budget.
         ((and (not (pattern-var? head))
               (not (= (length subj-ops) (length pat-ops))))
          '())
         ((pattern-var? head)
          (append
            ;; Case A: single-element binding for head.
            (apply append
              (map (lambda (i)
                     (let* ((chosen (list-ref subj-ops i))
                            (rem (remove-at subj-ops i))
                            (partial (bind-or-check head chosen sub)))
                       (apply append
                         (map (lambda (s1)
                                (match-ac op rest rem s1 ac-ops proto))
                              partial))))
                   (iota (length subj-ops))))
            ;; Case B: multi-element binding — head currently free, and
            ;; the subject has enough operands that a 2+-element binding
            ;; still leaves |rest| operands for the remaining pattern.
            (if (and (not (substitution-lookup sub head))
                     (>= (length subj-ops) 2)
                     (>= (length subj-ops) (+ (length rest) 2)))
                (apply append
                  (map (lambda (subset)
                         (let* ((binding (term-make-term-variadic
                                           proto op subset))
                                (rem (list-difference subj-ops subset))
                                (partial (bind-or-check head binding sub)))
                           (apply append
                             (map (lambda (s1)
                                    (match-ac op rest rem s1 ac-ops proto))
                                  partial))))
                       (proper-subsets-size>=2 subj-ops)))
                '())))
         (else
          ;; Non-var head: recurse match-rec against each subject element.
          (apply append
            (map (lambda (i)
                   (let* ((chosen (list-ref subj-ops i))
                          (rem (remove-at subj-ops i))
                          (partial (match-rec head chosen sub ac-ops proto)))
                     (apply append
                       (map (lambda (s1)
                              (match-ac op rest rem s1 ac-ops proto))
                            partial))))
                 (iota (length subj-ops))))))))))

;; Remove the element at index I from XS.
(define (remove-at xs i)
  (cond ((zero? i) (cdr xs))
        (else (cons (car xs) (remove-at (cdr xs) (- i 1))))))

;; All subsets of XS with size in [2, |xs|-1] (proper, non-trivial).
(define (proper-subsets-size>=2 xs)
  (let ((n (length xs)))
    (filter (lambda (s)
              (and (>= (length s) 2) (< (length s) n)))
            (all-subsets xs))))

;; Enumerate all subsets of XS (power set) as a list of lists.
(define (all-subsets xs)
  (cond ((null? xs) '(()))
        (else (let ((rest-subs (all-subsets (cdr xs))))
                (append rest-subs
                        (map (lambda (s) (cons (car xs) s)) rest-subs))))))

;; Multiset difference: XS minus YS element-by-element (using equal?).
(define (list-difference xs ys)
  (let loop ((xs xs) (acc '()) (ys ys))
    (cond
      ((null? xs) (reverse acc))
      ((member (car xs) ys)
       (loop (cdr xs) acc (remove-first (car xs) ys)))
      (else (loop (cdr xs) (cons (car xs) acc) ys)))))

;; Remove the first occurrence of X from XS (using equal?).
(define (remove-first x xs)
  (cond ((null? xs) '())
        ((equal? x (car xs)) (cdr xs))
        (else (cons (car xs) (remove-first x (cdr xs))))))

;; Build a compound term (op . args) in protocol PROTO. The protocol's
;; make-term needs an existing term as a template to preserve metadata;
;; we synthesize one via (cons op args) which is compatible with the
;; sexp-term-protocol's default (lambda (term new-args) (cons (car term) new-args)).
(define (term-make-term-variadic proto op args)
  (term-make-term proto (cons op args) args))

;; Variable binding: bind VAR↦SUBJECT if unbound, else check consistency.
;; The consistency check is equal?; term-equal? (unify-rec) agrees with it on
;; atoms by the contract check there, and differs on compounds only modulo AC.
(define (bind-or-check var subject sub)
  (let ((existing (substitution-lookup sub var)))
    (cond
      ((not existing)
       (list (make-substitution
               (cons (cons var subject) (substitution-bindings sub)))))
      ((equal? existing subject) (list sub))
      (else '()))))

;; Positional match: fixed-arity zip between pattern and subject operand
;; lists. Returns the (possibly many) substitutions that extend SUB.
(define (match-positional ps ss sub ac-ops proto)
  (cond
    ((and (null? ps) (null? ss)) (list sub))
    ((or (null? ps) (null? ss)) '())
    (else
     (let ((partial (match-rec (car ps) (car ss) sub ac-ops proto)))
       (apply append
         (map (lambda (s1)
                (match-positional (cdr ps) (cdr ss) s1 ac-ops proto))
              partial))))))

(define (contejean-devie-bfs a b m n)
  ;; Vectors represented as lists of length m (u-side) or n (v-side).
  ;; Nodes are (u . v); residual r = a·u − b·v drives expansion direction.
  (define (zeros k) (make-list k 0))
  (define (dot xs ys) (apply + (map * xs ys)))
  (define (residual u v) (- (dot a u) (dot b v)))
  (define (bump xs i)
    ;; Return XS with the i-th element incremented by 1.
    (let loop ((k 0) (xs xs) (acc '()))
      (if (= k i)
          (append (reverse acc) (cons (+ 1 (car xs)) (cdr xs)))
          (loop (+ k 1) (cdr xs) (cons (car xs) acc)))))
  (define (vec-le? u v) (every <= u v))
  (define (dominated? u v emitted)
    ;; True iff some emitted (u₀ . v₀) satisfies u₀ ≤ u and v₀ ≤ v
    ;; componentwise — i.e., (u,v) is not minimal.
    (any (lambda (e)
           (and (vec-le? (car e) u) (vec-le? (cdr e) v)))
         emitted))
  (let loop ((frontier (list (cons (zeros m) (zeros n))))
             (emitted '()))
    (cond
      ((null? frontier) (reverse emitted))
      (else
       (let* ((node (car frontier))
              (u (car node))
              (v (cdr node))
              (r (residual u v)))
         (cond
           ;; Solution node (non-zero): emit and stop expanding
           ((and (zero? r) (or (any positive? u) (any positive? v)))
            (if (dominated? u v emitted)
                (loop (cdr frontier) emitted)
                (loop (cdr frontier) (cons (cons u v) emitted))))
           ;; Dead-end (dominated by a known minimal solution): prune
           ((dominated? u v emitted)
            (loop (cdr frontier) emitted))
           (else
            ;; Expand: u-bumps if r ≤ 0; v-bumps if r ≥ 0.
            (let* ((u-bumps
                    (if (<= r 0)
                        (map (lambda (i) (cons (bump u i) v))
                             (iota m))
                        '()))
                   (v-bumps
                    (if (>= r 0)
                        (map (lambda (j) (cons u (bump v j)))
                             (iota n))
                        '())))
              (loop (append (cdr frontier) u-bumps v-bumps)
                    emitted)))))))))

;;; -------------------------------------------------------------------------
;;; AC-unification (Phase 5): two-sided unification modulo AC axioms.
;;; Returns a CSU (complete set of unifiers) as list<substitution>; empty
;;; means no unifier exists. Dispatches by operator AC-ness per unify-rec.
;;; -------------------------------------------------------------------------

(define (ac-unify t1 t2 theory proto)
  "Unify T1 and T2 modulo the AC axioms of THEORY using term protocol PROTO.
Returns a list of <substitution> — a CSU (complete set of unifiers);
empty list means no unifier exists. Pattern variables are <pattern-var>
records; other terms are protocol-level terms. Non-AC compound terms are
unified positionally; compound terms whose operator is both associative
and commutative (per THEORY) are unified modulo multiset equality, nested
AC operands included. Raises if any compound subterm of T1 or T2 has
operands that are not a proper list, e.g. (g . a).

Parameters:
  t1    : term (may contain <pattern-var> records)
  t2    : term (may contain <pattern-var> records)
  theory : <theory>
  proto  : <term-protocol>
Returns: list of <substitution>
Category: algebra
Keywords: AC unification, unify, associative, commutative, Stickel, Robinson"
  (unless (theory? theory)
    (error "ac-unify: expected theory" theory))
  (unless (term-protocol? proto)
    (error "ac-unify: expected term-protocol" proto))
  (assert-proper-term "ac-unify" t1 proto)
  (assert-proper-term "ac-unify" t2 proto)
  (let ((ac-ops (ac-ops-of theory)))
    (unify-rec t1 t2 empty-substitution ac-ops proto)))

;; Core recursion for unification. Walks bindings via resolve, then
;; dispatches on shape: var ↦ bind, compound/compound ↦ positional or AC,
;; ground/ground ↦ term-equal? (antisymmetry over the boolean term-compare).
(define (unify-rec t1 t2 sub ac-ops proto)
  (let ((t1* (resolve t1 sub))
        (t2* (resolve t2 sub)))
    (cond
      ((and (pattern-var? t1*) (pattern-var? t2*)
            (eq? (pattern-var-name t1*) (pattern-var-name t2*)))
       (list sub))
      ((pattern-var? t1*) (bind-with-occurs-check t1* t2* sub proto))
      ((pattern-var? t2*) (bind-with-occurs-check t2* t1* sub proto))
      ((and (term-compound? proto t1*) (term-compound? proto t2*))
       (cond
         ((not (eq? (term-get-operator proto t1*)
                    (term-get-operator proto t2*))) '())
         ((ac-op? (term-get-operator proto t1*) ac-ops)
          (unify-ac (term-get-operator proto t1*)
                    (flatten-ac t1* (term-get-operator proto t1*) proto)
                    (flatten-ac t2* (term-get-operator proto t1*) proto)
                    sub ac-ops proto))
         (else
          (unify-positional (term-get-operands proto t1*)
                            (term-get-operands proto t2*)
                            sub ac-ops proto))))
      (else
       (if (term-equal? proto t1* t2*) (list sub) '())))))

;; Walk a chain of var↦var bindings to the final value (or original term).
(define (resolve t sub)
  (cond
    ((and (pattern-var? t) (substitution-lookup sub t))
     (resolve (substitution-lookup sub t) sub))
    (else t)))

;; Extend SUB with VAR↦TERM unless TERM mentions VAR (occurs-check).
(define (bind-with-occurs-check var term sub proto)
  (cond
    ((occurs? var term sub proto) '())
    (else (list (make-substitution
                  (cons (cons var term) (substitution-bindings sub)))))))

;; True iff VAR appears inside TERM (walking bindings via resolve).
(define (occurs? var term sub proto)
  (let ((t (resolve term sub)))
    (cond
      ((and (pattern-var? t) (eq? (pattern-var-name t) (pattern-var-name var)))
       #t)
      ((term-compound? proto t)
       (any (lambda (a) (occurs? var a sub proto))
            (term-get-operands proto t)))
      (else #f))))

;; Positional (fixed-arity) unify: zip unify-rec across two operand lists.
(define (unify-positional t1s t2s sub ac-ops proto)
  (cond
    ((and (null? t1s) (null? t2s)) (list sub))
    ((or (null? t1s) (null? t2s)) '())
    (else
     (let ((partial (unify-rec (car t1s) (car t2s) sub ac-ops proto)))
       (apply append
         (map (lambda (s1)
                (unify-positional (cdr t1s) (cdr t2s) s1 ac-ops proto))
              partial))))))

;; AC-unifier: ground case uses multiset equality modulo AC; variable case
;; delegates to Stickel reduction via unify-ac-stickel.
(define (unify-ac op t1-ops t2-ops sub ac-ops proto)
  (cond
    ((and (all-ground? t1-ops proto) (all-ground? t2-ops proto))
     (if (multiset-equal? t1-ops t2-ops ac-ops proto)
         (list sub)
         '()))
    (else
     (unify-ac-stickel op t1-ops t2-ops sub ac-ops proto))))

;; True iff no element of TS contains a <pattern-var>.
(define (all-ground? ts proto)
  (every (lambda (t) (not (has-var? t proto))) ts))

;; True iff T contains a <pattern-var> anywhere.
(define (has-var? t proto)
  (cond
    ((pattern-var? t) #t)
    ((term-compound? proto t)
     (any (lambda (a) (has-var? a proto)) (term-get-operands proto t)))
    (else #f)))

;; True iff A and B are equal under PROTO's boolean term-compare: neither
;; sorts before the other. Sound only for a strict total order, so the two
;; contract violations that would make a tie lie are raised, not matched:
;;   - the comparator ranks two atoms that are equal? (checked whenever the
;;     pair is not two compounds, so the deep equal? is never paid on
;;     compound pairs);
;;   - the comparator ties distinct atoms, or an atom and a compound (a
;;     partial comparator answering #f outside its domain, or a written-form
;;     collision such as two records). A compound/compound tie is accepted,
;;     but no caller asks: multiset-equal? decides compound pairs
;;     structurally, and match-rec / unify-rec reach here only when one
;;     side is an atom.
;; On atoms this makes the comparator's equality coincide with equal?, which is
;; what bind-or-check and substitution-compose use. Lazy and lean on the hot
;; path: b<a is asked only when a<b is #f, and the compound test and equal?
;; run only on a tie or when the comparator ranks the pair.
(define (term-equal? proto a b)
  (cond
    ((term-compare proto a b) (ranked-apart? proto a b))
    ((term-compare proto b a) (ranked-apart? proto a b))
    ((equal? a b) #t)
    ((and (term-compound? proto a) (term-compound? proto b)) #t)
    (else
     (error "term-equal?: comparator ties distinct atoms (a boolean comparator must be total over atoms, and over atom/compound pairs)" a b))))

;; The comparator ranked A and B apart: raise if that contradicts equal? on a
;; non-compound pair, else report them unequal (#f).
(define (ranked-apart? proto a b)
  (if (and (equal? a b)
           (not (and (term-compound? proto a) (term-compound? proto b))))
      (error "term-equal?: comparator inconsistent with equal?: it ranks equal atoms" a))
  #f)

;; True iff the ground operand lists XS and YS are equal as multisets modulo
;; AC. Compound operands with an equal? partner cancel first, so only the
;; remainder pays for the AC-aware comparison. Top-level atoms are never
;; cancelled, so term-equal?'s contract checks see each of them; atoms inside
;; a cancelled compound are not checked. Cancelling greedily is complete
;; because AC-equality is an equivalence.
(define (multiset-equal? xs ys ac-ops proto)
  (let cancel ((xs xs) (kept '()) (ys ys))
    (cond
      ((null? xs) (multiset-equal-modulo-ac? (reverse kept) ys ac-ops proto))
      ((and (term-compound? proto (car xs)) (member (car xs) ys))
       (cancel (cdr xs) kept (remove-first (car xs) ys)))
      (else (cancel (cdr xs) (cons (car xs) kept) ys)))))

;; Greedy multiset equality over what multiset-equal? did not cancel. A
;; compound pair is decided by unify-rec, which recurses AC-aware and, both
;; sides being ground, binds nothing; the comparator decides the other pairs
;; (term-equal?). A written-form comparator ranks (* a b) and (* b a) apart,
;; so consulting it on compounds would miss AC-equality one level down. The
;; shape of X is tested once per scan of YS, so an atom X costs what the
;; comparator costs.
(define (multiset-equal-modulo-ac? xs ys ac-ops proto)
  (cond
    ((and (null? xs) (null? ys)) #t)
    ((or (null? xs) (null? ys)) #f)
    (else
     (let* ((x (car xs))
            (same?
             (if (term-compound? proto x)
                 (lambda (y)
                   (if (term-compound? proto y)
                       (pair? (unify-rec x y empty-substitution ac-ops proto))
                       (term-equal? proto x y)))
                 (lambda (y) (term-equal? proto x y))))
            (match-idx (find-index same? ys)))
       (and match-idx
            (multiset-equal-modulo-ac? (cdr xs) (remove-at ys match-idx)
                                       ac-ops proto))))))

;; Return the index of the first element of XS matching PRED, or #f.
(define (find-index pred xs)
  (let loop ((xs xs) (i 0))
    (cond
      ((null? xs) #f)
      ((pred (car xs)) i)
      (else (loop (cdr xs) (+ i 1))))))

;; Stickel reduction (v1: 0/1 multiplicity only — bijective pairing).
;; When |t1-ops| = |t2-ops|, each LHS slot is paired 1-to-1 with an RHS
;; slot; enumerate all such bijections and unify pairwise via unify-rec.
;; Conflicts (nonlinear bindings, e.g. x+x unifying with a+b) are pruned
;; by unify-rec's bind-with-occurs-check / substitution-compose path.
;; Non-unit multiplicities — which would bind a variable to an (op …)
;; compound from the opposite side — are deferred per the design doc's
;; Future extensions. Full Stickel with diophantine-basis over the
;; abstracted system is the follow-up path.
(define (unify-ac-stickel op t1-ops t2-ops sub ac-ops proto)
  (cond
    ((not (= (length t1-ops) (length t2-ops))) '())
    (else
     (let ((results
             (apply append
               (map (lambda (rhs-perm)
                      (unify-pairs t1-ops rhs-perm sub ac-ops proto))
                    (permutations t2-ops)))))
       (deduplicate-substitutions results)))))

;; Unify a zipped list of (lhs . rhs) pairs, threading SUB through.
;; Returns list<substitution> — empty on conflict at any pair.
(define (unify-pairs lhs-list rhs-list sub ac-ops proto)
  (cond
    ((and (null? lhs-list) (null? rhs-list)) (list sub))
    ((or (null? lhs-list) (null? rhs-list)) '())
    (else
     (let ((partial (unify-rec (car lhs-list) (car rhs-list) sub ac-ops proto)))
       (apply append
         (map (lambda (s1)
                (unify-pairs (cdr lhs-list) (cdr rhs-list) s1 ac-ops proto))
              partial))))))

;; All permutations of XS as a list of lists. O(|xs|!); acceptable for
;; the small operand counts typical in AC unification.
(define (permutations xs)
  (cond
    ((null? xs) '(()))
    (else
     (apply append
       (map (lambda (x)
              (map (lambda (p) (cons x p))
                   (permutations (remove-first x xs))))
            xs)))))

;; Deduplicate a list of substitutions by binding multiset. Keeps the
;; first occurrence, in order. Bindings are canonicalised via pattern-var
;; name (mirrors substitution-compose's identity convention). The seen set
;; is an equal? table, so the pass is linear in the result count; it runs at
;; every AC node, where a member scan made it quadratic.
(define (deduplicate-substitutions subs)
  (let ((seen (make-equal-hashtable)))
    (let loop ((subs subs) (acc '()))
      (cond
        ((null? subs) (reverse acc))
        (else
         (let ((key (sub->canonical (car subs))))
           (cond
             ((hashtable-contains? seen key)
              (loop (cdr subs) acc))
             (else
              (hashtable-set! seen key #t)
              (loop (cdr subs) (cons (car subs) acc))))))))))

;; Canonical representation of a substitution as a sorted assoc list
;; keyed by pattern-var-name; used for membership comparison.
(define (sub->canonical sub)
  (let ((bs (map (lambda (b) (cons (pattern-var-name (car b)) (cdr b)))
                 (substitution-bindings sub))))
    (sort
      (lambda (x y)
        (string<? (symbol->string (car x)) (symbol->string (car y))))
      bs)))
