;;; unification.scm — AC-matching and AC-unification.
;;;
;;; Term protocol contract: term-compare must be a total order consistent
;;; with equal? modulo the AC-equivalence induced by the caller's theory.

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
  (let ((ac-ops (ac-ops-of theory)))
    (match-rec pattern subject empty-substitution ac-ops proto)))

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
       ((ac-op? (term-get-operator proto p) ac-ops)
        (match-ac (term-get-operator proto p)
                  (flatten-ac p (term-get-operator proto p) proto)
                  (flatten-ac s (term-get-operator proto p) proto)
                  sub ac-ops proto))
       (else
        (match-positional (term-get-operands proto p)
                          (term-get-operands proto s)
                          sub ac-ops proto))))
    (else
     (if (zero? (term-compare proto p s)) (list sub) '()))))

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
