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
