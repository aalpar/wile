;;; (wile algebra semiring) — Semirings
;;;
;;; A semiring (S, +, ×, 0, 1) has:
;;; - (S, +, 0) is a commutative monoid
;;; - (S, ×, 1) is a monoid
;;; - × distributes over +
;;; - 0 annihilates ×: 0 × a = a × 0 = 0

(define-record-type <semiring>
  (make-semiring* plus-fn times-fn zero one carrier eq?-fn)
  semiring?
  (plus-fn  semiring-plus-fn)
  (times-fn semiring-times-fn)
  (zero     semiring-zero)
  (one      semiring-one)
  (carrier  semiring-carrier)
  (eq?-fn   semiring-eq?-fn))

(define (make-semiring plus times zero one . opts)
  "Construct a semiring from PLUS, TIMES, ZERO, and ONE.\nPLUS must be associative and commutative with ZERO as identity.\nTIMES must be associative with ONE as identity and must\ndistribute over PLUS. ZERO must annihilate TIMES from both sides.\n\nThe trailing alist OPTS accepts the following keys:\n  (carrier . SYM) — declares the carrier type. The symbol drives\n    consumer-side fast-path eligibility. Default #f (no fast path).\n    Vocabulary: 'big-int, 'integer, 'rational, 'real, 'complex,\n    'boolean, 'log-float, 'modular, 'saturating, 'opaque.\n  (eq? . PROC) — declares the carrier's equality predicate. PROC\n    is a binary procedure on carrier values; used by consumer\n    libraries (e.g. `(wile algebra graph)') for convergence\n    detection. Default `equal?'. Override when `equal?' is wrong\n    or expensive on the carrier — e.g. tolerance-based equality\n    on log-space floats, modular-aware equality on ℤ/Pℤ.\n\nExamples:\n  (let ((S (make-semiring + * 0 1)))\n    (semiring-plus S 3 4))   => 7\n  (let ((S (make-semiring + * 0 1 '(carrier . big-int))))\n    (semiring-carrier S))    => big-int\n  (let ((S (make-semiring + * 0 1 (cons 'eq? =))))\n    (semiring-eq? S 1 1.0))  => #t\n\nParameters:\n  plus : procedure\n  times : procedure\n  zero : any\n  one : any\n  opts : alist\nReturns: any\nCategory: algebra\nKeywords: semiring, rig, algebraic structure, distributive, carrier, equality\n\nSee also: `semiring-carrier', `semiring-eq?', `semiring->additive-monoid', `validate-semiring'."
  (assert-procedure "make-semiring" plus)
  (assert-procedure "make-semiring" times)
  (validate-opts-keys "make-semiring" opts '(carrier eq?))
  (let ((equality-fn (assv-or opts 'eq? equal?)))
    (assert-procedure "make-semiring" equality-fn)
    (make-semiring* plus times zero one
                    (assv-or opts 'carrier #f)
                    equality-fn)))

(define (semiring-plus S a b)
  "Add A and B under semiring S's additive operation.\n\nExamples:\n  (semiring-plus (counting-semiring) 3 4)  => 7\n\nParameters:\n  S : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: addition, add, sum, plus, oplus"
  ((semiring-plus-fn S) a b))

(define (semiring-times S a b)
  "Multiply A and B under semiring S's multiplicative operation.\n\nExamples:\n  (semiring-times (counting-semiring) 3 4)  => 12\n\nParameters:\n  S : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: multiplication, multiply, product, times, otimes"
  ((semiring-times-fn S) a b))

(define (semiring-eq? S a b)
  "Test equality of A and B under semiring S's declared equality predicate.\n\nReturns truthy iff A and B represent the same value on S's carrier.\nDefaults to host `equal?' when the semiring was constructed without\nan explicit `(eq? . PROC)' opt. Consumer libraries (notably `(wile\nalgebra graph)') consult this for fixpoint convergence detection — a\ncustom equality predicate is the principled hook for non-canonical\ncarriers (e.g. tolerance-based equality on log-space floats, modular-\naware equality on ℤ/Pℤ).\n\nThe contract is defined only on carrier values; out-of-carrier inputs\nare undefined behavior.\n\nExamples:\n  (semiring-eq? (counting-semiring) 3 3)  => #t\n  (semiring-eq? (counting-semiring) 3 4)  => #f\n  (let ((S (make-semiring + * 0.0 1.0\n                          (cons 'eq?\n                                (lambda (a b) (< (abs (- a b)) 1e-9))))))\n    (semiring-eq? S 1.0 1.0000000001))   => #t\n\nParameters:\n  S : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\nKeywords: equality, equal, equivalence, convergence, fixpoint\n\nSee also: `make-semiring', `semiring-carrier'."
  ((semiring-eq?-fn S) a b))

(define (semiring->additive-monoid S)
  "Extract the additive monoid (PLUS, ZERO) from semiring S.\n\nExamples:\n  (let ((M (semiring->additive-monoid (counting-semiring))))\n    (monoid-op M 3 4))  => 7\n\nParameters:\n  S : any\nReturns: any\nCategory: algebra\nKeywords: additive, forgetful functor, projection, plus monoid\n\nSee also: `semiring->multiplicative-monoid', `make-monoid'."
  (make-monoid (semiring-plus-fn S) (semiring-zero S)))

(define (semiring->multiplicative-monoid S)
  "Extract the multiplicative monoid (TIMES, ONE) from semiring S.\n\nExamples:\n  (let ((M (semiring->multiplicative-monoid (counting-semiring))))\n    (monoid-op M 3 4))  => 12\n\nParameters:\n  S : any\nReturns: any\nCategory: algebra\nKeywords: multiplicative, forgetful functor, projection, times monoid\n\nSee also: `semiring->additive-monoid', `make-monoid'."
  (make-monoid (semiring-times-fn S) (semiring-one S)))

;; ─── Pre-built instances ─────────────────────

(define (boolean-semiring)
  "Construct the Boolean semiring where PLUS is logical or and TIMES is logical and.\nThe additive identity (zero) is #f and the multiplicative\nidentity (one) is #t.\n\nExamples:\n  (let ((B (boolean-semiring)))\n    (semiring-plus B #f #t))   => #t\n  (let ((B (boolean-semiring)))\n    (semiring-times B #t #f))  => #f\n\nReturns: any\nCategory: algebra\nKeywords: boolean, logic, or, and, truth values\n\nSee also: `tropical-semiring', `counting-semiring', `make-semiring'."
  ;; eq? declared explicitly: #t/#f are eq?-comparable singletons, faster than equal?
  ;; carrier 'boolean: idempotent ⊕ (logical or) — `semiring-cycle-safe?' returns #t.
  (make-semiring
    (lambda (a b) (or a b))
    (lambda (a b) (and a b))
    #f #t
    '(carrier . boolean)
    (cons 'eq? eq?)))

(define tropical-inf 'tropical-inf)

(define (tropical-min a b)
  "Return the lesser of A and B under tropical arithmetic.\nIn the tropical semiring, addition is defined as min.\nReturns B if A is tropical-inf, A if B is tropical-inf.\n\nParameters:\n  a : any\n  b : any\nReturns: any\nCategory: algebra\n\nSee also: `tropical-semiring', `tropical-add'."
  (cond ((eq? a tropical-inf) b)
        ((eq? b tropical-inf) a)
        (else (min a b))))

(define (tropical-add a b)
  "Return the sum of A and B under tropical arithmetic.\nIn the tropical semiring, multiplication is defined as +.\nReturns tropical-inf if either argument is tropical-inf.\n\nParameters:\n  a : any\n  b : any\nReturns: any\nCategory: algebra\n\nSee also: `tropical-semiring', `tropical-min'."
  (cond ((eq? a tropical-inf) tropical-inf)
        ((eq? b tropical-inf) tropical-inf)
        (else (+ a b))))

(define (tropical-eq? a b)
  "Return #t iff A and B are equal under tropical-arithmetic semantics.\nHandles the tropical-inf symbol explicitly; falls back to numeric `=' on\nfinite values. Faster than `equal?' on numerics and correct on the\nasymmetric tropical-inf cases that `=' alone would error on.\n\nParameters:\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\n\nSee also: `tropical-semiring', `tropical-min', `tropical-add'."
  (cond ((eq? a tropical-inf) (eq? b tropical-inf))
        ((eq? b tropical-inf) #f)
        (else (= a b))))

(define (tropical-semiring)
  "Construct the tropical semiring where PLUS is min and TIMES is +.\nThe additive identity (zero) is tropical-inf and the multiplicative\nidentity (one) is 0. Useful for shortest-path problems.\nAll operations on finite values return exact results.\n\nExamples:\n  (let ((T (tropical-semiring)))\n    (semiring-plus T 3 5))   => 3\n  (let ((T (tropical-semiring)))\n    (semiring-times T 3 5))  => 8\n\nReturns: any\nCategory: algebra\nKeywords: tropical, min-plus, shortest path, optimization, graph algorithm\n\nSee also: `boolean-semiring', `counting-semiring', `make-semiring'."
  ;; eq? declared explicitly: numeric = is faster than equal? on numerics,
  ;; tropical-eq? wraps it to handle the tropical-inf symbol correctly.
  ;; carrier 'tropical: idempotent ⊕ (min) — `semiring-cycle-safe?' returns #t.
  ;; Convergence is the classical Bellman-Ford guarantee for shortest paths
  ;; with non-negative edge weights; negative-cycle inputs are out of scope.
  (make-semiring tropical-min tropical-add tropical-inf 0
                 '(carrier . tropical)
                 (cons 'eq? tropical-eq?)))

(define (counting-semiring)
  "Construct the standard counting semiring over exact integers.\nPLUS is addition, TIMES is multiplication, zero is 0, one is 1.\n\nExamples:\n  (let ((C (counting-semiring)))\n    (semiring-plus C 3 4))   => 7\n  (let ((C (counting-semiring)))\n    (semiring-times C 3 4))  => 12\n\nReturns: any\nCategory: algebra\nKeywords: natural numbers, counting, integers, standard arithmetic\n\nSee also: `boolean-semiring', `tropical-semiring', `bigint-counting-semiring', `make-semiring'."
  ;; eq? declared explicitly: = handles exact integers, bignums (in-place
  ;; compare on big.Int), and exact/inexact mixed cases consistently.
  (make-semiring + * 0 1 (cons 'eq? =)))

(define (bigint-counting-semiring)
  "Construct a counting semiring with carrier 'big-int, opting into the\nbignum-targeted fast path when consumed by `make-graph-analysis'. The\nScheme-visible operations behave identically to (counting-semiring) —\narithmetic auto-promotes to bignum on overflow — but the carrier\nannotation lets `(wile algebra graph)' route path-counting queries\nthrough `count-paths-in-dag', which uses in-place `*big.Int' arithmetic\nand sidesteps the per-relaxation allocation overhead of the generic\nBellman-Ford inner loop.\n\nFast-path attachment requires three conditions on the\n`make-graph-analysis' call site, checked at construction time:\n  1. carrier 'big-int (this semiring or any (carrier . big-int) variant);\n  2. weight-fn is #f (unit weights — the kernel has no edge-data slot);\n  3. all adjacency node identifiers are atomic (symbol, string, number,\n     char, boolean — the kernel's name->index interning uses a hashtable).\nWhen any condition fails, `make-graph-analysis' silently falls back to\nthe pure-Scheme inner loop. The carrier opt is advisory; declaring it\nnever changes Scheme-visible arithmetic or query results, only\ndispatch cost. Use `graph-analysis-fast-path?' to verify attachment.\n\nWeighted bignum acceleration (sub-path 4B) is not yet implemented;\nbig-int carrier + non-#f weight-fn currently routes to the slow path.\n\nExamples:\n  (let ((C (bigint-counting-semiring)))\n    (semiring-carrier C))    => big-int\n  (let ((C (bigint-counting-semiring)))\n    (semiring-plus C 3 4))   => 7\n\nReturns: any\nCategory: algebra\nKeywords: counting, bignum, big-int, carrier, fast path, allocation\n\nSee also: `counting-semiring', `semiring-carrier', `make-graph-analysis', `graph-analysis-fast-path?'."
  ;; eq? declared explicitly: = is in-place compare on *big.Int in Go,
  ;; faster than equal?'s structural walk on the value-box.
  (make-semiring + * 0 1 '(carrier . big-int) (cons 'eq? =)))

;; ─── Approximate counting variants ───────────
;;
;; Three explicit overflow-aware counting semirings for workloads where the
;; exact `counting-semiring' is intractable (cyclic graphs, deep walks). All
;; three keep arithmetic in machine-word range when carrier fits — saturating
;; and modular use Scheme's numeric tower (auto-promotes to bignum if the
;; intermediate exceeds fixnum, then collapses back via min/modulo); log uses
;; float64 throughout.
;;
;; See plans/2026-05-24-approximate-counting-semirings.md for the design and
;; the trade-off matrix.

(define (modular-counting-semiring P)
  "Construct a counting semiring whose carrier is Z/PZ (integers mod P).\nPLUS is modular addition, TIMES is modular multiplication, zero is 0,\none is 1. Values are normalized to [0, P-1] via Scheme's `modulo'.\n\nIS A TRUE SEMIRING — modular arithmetic preserves associativity,\ncommutativity, distributivity, identities, and zero-annihilation.\n\nUseful when the modular value is interpreted as a *hash, witness, or\nparity*, NOT as an approximate count:\n  - Graph fingerprinting (walk-count multisets as isomorphism witnesses)\n  - Parity counting (bipartiteness, perfect-matching parity via mod 2)\n  - Schwartz-Zippel polynomial identity testing\n  - Rabin-Karp structural hashing for sub-graph indexing\n  - Karp-style randomized algorithms (matching mod P)\n\nA true count divisible by P returns 0 from the modular semiring,\nindistinguishable from \"no walks exist.\" For random/unstructured inputs\nthe collision probability is ~1/P; for adversarial or structured inputs\nit can be higher. Callers needing certainty can run with two primes and\ncompare.\n\nIf you want approximately N walks with a meaningful magnitude, use\n`saturating-counting-semiring' or `log-counting-semiring' instead —\nmodular values are NOT approximate counts.\n\n**Tractability on cycles:** modular Bellman-Ford on a cyclic graph\nDOES NOT converge. Z/PZ has bounded values but lacks an absorbing top\nelement, so loop products with non-zero order in Z/PZ* rotate the\ncounted value forever (Mohri 2002's k-closedness criterion fails for\nany finite k). `make-graph-analysis' queries on cyclic adjacencies\nwill hit the worklist's 2·V·E safety cap. The principled algorithm\nfor modular cyclic counting is matrix `*'-closure A* = (I − A)^{-1}\nover Z/PZ, not Bellman-Ford — a separate kernel, not yet shipped.\nUse modular for DAG-shaped workloads (fingerprinting, parity,\nSchwartz-Zippel); use `saturating-counting-semiring' for cyclic\napproximate counting.\n\nValidation: P must be an exact integer ≥ 2. Primality is advisory only\n(not enforced); use-case dependent.\n\nExamples:\n  (let ((S (modular-counting-semiring 7)))\n    (semiring-plus S 3 5))   => 1   ; (3+5) mod 7\n  (let ((S (modular-counting-semiring 7)))\n    (semiring-times S 3 5))  => 1   ; (15) mod 7\n  (let ((S (modular-counting-semiring 7)))\n    (semiring-plus S -2 1))  => 6   ; modulo canonicalizes\n\nParameters:\n  P : exact integer, ≥ 2\nReturns: semiring\nCategory: algebra\nKeywords: modular, counting, hashing, fingerprint, parity, Z/PZ, DAG\n\nSee also: `mersenne-31', `mersenne-61', `counting-semiring', `saturating-counting-semiring', `log-counting-semiring'."
  (unless (and (integer? P) (exact? P) (>= P 2))
    (error "modular-counting-semiring: modulus must be an exact integer ≥ 2" P))
  ;; modulo (R7RS) normalizes inputs to [0, P-1] regardless of sign, so no
  ;; separate canonicalize step is needed at the operation boundary.
  ;; The carrier symbol 'modular is advisory; consumer libraries can dispatch
  ;; on it if a future Go-side modular kernel is added.
  (let ((plus  (lambda (a b) (modulo (+ a b) P)))
        (times (lambda (a b) (modulo (* a b) P))))
    (make-semiring plus times 0 1
                   (cons 'carrier 'modular)
                   (cons 'eq? =))))

;; Named primes for `modular-counting-semiring'. The two choices give callers
;; an explicit speed / collision-rate trade-off:
;;
;;   mersenne-31  = 2^31 - 1 ≈ 2.1 × 10^9    — collision rate ~1 / 2×10^9
;;                                              ops stay in fixnum on most
;;                                              host architectures (a, b near
;;                                              2^31 produce intermediates
;;                                              near 2^62, which is still in
;;                                              int64 territory)
;;   mersenne-61  = 2^61 - 1 ≈ 2.3 × 10^18   — collision rate ~1 / 2×10^18
;;                                              times of two near-2^61 values
;;                                              briefly promotes to bignum
;;                                              before modulo collapses
;;
;; Both are well-known Mersenne primes. Default to mersenne-31 unless
;; collision probability dominates the cost calculation.
(define mersenne-31 (- (expt 2 31) 1))
(define mersenne-61 (- (expt 2 61) 1))

(define (log-counting-semiring)
  "Construct a counting semiring whose carrier is float64 in log-space.\nStored values are log(true-count). PLUS is log-sum-exp, TIMES is +,\nzero is -inf.0, one is 0.0.\n\nIS A TRUE SEMIRING — log-sum-exp is associative and commutative,\nregular `+' distributes over it. Effective magnitude range is\nexp(±1e308) ≈ 10^±10^307 (float64), practically unbounded.\n\nLoses precision past ~2^53, preserves orders of magnitude. Two paths\nwith counts 10^50 and 10^50 + 1 are indistinguishable; counts of 10^50\nvs 10^60 are clearly ranked.\n\n**Tractability on cycles:** the log semiring is NOT bounded on cycles.\nWalk counts on a cyclic graph grow without bound in linear log-space\n(log of an infinite count is still infinite), so `make-graph-analysis'\nqueries on cyclic adjacencies do not converge any faster than the\nexact `counting-semiring' — both will hit the worklist's 2·V·E safety\ncap. The log carrier saves *space* per value, not *iterations*. The\nONLY approximate semiring that converges under Bellman-Ford on cyclic\ngraphs is `saturating-counting-semiring', whose CAP is an absorbing\ntop element. Modular Z/PZ also lacks an absorbing element and shares\nthe same non-convergence pathology — use it for DAG fingerprinting,\nnot cyclic counting. Use `log-counting-semiring' for DAG-shaped\nworkloads where the questions are ranking-by-magnitude.\n\nUseful for: relative-magnitude ranking on DAGs (fan-in pressure where\nexact counts past 10^9 are uninteresting), Viterbi-like maximum-\nlikelihood path queries, any DAG analysis where the question is \"which\nnode has the most\" not \"exactly how many.\"\n\nStructural cousin of the tropical semiring (max/+) — same arithmetic\nshape, soft-max instead of hard-max.\n\nExamples:\n  (let ((L (log-counting-semiring)))\n    (semiring-zero L))                           => -inf.0\n  (let ((L (log-counting-semiring)))\n    (semiring-one L))                            => 0.0\n  (let ((L (log-counting-semiring)))\n    (semiring-times L 1.0 2.0))                  => 3.0   ; log(e * e^2) = 3\n  (let* ((L (log-counting-semiring))\n         (r (semiring-plus L 1.0 1.0)))\n    (< (abs (- r (+ 1.0 (log 2)))) 1e-12))       => #t    ; log(2e) = 1 + log 2\n\nReturns: semiring\nCategory: algebra\nKeywords: log-space, log-sum-exp, counting, ranking, Viterbi, soft-max, DAG\n\nSee also: `counting-semiring', `modular-counting-semiring', `saturating-counting-semiring', `tropical-semiring'."
  ;; log-sum-exp with max-subtraction stability guard. If both inputs are
  ;; -inf.0 (the semiring-zero), the naive formula gives -inf + log(1+exp(0))
  ;; = -inf + log(2) = -inf — correct, but log(1+exp(0)) computes log(2)
  ;; unnecessarily. The early-return on either being -inf.0 short-circuits.
  (let ((plus
          (lambda (a b)
            (cond
              ((= a -inf.0) b)
              ((= b -inf.0) a)
              (else
                (let ((hi (max a b))
                      (lo (min a b)))
                  ;; lo - hi is ≤ 0; exp(lo - hi) is in (0, 1]; log(1 + that)
                  ;; is in (0, log 2]. Underflow to log(1+0) = 0 when
                  ;; lo - hi is very negative is mathematically correct ("a
                  ;; count of 10^300 plus a count of 10^200 is
                  ;; indistinguishably 10^300").
                  (+ hi (log (+ 1.0 (exp (- lo hi))))))))))
        (eq?-pred
          ;; Compare in log-space at float epsilon. equal? would distinguish
          ;; values that differ in the last bit; for graph-query convergence
          ;; we want stability at float precision.
          (lambda (a b)
            (cond
              ((= a -inf.0) (= b -inf.0))
              ((= b -inf.0) #f)
              (else (< (abs (- a b)) 1e-12))))))
    (make-semiring plus + -inf.0 0.0
                   (cons 'carrier 'log-float)
                   (cons 'eq? eq?-pred))))

(define (saturating-counting-semiring cap)
  "Construct a counting semiring whose carrier is [0, CAP], saturating.\nPLUS is min(a+b, CAP), TIMES is min(a×b, CAP). The cap acts as an\nabsorbing top element: any operation producing a value ≥ CAP yields\nCAP. zero is 0, one is 1.\n\nIS A TRUE COMMUTATIVE SEMIRING — saturating arithmetic on [0, CAP]\npreserves all semiring axioms. The structure is isomorphic to the\nquotient of the complete-natural-number semiring (N ∪ {∞}, +, ×) by\nthe equivalence x ≥ CAP ↦ ⊤. Distributivity holds: for any\na, b, c ∈ [0, CAP], both a × (b + c) and (a × b) + (a × c) reduce to\nmin(a × (b + c), CAP) = min(ab + ac, CAP) because the clamping commutes\nwith the outer min.\n\nINFORMATION-BOUNDED, not algebraically-defective: two distinct true\ncounts that both exceed CAP saturate to CAP and become indistinguishable.\nThis is a loss of *information*, not a violation of any axiom. Values\nat CAP convey \"≥ CAP,\" not an exact count. Composition past the cap\ndegrades: once two operands both equal CAP, every subsequent operation\nproduces CAP, collapsing magnitude information.\n\n**Tractability on cycles:** the saturating semiring is the ONLY\napproximate-counting variant that converges under Bellman-Ford on\ncyclic graphs. CAP is an algebraic absorbing top: once any node's\ndistance reaches CAP, further `⊕'/`⊗' operations produce CAP, so the\nworklist's `(semiring-eq? merged old-val)' check finally returns #t\nand the node stops re-enqueueing. Empirically: the documented 539-node\ncyclic incident (12 back-edges, 593k-iteration safety cap) converges\nin ~0.23s under `(saturating-counting-semiring (expt 2 53))'. See\n`examples/benchmarks/bench-cyclic-counting-approximate.scm'.\nBy contrast, `modular-counting-semiring' and `log-counting-semiring'\nlack an absorbing element and hit the worklist safety cap on cycles.\n\nUseful for: ranking, threshold queries (\"any count above K?\"),\ninstrumentation, **and cyclic approximate counting**. Default cap\nsuggestion: 2^53 (largest exactly-representable as float64, in case\ncallers later convert).\n\nValidation: CAP must be a positive exact integer.\n\nExamples:\n  (let ((S (saturating-counting-semiring 100)))\n    (semiring-plus S 50 30))                     => 80\n  (let ((S (saturating-counting-semiring 100)))\n    (semiring-plus S 80 50))                     => 100  ; saturated\n  (let ((S (saturating-counting-semiring 100)))\n    (semiring-times S 11 11))                    => 100  ; 121 saturates\n\nParameters:\n  cap : exact positive integer\nReturns: semiring\nCategory: algebra\nKeywords: saturating, bounded, counting, clamp, ranking, threshold, cyclic, absorbing\n\nSee also: `counting-semiring', `modular-counting-semiring', `log-counting-semiring', `bounded-carrier-semiring?'."
  (unless (and (integer? cap) (exact? cap) (positive? cap))
    (error "saturating-counting-semiring: cap must be a positive exact integer" cap))
  ;; For PLUS: (a + b) can momentarily promote to bignum if a + b > fixnum
  ;; range, but min collapses to int. For TIMES: pre-check a ≤ cap/b avoids
  ;; computing the multiplication entirely when it would saturate. This
  ;; matters because (* a b) for large a, b can produce a bignum even if
  ;; cap is small.
  (let ((plus  (lambda (a b)
                 (let ((s (+ a b)))
                   (if (>= s cap) cap s))))
        (times (lambda (a b)
                 (cond
                   ((or (= a 0) (= b 0)) 0)
                   ;; Pre-check: if a > cap/b then a*b > cap (integer-divide
                   ;; rounds down, so the check is conservative — correct).
                   ((> a (quotient cap b)) cap)
                   (else
                     (let ((p (* a b)))
                       (if (>= p cap) cap p)))))))
    (make-semiring plus times 0 1
                   (cons 'carrier 'saturating)
                   (cons 'eq? =))))

(define (bounded-carrier-semiring? S)
  "Return #t iff S's carrier has a saturation point past which information\nis irrecoverable.\n\nCurrently, this is true for `saturating-counting-semiring' instances\n(carrier saturates at CAP — values past CAP are indistinguishable, so\nmagnitude information is lost). It is #f for `modular-counting-semiring'\n(carrier is exactly Z/PZ; values are well-defined modular fingerprints,\nnot approximations) and `log-counting-semiring' (carrier covers the\nfloat64 magnitude range; bounded precision, unbounded magnitude).\n\nThe predicate is a SEMANTIC WARNING, not an algebraic flag: all three\napproximate variants are true semirings. It marks semirings whose\ncarrier has a saturation point so downstream consumers can warn callers\nthat results past the saturation point will be uninformative.\n\nExamples:\n  (bounded-carrier-semiring? (saturating-counting-semiring 100))  => #t\n  (bounded-carrier-semiring? (modular-counting-semiring 7))       => #f\n  (bounded-carrier-semiring? (log-counting-semiring))             => #f\n  (bounded-carrier-semiring? (counting-semiring))                 => #f\n\nParameters:\n  S : semiring\nReturns: boolean\nCategory: algebra\nKeywords: bounded, saturation, predicate, carrier, approximation\n\nSee also: `saturating-counting-semiring', `semiring-carrier'."
  (and (semiring? S)
       (eq? (semiring-carrier S) 'saturating)))

(define (semiring-cycle-safe? S)
  "Return #t iff Bellman-Ford-style worklist iteration over S is guaranteed\nto converge on cyclic adjacencies in finite steps.\n\nConvergence requires either (a) idempotent ⊕ — `a ⊕ a = a' for all a in\nthe carrier — or (b) an absorbing top element `⊤' reachable from any\nnon-zero state via finitely many ⊕/⊗ steps (Mohri's k-closedness for\nsome finite k). Idempotence is what makes the Boolean (`or') and\ntropical (`min') semirings converge classically; the absorbing top is\nwhat makes `saturating-counting-semiring' converge despite `+' not being\nidempotent. `modular-counting-semiring' and `log-counting-semiring' have\nneither property and `make-graph-analysis' queries on cyclic adjacencies\nover them WILL hit the worklist's 2·V·E safety cap.\n\nDispatched on the carrier symbol attached at construction (see\n`semiring-carrier'). A closed set of known-safe symbols answers #t:\n\n  'saturating  — absorbing top at CAP\n  'boolean     — idempotent ⊕ (logical or)\n  'tropical    — idempotent ⊕ (min)\n\nAll other symbols, including #f (unannotated), answer #f. Semirings\nbuilt via raw `make-semiring' without a `(carrier . SYM)' opt therefore\nreturn #f even if they happen to be cycle-safe — declare the carrier to\nopt in. This is the conservative default: we never claim convergence\nwe haven't verified algebraically.\n\nDISTINCT from `bounded-carrier-semiring?'. Saturating is BOTH bounded\nand cycle-safe; tropical and boolean are cycle-safe but NOT bounded;\nmodular and log are bounded-magnitude (modular in Z/PZ, log in float64)\nbut NOT cycle-safe. The two predicates answer different questions.\n\nAdvisory, not gating: `make-graph-analysis' already detects worst-case\nnon-convergence via the 2·V·E safety cap. This predicate lets callers\nrefuse a query early or surface a warning before paying the cap.\n\nExamples:\n  (semiring-cycle-safe? (saturating-counting-semiring 100))  => #t\n  (semiring-cycle-safe? (boolean-semiring))                  => #t\n  (semiring-cycle-safe? (tropical-semiring))                 => #t\n  (semiring-cycle-safe? (modular-counting-semiring 7))       => #f\n  (semiring-cycle-safe? (log-counting-semiring))             => #f\n  (semiring-cycle-safe? (counting-semiring))                 => #f\n  (semiring-cycle-safe? (bigint-counting-semiring))          => #f\n  (semiring-cycle-safe? (make-semiring + * 0 1))             => #f\n  (semiring-cycle-safe? 42)                                  => #f\n\nParameters:\n  S : any\nReturns: boolean\nCategory: algebra\nKeywords: cycle, convergence, Bellman-Ford, idempotent, absorbing, k-closed, worklist\n\nSee also: `bounded-carrier-semiring?', `semiring-carrier', `saturating-counting-semiring', `modular-counting-semiring', `log-counting-semiring'."
  (and (semiring? S)
       (case (semiring-carrier S)
         ((saturating boolean tropical) #t)
         (else #f))))

;; ─── Macro ───────────────────────────────────

(define-syntax with-semiring
  (syntax-rules ()
    ((with-semiring S (plus times zero one) body ...)
     (let ((tmp S))
       (let ((plus  (lambda (a b) (semiring-plus tmp a b)))
             (times (lambda (a b) (semiring-times tmp a b)))
             (zero  (semiring-zero tmp))
             (one   (semiring-one tmp)))
         body ...)))))

;; ─── Validation ──────────────────────────────

(define (validate-semiring S samples)
  "Spot-check that S satisfies the semiring laws on SAMPLES.\nTests additive and multiplicative identity, zero annihilation,\nadditive commutativity, and left and right distributivity for\nall elements and triples in SAMPLES. Returns #t if all laws\nhold, or a list of (violation-type element ...) entries\ndescribing failures.\n\nExamples:\n  (validate-semiring (counting-semiring) '(0 1 2 3))  => #t\n\nParameters:\n  S : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: distributivity, annihilation, commutativity, law checking, validation\n\nSee also: `make-semiring', `semiring-plus', `semiring-times'."
  (let ((fail! (make-violation-reporter))
        (z (semiring-zero S))
        (o (semiring-one S)))
    (for-each
      (lambda (a)
        ;; Additive identity
        (unless (equal? (semiring-plus S z a) a)
          (fail! 'additive-left-identity a))
        (unless (equal? (semiring-plus S a z) a)
          (fail! 'additive-right-identity a))
        ;; Multiplicative identity
        (unless (equal? (semiring-times S o a) a)
          (fail! 'multiplicative-left-identity a))
        (unless (equal? (semiring-times S a o) a)
          (fail! 'multiplicative-right-identity a))
        ;; Zero annihilation
        (unless (equal? (semiring-times S z a) z)
          (fail! 'left-annihilation a))
        (unless (equal? (semiring-times S a z) z)
          (fail! 'right-annihilation a))
        (for-each
          (lambda (b)
            ;; Additive commutativity
            (unless (equal? (semiring-plus S a b) (semiring-plus S b a))
              (fail! 'additive-commutativity a b))
            ;; Left distributivity: a × (b + c)
            (for-each
              (lambda (c)
                (unless (equal? (semiring-times S a (semiring-plus S b c))
                                (semiring-plus S (semiring-times S a b)
                                                 (semiring-times S a c)))
                  (fail! 'left-distributivity a b c))
                (unless (equal? (semiring-times S (semiring-plus S a b) c)
                                (semiring-plus S (semiring-times S a c)
                                                 (semiring-times S b c)))
                  (fail! 'right-distributivity a b c)))
              samples))
          samples))
      samples)
    (fail!)))
