;;; (wile algebra semiring) — Semirings
;;;
;;; A semiring (S, +, ×, 0, 1) has:
;;; - (S, +, 0) is a commutative monoid
;;; - (S, ×, 1) is a monoid
;;; - × distributes over +
;;; - 0 annihilates ×: 0 × a = a × 0 = 0

(define-record-type <semiring>
  (make-semiring* plus-fn times-fn zero one carrier)
  semiring?
  (plus-fn  semiring-plus-fn)
  (times-fn semiring-times-fn)
  (zero     semiring-zero)
  (one      semiring-one)
  (carrier  semiring-carrier))

(define (make-semiring plus times zero one . opts)
  "Construct a semiring from PLUS, TIMES, ZERO, and ONE.\nPLUS must be associative and commutative with ZERO as identity.\nTIMES must be associative with ONE as identity and must\ndistribute over PLUS. ZERO must annihilate TIMES from both sides.\n\nThe trailing alist OPTS accepts the following keys:\n  (carrier . SYM) — declares the carrier type. The symbol drives\n    consumer-side fast-path eligibility. Default #f (no fast path).\n    Vocabulary: 'big-int, 'integer, 'rational, 'real, 'complex,\n    'boolean, 'log-float, 'modular, 'saturating, 'opaque.\n\nExamples:\n  (let ((S (make-semiring + * 0 1)))\n    (semiring-plus S 3 4))   => 7\n  (let ((S (make-semiring + * 0 1 '(carrier . big-int))))\n    (semiring-carrier S))    => big-int\n\nParameters:\n  plus : procedure\n  times : procedure\n  zero : any\n  one : any\n  opts : alist\nReturns: any\nCategory: algebra\nKeywords: semiring, rig, algebraic structure, distributive, carrier\n\nSee also: `semiring-carrier', `semiring->additive-monoid', `validate-semiring'."
  (assert-procedure "make-semiring" plus)
  (assert-procedure "make-semiring" times)
  (validate-opts-keys "make-semiring" opts '(carrier))
  (make-semiring* plus times zero one (assv-or opts 'carrier #f)))

(define (semiring-plus S a b)
  "Add A and B under semiring S's additive operation.\n\nExamples:\n  (semiring-plus (counting-semiring) 3 4)  => 7\n\nParameters:\n  S : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: addition, add, sum, plus, oplus"
  ((semiring-plus-fn S) a b))

(define (semiring-times S a b)
  "Multiply A and B under semiring S's multiplicative operation.\n\nExamples:\n  (semiring-times (counting-semiring) 3 4)  => 12\n\nParameters:\n  S : any\n  a : any\n  b : any\nReturns: any\nCategory: algebra\nKeywords: multiplication, multiply, product, times, otimes"
  ((semiring-times-fn S) a b))

(define (semiring->additive-monoid S)
  "Extract the additive monoid (PLUS, ZERO) from semiring S.\n\nExamples:\n  (let ((M (semiring->additive-monoid (counting-semiring))))\n    (monoid-op M 3 4))  => 7\n\nParameters:\n  S : any\nReturns: any\nCategory: algebra\nKeywords: additive, forgetful functor, projection, plus monoid\n\nSee also: `semiring->multiplicative-monoid', `make-monoid'."
  (make-monoid (semiring-plus-fn S) (semiring-zero S)))

(define (semiring->multiplicative-monoid S)
  "Extract the multiplicative monoid (TIMES, ONE) from semiring S.\n\nExamples:\n  (let ((M (semiring->multiplicative-monoid (counting-semiring))))\n    (monoid-op M 3 4))  => 12\n\nParameters:\n  S : any\nReturns: any\nCategory: algebra\nKeywords: multiplicative, forgetful functor, projection, times monoid\n\nSee also: `semiring->additive-monoid', `make-monoid'."
  (make-monoid (semiring-times-fn S) (semiring-one S)))

;; ─── Pre-built instances ─────────────────────

(define (boolean-semiring)
  "Construct the Boolean semiring where PLUS is logical or and TIMES is logical and.\nThe additive identity (zero) is #f and the multiplicative\nidentity (one) is #t.\n\nExamples:\n  (let ((B (boolean-semiring)))\n    (semiring-plus B #f #t))   => #t\n  (let ((B (boolean-semiring)))\n    (semiring-times B #t #f))  => #f\n\nReturns: any\nCategory: algebra\nKeywords: boolean, logic, or, and, truth values\n\nSee also: `tropical-semiring', `counting-semiring', `make-semiring'."
  (make-semiring
    (lambda (a b) (or a b))
    (lambda (a b) (and a b))
    #f #t))

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

(define (tropical-semiring)
  "Construct the tropical semiring where PLUS is min and TIMES is +.\nThe additive identity (zero) is tropical-inf and the multiplicative\nidentity (one) is 0. Useful for shortest-path problems.\nAll operations on finite values return exact results.\n\nExamples:\n  (let ((T (tropical-semiring)))\n    (semiring-plus T 3 5))   => 3\n  (let ((T (tropical-semiring)))\n    (semiring-times T 3 5))  => 8\n\nReturns: any\nCategory: algebra\nKeywords: tropical, min-plus, shortest path, optimization, graph algorithm\n\nSee also: `boolean-semiring', `counting-semiring', `make-semiring'."
  (make-semiring tropical-min tropical-add tropical-inf 0))

(define (counting-semiring)
  "Construct the standard counting semiring over exact integers.\nPLUS is addition, TIMES is multiplication, zero is 0, one is 1.\n\nExamples:\n  (let ((C (counting-semiring)))\n    (semiring-plus C 3 4))   => 7\n  (let ((C (counting-semiring)))\n    (semiring-times C 3 4))  => 12\n\nReturns: any\nCategory: algebra\nKeywords: natural numbers, counting, integers, standard arithmetic\n\nSee also: `boolean-semiring', `tropical-semiring', `bigint-counting-semiring', `make-semiring'."
  (make-semiring + * 0 1))

(define (bigint-counting-semiring)
  "Construct a counting semiring with carrier 'big-int, opting into the\nbignum-targeted fast path when consumed by `make-graph-analysis'. The\nScheme-visible operations behave identically to (counting-semiring) —\narithmetic auto-promotes to bignum on overflow — but the carrier\nannotation lets `(wile algebra graph)' route path-counting queries\nthrough `count-paths-in-dag', which uses in-place `*big.Int' arithmetic\nand sidesteps the per-relaxation allocation overhead of the generic\nBellman-Ford inner loop.\n\nFast-path attachment requires three conditions on the\n`make-graph-analysis' call site, checked at construction time:\n  1. carrier 'big-int (this semiring or any (carrier . big-int) variant);\n  2. weight-fn is #f (unit weights — the kernel has no edge-data slot);\n  3. all adjacency node identifiers are atomic (symbol, string, number,\n     char, boolean — the kernel's name->index interning uses a hashtable).\nWhen any condition fails, `make-graph-analysis' silently falls back to\nthe pure-Scheme inner loop. The carrier opt is advisory; declaring it\nnever changes Scheme-visible arithmetic or query results, only\ndispatch cost. Use `graph-analysis-fast-path?' to verify attachment.\n\nWeighted bignum acceleration (sub-path 4B) is not yet implemented;\nbig-int carrier + non-#f weight-fn currently routes to the slow path.\n\nExamples:\n  (let ((C (bigint-counting-semiring)))\n    (semiring-carrier C))    => big-int\n  (let ((C (bigint-counting-semiring)))\n    (semiring-plus C 3 4))   => 7\n\nReturns: any\nCategory: algebra\nKeywords: counting, bignum, big-int, carrier, fast path, allocation\n\nSee also: `counting-semiring', `semiring-carrier', `make-graph-analysis', `graph-analysis-fast-path?'."
  (make-semiring + * 0 1 '(carrier . big-int)))

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
