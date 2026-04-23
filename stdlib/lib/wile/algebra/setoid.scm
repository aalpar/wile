;;; (wile algebra setoid) — Setoids
;;;
;;; A setoid is a set equipped with an explicit equivalence relation:
;;; (S, ≡) where ≡ is reflexive (a ≡ a), symmetric (a ≡ b ⟹ b ≡ a),
;;; and transitive (a ≡ b ∧ b ≡ c ⟹ a ≡ c).
;;;
;;; This module also hosts the algebraic-structure shared plumbing used
;;; by every `(wile algebra X)' library: assertion macros, equivalence-
;;; parameterized collection helpers, options-alist helpers, and the
;;; violation reporter. See stdlib/lib/wile/algebra/CLAUDE.md for the
;;; structure-API convention that anchors these.

;; ─── Assertion macros (defined first so make-X below can use them) ────

(define-syntax assert-procedure
  (syntax-rules ()
    ;; (assert-procedure "site" x) — raise if X is not a procedure.
    ;; Captures the source identifier of X in the error message so the
    ;; failure pinpoints the offending argument without a hand-written
    ;; label. Intended for procedural-argument checks at the top of
    ;; make-X constructors.
    ;;
    ;; Example:
    ;;   (define (make-ring plus times zero one negate)
    ;;     (assert-procedure "make-ring" plus)
    ;;     (assert-procedure "make-ring" times)
    ;;     (assert-procedure "make-ring" negate)
    ;;     ...)
    ((assert-procedure site expr)
     (unless (procedure? expr)
       (error (string-append site ": "
                             (symbol->string 'expr)
                             " must be a procedure")
              expr)))))

(define-record-type <setoid>
  (make-setoid* equiv-fn)
  setoid?
  (equiv-fn setoid-equiv-fn))

(define (make-setoid equiv-fn)
  "Construct a setoid from an equivalence-relation procedure EQUIV-FN.\nEQUIV-FN takes two elements and returns a boolean indicating whether\nthey are considered equivalent. The underlying relation must be\nreflexive, symmetric, and transitive for the resulting setoid to\nbehave correctly; `validate-setoid' spot-checks these laws.\n\nExamples:\n  (setoid-equiv? (make-setoid eq?) 'a 'a)    => #t\n  (setoid-equiv? (make-setoid =) 1 1.0)      => #t\n\nParameters:\n  equiv-fn : procedure\nReturns: setoid\nCategory: algebra\nKeywords: setoid, equivalence, construction\n\nSee also: `default-setoid', `validate-setoid'."
  (assert-procedure "make-setoid" equiv-fn)
  (make-setoid* equiv-fn))

(define (setoid-equiv? S a b)
  "Test whether A and B are equivalent under setoid S.\nApplies S's equivalence relation to A and B, returning #t\nif they are considered equivalent, #f otherwise.\n\nExamples:\n  (setoid-equiv? (default-setoid) '(1 2) '(1 2))  => #t\n  (setoid-equiv? (default-setoid) 1 2)             => #f\n\nParameters:\n  S : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\nKeywords: equivalence, equality, equiv, congruence, identification\n\nSee also: `make-setoid', `validate-setoid'."
  ((setoid-equiv-fn S) a b))

(define (default-setoid)
  "Construct a setoid using Scheme's equal? as the equivalence relation.\n\nExamples:\n  (setoid-equiv? (default-setoid) '(1 2) '(1 2))  => #t\n  (setoid-equiv? (default-setoid) 'a 'b)           => #f\n\nReturns: any\nCategory: algebra\nKeywords: equal, structural equality, default\n\nSee also: `numeric-setoid', `eqv-setoid'."
  (make-setoid equal?))

(define (numeric-setoid)
  "Construct a setoid using Scheme's = as the equivalence relation.\nOnly valid for numeric elements.\n\nExamples:\n  (setoid-equiv? (numeric-setoid) 1 1.0)    => #t\n  (setoid-equiv? (numeric-setoid) 1/2 0.5)  => #t\n\nReturns: any\nCategory: algebra\nKeywords: numeric, number, mathematical equality\n\nSee also: `default-setoid', `string-setoid'."
  (make-setoid =))

(define (string-setoid)
  "Construct a setoid using string=? as the equivalence relation.\nOnly valid for string elements.\n\nExamples:\n  (setoid-equiv? (string-setoid) \"abc\" \"abc\")  => #t\n  (setoid-equiv? (string-setoid) \"a\" \"b\")      => #f\n\nReturns: any\nCategory: algebra\nKeywords: string, text, string equality\n\nSee also: `default-setoid', `numeric-setoid'."
  (make-setoid string=?))

(define (eqv-setoid)
  "Construct a setoid using Scheme's eqv? as the equivalence relation.\nUses identity/value equality -- same object or same simple value.\n\nExamples:\n  (setoid-equiv? (eqv-setoid) 'a 'a)  => #t\n  (setoid-equiv? (eqv-setoid) 1 1)    => #t\n\nReturns: any\nCategory: algebra\nKeywords: eqv, identity, pointer equality, value equality\n\nSee also: `default-setoid'."
  (make-setoid eqv?))

(define (setoid-equivalence-class S x samples)
  "Return elements of SAMPLES equivalent to X under setoid S.\nFilters SAMPLES to those elements for which S's equivalence\nrelation holds with X. The result always includes X itself\nwhen X appears in SAMPLES.\n\nExamples:\n  (setoid-equivalence-class (numeric-setoid) 1 '(1 1.0 2 3))  => (1 1.0)\n  (setoid-equivalence-class (default-setoid) 5 '(1 2 3))      => ()\n\nParameters:\n  S : any\n  x : any\n  samples : list\nReturns: list\nCategory: algebra\nKeywords: equivalence class, partition, quotient, coset, fiber, preimage\n\nSee also: `setoid-equiv?', `make-setoid'."
  (let loop ((xs samples) (acc '()))
    (cond
      ((null? xs) (reverse acc))
      ((setoid-equiv? S x (car xs))
       (loop (cdr xs) (cons (car xs) acc)))
      (else (loop (cdr xs) acc)))))

;; ─── Collection operations parameterized by setoid ───────────

(define (setoid-member? S x xs)
  "Test whether X appears in list XS under setoid S's equivalence.\nSetoid-driven analogue of `memv' / `member' that returns a boolean\n(not the matching tail). Used by structures whose carriers need\nequivalence-aware membership beyond Scheme's fixed predicates.\n\nExamples:\n  (setoid-member? (numeric-setoid) 1.0 '(1 2 3))   => #t\n  (setoid-member? (default-setoid) 5 '(1 2 3))     => #f\n\nParameters:\n  S : setoid\n  x : any\n  xs : list\nReturns: boolean\nCategory: algebra\nKeywords: member, contains, setoid, equivalence, membership\n\nSee also: `setoid-assoc', `setoid-dedup', `setoid-equiv?'."
  (let loop ((xs xs))
    (cond ((null? xs) #f)
          ((setoid-equiv? S x (car xs)) #t)
          (else (loop (cdr xs))))))

(define (setoid-assoc S key alist)
  "Return the first (key . v) pair in ALIST whose car is setoid-equivalent\nto KEY under S, or #f if none. Setoid-driven analogue of `assv' /\n`assoc' / `assq' for alists keyed by non-atomic or equivalence-tolerant\nvalues that Scheme's built-in predicates can't match.\n\nExamples:\n  (setoid-assoc (numeric-setoid) 1.0 '((1 . a) (2 . b)))  => (1 . a)\n  (setoid-assoc (default-setoid) 'x   '((a . 1) (b . 2))) => #f\n\nParameters:\n  S : setoid\n  key : any\n  alist : list\nReturns: pair or #f\nCategory: algebra\nKeywords: assoc, assv, lookup, setoid, equivalence\n\nSee also: `setoid-member?', `setoid-dedup', `setoid-equiv?'."
  (let loop ((xs alist))
    (cond
      ((null? xs) #f)
      ((and (pair? (car xs))
            (setoid-equiv? S key (caar xs)))
       (car xs))
      (else (loop (cdr xs))))))

(define (setoid-dedup S xs)
  "Return XS with later setoid-equivalent duplicates removed, preserving\nfirst-seen order. Setoid-driven analogue of `delete-duplicates' on lists\nwhose element equality follows an arbitrary equivalence rather than a\nfixed R7RS predicate.\n\nExamples:\n  (setoid-dedup (numeric-setoid) '(1 1.0 2 2 3))  => (1 2 3)\n  (setoid-dedup (default-setoid) '(a b a c b))    => (a b c)\n\nParameters:\n  S : setoid\n  xs : list\nReturns: list\nCategory: algebra\nKeywords: dedup, unique, setoid, equivalence, delete-duplicates\n\nSee also: `setoid-member?', `setoid-assoc', `setoid-equiv?'."
  (let loop ((src xs) (seen '()))
    (cond
      ((null? src) (reverse seen))
      ((setoid-member? S (car src) seen) (loop (cdr src) seen))
      (else (loop (cdr src) (cons (car src) seen))))))

(define (make-violation-reporter)
  "Return a two-mode procedure for collecting law-check violations.\nCall the returned procedure with (type args ...) to record a violation;\ncall it with zero arguments to finalize and return the result: #t if\nno violations were recorded, or the reversed list of (type . args)\nentries otherwise.\n\nReplaces the handwritten\n  (let ((violations '()))\n    (define (fail! type . args)\n      (set! violations (cons (cons type args) violations)))\n    ...checks calling fail!...\n    (if (null? violations) #t (reverse violations)))\nboilerplate shared by every algebraic law-checking function. Validators\nbind `(let ((fail! (make-violation-reporter))) ... (fail!))' and use\nfail! both to record and to finalize.\n\nExamples:\n  (let ((fail! (make-violation-reporter)))\n    (fail! 'left-identity 1)\n    (fail! 'associativity 2 3 4)\n    (fail!))\n  => ((left-identity 1) (associativity 2 3 4))\n\n  (let ((fail! (make-violation-reporter)))\n    (fail!))\n  => #t\n\nReturns: procedure\nCategory: algebra\nKeywords: validation, law checking, violation, reporter, collector, spot-check\n\nSee also: `validate-setoid', `assert-validation'."
  (let ((violations '()))
    (case-lambda
      (() (if (null? violations) #t (reverse violations)))
      ((type . args)
       (set! violations (cons (cons type args) violations))))))

(define (validate-setoid S samples)
  "Spot-check that S satisfies the setoid laws on SAMPLES.\nTests reflexivity (a ≡ a), symmetry (a ≡ b ⟹ b ≡ a), and\ntransitivity (a ≡ b ∧ b ≡ c ⟹ a ≡ c) for all elements and\ntriples in SAMPLES. Returns #t if all laws hold, or a list of\n(violation-type element ...) entries describing failures.\n\nExamples:\n  (validate-setoid (default-setoid) '(1 2 3))  => #t\n\nParameters:\n  S : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: reflexivity, symmetry, transitivity, validation, law checking, equivalence\n\nSee also: `make-setoid', `setoid-equiv?', `make-violation-reporter'."
  (let ((fail! (make-violation-reporter)))
    ;; Reflexivity: a ≡ a
    (for-each
      (lambda (a) (unless (setoid-equiv? S a a) (fail! 'reflexivity a)))
      samples)
    ;; Symmetry: a ≡ b ⟹ b ≡ a
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            (when (and (setoid-equiv? S a b)
                       (not (setoid-equiv? S b a)))
              (fail! 'symmetry a b)))
          samples))
      samples)
    ;; Transitivity: a ≡ b ∧ b ≡ c ⟹ a ≡ c
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            (when (setoid-equiv? S a b)
              (for-each
                (lambda (c)
                  (when (and (setoid-equiv? S b c)
                             (not (setoid-equiv? S a c)))
                    (fail! 'transitivity a b c)))
                samples)))
          samples))
      samples)
    (fail!)))

(define-syntax with-setoid
  (syntax-rules ()
    ((with-setoid S (equiv?) body ...)
     (let ((tmp S))
       (let ((equiv? (lambda (a b) (setoid-equiv? tmp a b))))
         body ...)))))

(define-syntax assert-validation
  (syntax-rules ()
    ;; (assert-validation (validate-X args ...)) — raise if the result is
    ;; not #t. Preserves the source expression in the error datum so the
    ;; raise identifies the failing check without a hand-written label.
    ;;
    ;; Replaces the handwritten
    ;;   (let ((r (validate-X args ...)))
    ;;     (unless (eq? r #t) (error "assert-X: ..." r)))
    ;; at every call site. Pairs with `make-violation-reporter' — the
    ;; #t vs (violations ...) return contract is shared.
    ((assert-validation expr)
     (let ((result expr))
       (unless (eq? result #t)
         (error "assert-validation: validation failed" 'expr result))))))

;; ─── Options-alist helpers for make-X constructors ───────────
;;
;; These are plumbing shared across algebraic-structure constructors
;; that accept a trailing alist of optional metadata. They live here
;; because every leaf library already imports (wile algebra setoid);
;; they are not setoid operations per se. Move to a dedicated module
;; if a third category of shared helpers accumulates.

(define (assv-or opts key fallback)
  "Return the cdr of the first pair in OPTS whose car is `eqv?' to KEY,\nor FALLBACK if no such pair exists. Convenience wrapper over `assv' for\ntrailing-option alists accepted by make-X constructors.\n\nExamples:\n  (assv-or '((a . 1) (b . 2)) 'b #f)  => 2\n  (assv-or '((a . 1)) 'missing 'default)  => default\n\nParameters:\n  opts : alist\n  key : any\n  fallback : any\nReturns: any\nCategory: algebra\nKeywords: options, alist, lookup, default, construction\n\nSee also: `validate-opts-keys'."
  (let ((p (assv key opts)))
    (if p (cdr p) fallback)))

(define (validate-opts-keys site opts known-keys)
  "Raise an error if any pair in OPTS has a car not in KNOWN-KEYS.\nUsed to reject typos (e.g. `'elements?' for `'element?') at construction\ntime rather than silently returning the fallback. SITE is a string\nidentifying the caller; it is prepended to the error message.\n\nExamples:\n  ;; No error:\n  (validate-opts-keys \"make-foo\" '((a . 1)) '(a b c))\n  ;; Raises: \"make-foo: unknown option key\"\n  ;; (validate-opts-keys \"make-foo\" '((x . 1)) '(a b c))\n\nParameters:\n  site : string\n  opts : alist\n  known-keys : list of symbol\nReturns: unspecified\nCategory: algebra\nKeywords: options, alist, validation, typo, construction\n\nSee also: `assv-or'."
  (for-each
    (lambda (pair)
      (unless (and (pair? pair) (memv (car pair) known-keys))
        (error (string-append site ": unknown option key") pair known-keys)))
    opts))
