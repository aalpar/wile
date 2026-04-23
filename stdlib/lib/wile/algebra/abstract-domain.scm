;;; (wile algebra abstract-domain) — Pre-built abstract interpretation domains
;;;
;;; Sibling of (wile algebra interval). Currently hosts the sign domain:
;;; a 5-element flat lattice {flat-bottom, neg, zero, pos, flat-top} with
;;; an abstraction function from integers and a sign arithmetic table for
;;; add/sub/mul.
;;;
;;; Extracted from wile-goast's goast/domains.scm to give the reusable
;;; algebraic content a home outside the Go-AST analysis pipeline.
;;;
;;; References:
;;;   Cousot & Cousot (1977) Abstract Interpretation: a Unified Lattice
;;;     Model for Static Analysis of Programs by Construction or
;;;     Approximation of Fixpoints.

;; ─── Sign lattice ───────────────────────────────

(define (sign-lattice)
  "Construct the sign lattice {flat-bottom, neg, zero, pos, flat-top}.
The three middle elements are incomparable; flat-bottom sits below them
and flat-top above. Standard five-point abstract domain for integer
signs in abstract interpretation.

Returns: lattice
Category: algebra
Keywords: sign, abstract domain, abstract interpretation, flat lattice

See also: `abstract-sign', `sign-binop', `flat-lattice'."
  (flat-lattice '(neg zero pos) eq?))

;; ─── Integer abstraction ────────────────────────

(define (abstract-sign n)
  "Abstract a concrete integer to its sign in `(sign-lattice)`.
Negative integers map to `neg`, zero to `zero`, and positive integers
to `pos`.

Parameters:
  n : integer
Returns: symbol
Category: algebra
Keywords: sign, abstraction, abstract interpretation

Examples:
  (abstract-sign -5)  => neg
  (abstract-sign 0)   => zero
  (abstract-sign 7)   => pos

See also: `sign-lattice', `sign-binop'."
  (cond ((< n 0) 'neg)
        ((= n 0) 'zero)
        (else    'pos)))

;; ─── Sign arithmetic table ──────────────────────
;;
;; Not a lattice operation; sign arithmetic for abstract interpretation.
;; Kept here as the natural complement to `sign-lattice` (cf. the
;; `(wile algebra interval)` pattern of pairing a lattice with its
;; transfer operations).

(define (sign? s)
  "Test whether S is a valid sign-lattice value.
Valid signs are `neg`, `zero`, `pos`, `flat-bottom`, `flat-top`.

Parameters:
  s : any
Returns: boolean
Category: algebra
Keywords: sign, predicate, abstract domain

See also: `sign-lattice', `abstract-sign', `sign-binop'."
  (and (symbol? s)
       (memq s '(neg zero pos flat-bottom flat-top))
       #t))

(define (sign-binop op a b)
  "Apply sign operator OP to sign values A and B.
OP is one of `add`, `sub`, `mul`. A and B are sign values drawn from
`(sign-lattice)`: `neg`, `zero`, `pos`, `flat-bottom`, or `flat-top`.
Returns the resulting sign.

Strict on `flat-bottom`: any operand being `flat-bottom` yields
`flat-bottom`. Multiplication by `zero` is `zero` even if the other
operand is `flat-top` (annihilation). Otherwise `flat-top` propagates.

Raises an error if OP is not one of `add`, `sub`, `mul`, or if A / B
is not a valid sign. Callers wanting a conservative default must
wrap `sign-binop` explicitly.

Parameters:
  op : symbol
  a : symbol
  b : symbol
Returns: symbol
Category: algebra
Keywords: sign, abstract domain, transfer, abstract interpretation, arithmetic

Examples:
  (sign-binop 'mul 'neg 'neg)            => pos
  (sign-binop 'mul 'zero 'flat-top)      => zero
  (sign-binop 'add 'neg 'pos)            => flat-top
  (sign-binop 'add 'flat-bottom 'pos)    => flat-bottom

See also: `sign?', `sign-lattice', `abstract-sign'."
  (unless (memq op '(add sub mul))
    (error "sign-binop: unknown operator" op))
  (unless (sign? a)
    (error "sign-binop: invalid sign for a" a))
  (unless (sign? b)
    (error "sign-binop: invalid sign for b" b))
  (let ((bot 'flat-bottom)
        (top 'flat-top))
    (cond
      ((or (eq? a bot) (eq? b bot)) bot)
      ((and (eq? op 'mul) (or (eq? a 'zero) (eq? b 'zero))) 'zero)
      ((or (eq? a top) (eq? b top)) top)
      (else
        (case op
          ((add)
           (case a
             ((neg)  (case b ((neg) 'neg)  ((zero) 'neg) ((pos) top)))
             ((zero) b)
             ((pos)  (case b ((neg) top)   ((zero) 'pos) ((pos) 'pos)))))
          ((sub)
           (case a
             ((neg)  (case b ((neg) top)   ((zero) 'neg) ((pos) 'neg)))
             ((zero) (case b ((neg) 'pos)  ((zero) 'zero) ((pos) 'neg)))
             ((pos)  (case b ((neg) 'pos)  ((zero) 'pos) ((pos) top)))))
          ((mul)
           (case a
             ((neg)  (case b ((neg) 'pos)  ((zero) 'zero) ((pos) 'neg)))
             ((zero) 'zero)
             ((pos)  (case b ((neg) 'neg)  ((zero) 'zero) ((pos) 'pos))))))))))
