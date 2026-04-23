;; ================================================================
;; Chapter 09 -- Dataflow analysis: MFP solver on the sign domain
;;
;; What you will learn:
;;   - The sign lattice from (wile algebra abstract-domain) and the
;;     accompanying `abstract-sign` / `sign-binop` operators.
;;   - How to supply a CFG to the MFP solver via make-cfg-protocol --
;;     four closures describe shape; block interior stays opaque to
;;     the solver.
;;   - Running forward dataflow via run-analysis and reading results
;;     via analysis-in / analysis-out.
;;   - How the lattice bounds the analysis: joining `neg` and `pos`
;;     at a merge point yields `flat-top` (the "I don't know" element).
;;
;; Prerequisites: chapter 02 (lattice, join), chapter 08 (sign lattice).
;; Sub-libraries used:
;;   (wile algebra lattice), (wile algebra abstract-domain),
;;   (wile algebra dataflow).
;; ================================================================

(import (scheme base) (scheme write)
        (wile algebra lattice)
        (wile algebra abstract-domain)
        (wile algebra dataflow))
(include "../lib/check.scm")

;; ----------------------------------------------------------------
;; Part 1: The sign lattice.
;;
;; Five elements: flat-bottom (the "unreachable" state), neg, zero,
;; pos (the three atoms, mutually incomparable), flat-top (the
;; "could be anything" state).
;; ----------------------------------------------------------------

(define L (sign-lattice))
(check-true (lattice? L)                         "sign-lattice is a lattice")
(check= (lattice-bottom L)  'flat-bottom         "bottom is flat-bottom")
(check= (lattice-top L)     'flat-top            "top is flat-top")

;; Joining two distinct atoms climbs to top -- "I don't know which sign."
(check= (lattice-join L 'neg 'pos)  'flat-top    "neg v pos = top")
(check= (lattice-join L 'neg 'neg)  'neg         "neg v neg = neg")
(check= (lattice-join L 'neg 'flat-bottom) 'neg  "neg v bot = neg")

;; ----------------------------------------------------------------
;; Part 2: Abstracting concrete integers.
;; ----------------------------------------------------------------

(check= (abstract-sign  -5)   'neg    "abstract -5")
(check= (abstract-sign   0)   'zero   "abstract 0")
(check= (abstract-sign   7)   'pos    "abstract 7")

;; ----------------------------------------------------------------
;; Part 3: Sign arithmetic.
;;
;; sign-binop applies a sign operator without losing information you
;; can still prove. Multiplying two negatives gives positive. Adding
;; two positives stays positive. Adding neg and pos, you lose the
;; sign -- it becomes top.
;; ----------------------------------------------------------------

(check= (sign-binop 'mul 'neg 'neg)            'pos    "neg * neg = pos")
(check= (sign-binop 'mul 'pos 'pos)            'pos    "pos * pos = pos")
(check= (sign-binop 'mul 'zero 'flat-top)      'zero   "anything * 0 = 0 (annihilation)")
(check= (sign-binop 'add 'neg 'pos)            'flat-top "neg + pos = top (sign lost)")
(check= (sign-binop 'add 'pos 'pos)            'pos    "pos + pos stays pos")
(check= (sign-binop 'add 'flat-bottom 'pos)    'flat-bottom
        "bottom absorbs (unreachable state)")

;; ----------------------------------------------------------------
;; Part 4: A tiny CFG in alist form, and a protocol for it.
;;
;; We represent each block as an alist with 'id, 'op, 'args, 'preds,
;; 'succs. The CFG protocol exposes the four closures the solver needs.
;;
;; Program:
;;   B0:  x = 5                 (x becomes pos)
;;   B1:  y = x * x             (y becomes pos)
;;   B2:  z = y - y             (z becomes... what?)
;; ----------------------------------------------------------------

(define linear-cfg
  `((b0 (op . const)  (val . 5)          (preds) (succs b1))
    (b1 (op . mul)    (args . (b0 b0))   (preds b0) (succs b2))
    (b2 (op . sub)    (args . (b1 b1))   (preds b1) (succs))))

(define (blk-id    b) (car b))
(define (blk-field b k) (cdr (assq k (cdr b))))
(define (blk-preds b) (or (blk-field b 'preds) '()))
(define (blk-succs b) (or (blk-field b 'succs) '()))

(define linear-proto
  (make-cfg-protocol
    (lambda (fn) fn)              ; blocks-of: identity (fn is the block list)
    blk-id
    blk-preds
    blk-succs))

(check-true (cfg-protocol? linear-proto)           "protocol built")

;; `reverse-postorder` gives the solver's visit order.
(check= (reverse-postorder linear-cfg linear-proto)
        '(b0 b1 b2)
        "RPO for straight-line CFG")

;; ----------------------------------------------------------------
;; Part 5: Defining the transfer function.
;;
;; The transfer maps (block, in-state) to out-state. For our domain
;; the state is a sign (of `the variable we are tracking`), and the
;; transfer interprets the block's operation.
;;
;; To keep the example small, we track one variable per block (the
;; value assigned). The transfer uses the in-state as the sign of the
;; operand(s).
;; ----------------------------------------------------------------

(define (linear-transfer block in-state)
  (case (blk-field block 'op)
    ((const) (abstract-sign (blk-field block 'val)))
    ((mul)   (sign-binop 'mul in-state in-state))
    ((sub)   (sign-binop 'sub in-state in-state))
    (else    in-state)))

;; ----------------------------------------------------------------
;; Part 6: Run the analysis.
;;
;; Forward direction: propagate from entry. Initial state defaults
;; to lattice-bottom; we pass (init-state 'flat-top) if we wanted to
;; start at top instead. For this linear chain the initial state does
;; not matter because B0 produces `pos` directly.
;; ----------------------------------------------------------------

(define result (run-analysis 'forward L linear-transfer linear-cfg linear-proto))

(check-true (list? result)                        "analysis returns a result alist")
(check= (length result) 3                         "one entry per block")

;; B0: out = abstract-sign(5) = pos.
(check= (analysis-out result 'b0)   'pos          "B0 out: 5 is pos")

;; B1: out = mul(pos, pos) = pos.
(check= (analysis-out result 'b1)   'pos          "B1 out: pos * pos = pos")

;; B2: out = sub(pos, pos) = top (could be anything).
;; This is the interesting case: the analysis correctly reports that
;; subtracting two positives does not keep the sign.
(check= (analysis-out result 'b2)   'flat-top     "B2 out: pos - pos = top")

;; ----------------------------------------------------------------
;; Part 7: Branching CFG -- merge point joins sign states.
;;
;; Program:
;;       B0: x = ?         (unknown, top)
;;        \
;;        B1: y = x + 1    (still top)
;;        / \
;;     B2   B3             (two branches)
;;        \ /
;;        B4                (merge)
;;
;; Sign lattice at merge joins the incoming states. If B2 says `pos`
;; and B3 says `neg`, the merge goes to top.
;; ----------------------------------------------------------------

(define branch-cfg
  `((b0 (op . const)  (val . 5)    (preds) (succs b1))          ; x = 5 (pos)
    (b1 (op . neg-it)               (preds b0) (succs b2 b3))    ; branch on y
    (b2 (op . ident)                (preds b1) (succs b4))       ; y = x
    (b3 (op . neg-it)               (preds b1) (succs b4))       ; y = -x
    (b4 (op . ident)                (preds b2 b3) (succs))))     ; merge

(define (branch-transfer block in-state)
  (case (blk-field block 'op)
    ((const)   (abstract-sign (blk-field block 'val)))
    ((ident)   in-state)
    ((neg-it)  (sign-binop 'sub 'zero in-state))
    (else      in-state)))

(define br-result (run-analysis 'forward L branch-transfer branch-cfg linear-proto))

(check= (analysis-out br-result 'b0)  'pos
        "B0: const 5 is pos")
;; B1 negates the entry sign: pos -> neg.
(check= (analysis-out br-result 'b1)  'neg
        "B1: neg-it of pos is neg")
(check= (analysis-out br-result 'b2)  'neg
        "B2: identity of B1's out (neg)")
(check= (analysis-out br-result 'b3)  'pos
        "B3: neg-it of neg is pos")

;; Merge: B4's in-state = join of B2's out (neg) and B3's out (pos) = top.
;; B4 is identity, so its out is also top.
(check= (analysis-in  br-result 'b4)  'flat-top
        "B4 in: neg v pos = top (lost sign at merge)")
(check= (analysis-out br-result 'b4)  'flat-top
        "B4 out: identity of top")

;; ----------------------------------------------------------------
;; Part 8: Asking for an initial state.
;;
;; The seed (entry block in forward analysis) starts from whatever
;; the caller passes via (init-state x). If we seed the analysis with
;; `pos`, B0's in-state is `pos`. But B0 is a const block that
;; overwrites in-state regardless, so the out is still `pos`.
;; ----------------------------------------------------------------

(define seeded
  (run-analysis 'forward L linear-transfer linear-cfg linear-proto
                (init-state 'pos)))

(check= (analysis-in  seeded 'b0)   'pos
        "seeded in-state at entry")
(check= (analysis-out seeded 'b0)   'pos
        "entry out-state still driven by transfer")

(display "chapter 09 complete") (newline)
