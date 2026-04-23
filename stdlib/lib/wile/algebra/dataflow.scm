;;; (wile algebra dataflow) — MFP worklist dataflow solver
;;;
;;; Lattice-parameterized forward/backward fixpoint analysis over a
;;; CFG-protocol-abstracted control flow graph. The protocol separates
;;; the algorithm (here) from the CFG representation (supplied by the
;;; caller via `make-cfg-protocol`).
;;;
;;; References:
;;;   Kildall (1973) A Unified Approach to Global Program Optimization.
;;;   Kam & Ullman (1976) Global Data Flow Analysis and Iterative
;;;     Algorithms — MFP vs MOP on distributive frameworks.
;;;   Cousot & Cousot (1977) Abstract Interpretation: a Unified Lattice
;;;     Model for Static Analysis of Programs by Construction or
;;;     Approximation of Fixpoints.
;;;
;;; Extracted from wile-goast's goast/dataflow.scm L99-239, generalized
;;; over CFG shape. wile-goast supplies an `ssa-cfg-protocol` adapter
;;; for its SSA function shape.

;; ─── CFG protocol ─────────────────────────
;;
;; Four closures describe the CFG shape to the solver:
;;   blocks-of : fn   → list of blocks
;;   index-of  : blk  → identifier (eqv?-comparable, typically integer)
;;   preds-of  : blk  → list of indices
;;   succs-of  : blk  → list of indices
;;
;; Block interior is opaque to `run-analysis` — only the `transfer`
;; procedure knows what's inside a block. This is what makes the
;; solver CFG-shape-agnostic.

(define-record-type <cfg-protocol>
  (make-cfg-protocol blocks-of index-of preds-of succs-of)
  cfg-protocol?
  (blocks-of cfg-protocol-blocks-of)
  (index-of  cfg-protocol-index-of)
  (preds-of  cfg-protocol-preds-of)
  (succs-of  cfg-protocol-succs-of))

;; ─── Reverse postorder ────────────────────

(define (reverse-postorder blocks protocol)
  "Compute reverse postorder of BLOCKS under CFG PROTOCOL.
DFS from the first block's index, record block indices in postorder,
then reverse. Used by `run-analysis` to seed worklist priority.

Parameters:
  blocks : list
  protocol : cfg-protocol
Returns: list
Category: algebra
Keywords: reverse postorder, DFS, worklist, dataflow

See also: `run-analysis'."
  (let ((index-of (cfg-protocol-index-of protocol))
        (succs-of (cfg-protocol-succs-of protocol)))
    (let ((block-map (map (lambda (b) (cons (index-of b) b)) blocks)))
      (define (succs-of-idx idx)
        (let ((entry (assv idx block-map)))
          (if entry (or (succs-of (cdr entry)) '()) '())))
      (let ((visited '()) (result '()))
        (define (dfs idx)
          (unless (memv idx visited)
            (set! visited (cons idx visited))
            (for-each dfs (succs-of-idx idx))
            (set! result (cons idx result))))
        (if (null? blocks)
            '()
            (begin (dfs (index-of (car blocks)))
                   result))))))

;; ─── Result accessors ─────────────────────

(define (analysis-in result block-idx)
  "Query the in-state at BLOCK-IDX from a `run-analysis' result.
Returns #f if the block is not present in RESULT.

Parameters:
  result : list
  block-idx : any
Returns: any
Category: algebra
Keywords: dataflow, analysis, in state

See also: `analysis-out', `analysis-states', `run-analysis'."
  (let ((entry (assv block-idx result)))
    (and entry (cadr entry))))

(define (analysis-out result block-idx)
  "Query the out-state at BLOCK-IDX from a `run-analysis' result.
Returns #f if the block is not present in RESULT.

Parameters:
  result : list
  block-idx : any
Returns: any
Category: algebra
Keywords: dataflow, analysis, out state

See also: `analysis-in', `analysis-states', `run-analysis'."
  (let ((entry (assv block-idx result)))
    (and entry (caddr entry))))

(define (analysis-states result)
  "Return the full per-block result alist from `run-analysis'.
Each entry is `(idx in out)'.

Parameters:
  result : list
Returns: list
Category: algebra
Keywords: dataflow, analysis, states

See also: `analysis-in', `analysis-out', `run-analysis'."
  result)

;; ─── MFP worklist solver ──────────────────

(define (run-analysis direction lattice transfer fn protocol . args)
  "Run worklist-based MFP dataflow analysis on a CFG.
DIRECTION is `forward` or `backward`. LATTICE is a `<lattice>` over
per-block states. TRANSFER is `(lambda (block in-state) out-state)`.
FN is the CFG-bearing function; PROTOCOL adapts FN to the solver.

Optional args: an initial state value (defaults to `lattice-bottom`),
and the symbol `check-monotone` to enable monotonicity-violation
detection during iteration. These can appear in either order.

Returns a per-block result alist shaped `((idx in out) ...)`. Use
`analysis-in` / `analysis-out` / `analysis-states` to query.

Terminates when no block's out-state changes across a worklist pass.
Worklist is rank-ordered by reverse postorder (forward) or reverse
of reverse postorder (backward) for efficient propagation.

Parameters:
  direction : symbol
  lattice : lattice
  transfer : procedure
  fn : any
  protocol : cfg-protocol
Returns: list
Category: algebra
Keywords: dataflow, MFP, worklist, fixpoint, monotone framework

Examples:
  (run-analysis 'forward (sign-lattice) my-transfer fn (ssa-cfg-protocol))
  (run-analysis 'forward lat xfer fn proto init-state 'check-monotone)

See also: `make-cfg-protocol', `analysis-in', `analysis-out',
`reverse-postorder'."
  (let* ((initial-state (if (and (pair? args) (not (symbol? (car args))))
                            (car args)
                            (lattice-bottom lattice)))
         (flags (if (and (pair? args) (not (symbol? (car args))))
                    (cdr args)
                    args))
         (check-mono (and (memq 'check-monotone flags) #t))
         (blocks-of (cfg-protocol-blocks-of protocol))
         (index-of  (cfg-protocol-index-of protocol))
         (preds-of  (cfg-protocol-preds-of protocol))
         (succs-of  (cfg-protocol-succs-of protocol))
         (blocks (blocks-of fn))
         (forward? (eq? direction 'forward))
         (block-map (map (lambda (b) (cons (index-of b) b)) blocks))
         (block-ref (lambda (idx) (cdr (assv idx block-map))))
         (rpo (reverse-postorder blocks protocol))
         (order (if forward? rpo (reverse rpo)))
         (rank-map (let loop ((os order) (r 0) (m '()))
                     (if (null? os) m
                         (loop (cdr os) (+ r 1)
                               (cons (cons (car os) r) m)))))
         (rank-of (lambda (idx)
                    (let ((e (assv idx rank-map)))
                      (if e (cdr e) 999))))
         (flow-preds (lambda (b)
                       (or (if forward? (preds-of b) (succs-of b)) '())))
         (flow-succs (lambda (b)
                       (or (if forward? (succs-of b) (preds-of b)) '())))
         (entry-idx (if (null? blocks) #f (index-of (car blocks))))
         (exit-idxs (filter-map
                      (lambda (b)
                        (let ((s (succs-of b)))
                          (and (or (not s) (null? s)) (index-of b))))
                      blocks))
         (seed-idxs (if forward?
                        (if entry-idx (list entry-idx) '())
                        exit-idxs))
         (bot (lattice-bottom lattice))
         (states (map (lambda (b)
                        (let ((idx (index-of b)))
                          (if (memv idx seed-idxs)
                              (if forward?
                                  (list idx initial-state bot)
                                  (list idx bot initial-state))
                              (list idx bot bot))))
                      blocks)))
    (define (get-in idx) (cadr (assv idx states)))
    (define (get-out idx) (caddr (assv idx states)))
    (define (set-state! idx in-val out-val)
      (set! states
        (map (lambda (entry)
               (if (equal? (car entry) idx)
                   (list idx in-val out-val)
                   entry))
             states)))
    (define (worklist-insert wl idx)
      (if (memv idx wl) wl
          (let insert ((rest wl))
            (cond ((null? rest) (list idx))
                  ((<= (rank-of idx) (rank-of (car rest)))
                   (cons idx rest))
                  (else (cons (car rest) (insert (cdr rest))))))))
    (define (worklist-insert-all wl idxs)
      (let loop ((is idxs) (w wl))
        (if (null? is) w
            (loop (cdr is) (worklist-insert w (car is))))))
    (let loop ((wl (worklist-insert-all '() seed-idxs)))
      (if (null? wl)
          states
          (let* ((idx (car wl))
                 (rest-wl (cdr wl))
                 (blk (block-ref idx))
                 (pred-idxs (flow-preds blk))
                 (in-val (if (null? pred-idxs)
                             (if (memv idx seed-idxs)
                                 (if forward? initial-state bot)
                                 bot)
                             (let join-preds ((ps pred-idxs)
                                              (acc (if (and (memv idx seed-idxs) forward?)
                                                       initial-state
                                                       bot)))
                               (if (null? ps) acc
                                   (join-preds (cdr ps)
                                     (lattice-join lattice acc
                                       (get-out (car ps))))))))
                 (out-val (transfer blk in-val))
                 (old-out (get-out idx)))
            (when (and check-mono
                       (not (lattice-leq? lattice old-out out-val)))
              (error (string-append "run-analysis: monotonicity violation at block "
                                    (number->string idx))))
            (set-state! idx in-val out-val)
            (if (lattice-leq? lattice out-val old-out)
                (loop rest-wl)
                (loop (worklist-insert-all rest-wl
                        (flow-succs blk)))))))))
