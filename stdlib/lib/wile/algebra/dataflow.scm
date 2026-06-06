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
  (%make-cfg-protocol blocks-of-fn index-of-fn preds-of-fn succs-of-fn)
  cfg-protocol?
  (blocks-of-fn cfg-protocol-blocks-of-fn)
  (index-of-fn  cfg-protocol-index-of-fn)
  (preds-of-fn  cfg-protocol-preds-of-fn)
  (succs-of-fn  cfg-protocol-succs-of-fn))

(define (make-cfg-protocol blocks-of index-of preds-of succs-of)
  "Construct a CFG protocol record from four closures:
  BLOCKS-OF : fn   → list of blocks
  INDEX-OF  : blk  → identifier (eqv?-comparable; typically integer or symbol)
  PREDS-OF  : blk  → list of indices (or `#f`, treated as empty)
  SUCCS-OF  : blk  → list of indices (or `#f`, treated as empty)

All four arguments must be procedures. The record carries no data
itself — only the accessor closures that let `run-analysis`
traverse an arbitrary CFG representation.

Parameters:
  blocks-of : procedure
  index-of : procedure
  preds-of : procedure
  succs-of : procedure
Returns: cfg-protocol
Category: algebra
Keywords: CFG, control flow graph, protocol, dataflow

See also: `run-analysis', `cfg-protocol?', `cfg-blocks-of',
`cfg-index-of', `cfg-preds-of', `cfg-succs-of'."
  (unless (procedure? blocks-of)
    (error "make-cfg-protocol: blocks-of must be a procedure" blocks-of))
  (unless (procedure? index-of)
    (error "make-cfg-protocol: index-of must be a procedure" index-of))
  (unless (procedure? preds-of)
    (error "make-cfg-protocol: preds-of must be a procedure" preds-of))
  (unless (procedure? succs-of)
    (error "make-cfg-protocol: succs-of must be a procedure" succs-of))
  (%make-cfg-protocol blocks-of index-of preds-of succs-of))

;; Wrapper procedures — mirror `term-compound?` / `term-get-operator` etc.
;; in (wile algebra symbolic). Callers should prefer these over the raw
;; `cfg-protocol-*-fn` accessors (which are exported for completeness only).

(define (cfg-blocks-of proto fn)
  "Return the list of blocks of FN under CFG PROTO.

Parameters:
  proto : cfg-protocol
  fn : any
Returns: list
Category: algebra"
  ((cfg-protocol-blocks-of-fn proto) fn))

(define (cfg-index-of proto block)
  "Return the identifier of BLOCK under CFG PROTO. Result must be
eqv?-comparable (typically integer or symbol).

Parameters:
  proto : cfg-protocol
  block : any
Returns: any
Category: algebra"
  ((cfg-protocol-index-of-fn proto) block))

(define (cfg-preds-of proto block)
  "Return the predecessor indices of BLOCK under CFG PROTO. The
returned value may be `#f`, treated as the empty list.

Parameters:
  proto : cfg-protocol
  block : any
Returns: list-or-false
Category: algebra"
  ((cfg-protocol-preds-of-fn proto) block))

(define (cfg-succs-of proto block)
  "Return the successor indices of BLOCK under CFG PROTO. The
returned value may be `#f`, treated as the empty list.

Parameters:
  proto : cfg-protocol
  block : any
Returns: list-or-false
Category: algebra"
  ((cfg-protocol-succs-of-fn proto) block))

;; ─── Initial-state wrapper ──────────────────
;;
;; Tagged record so callers must wrap initial lattice values in
;; `(init-state x)` when passing through `run-analysis`'s varargs.
;; Prevents ambiguity with flag symbols (previously: a sign-lattice
;; consumer passing `'neg` as initial state was silently parsed as
;; a flag). The wrapper makes "initial state" distinguishable from
;; "flag" without relying on type heuristics.

(define-record-type <init-state>
  (init-state value)
  init-state?
  (value init-state-value))

;; `(widen OP)` is the optional widening-operator wrapper for run-analysis,
;; mirroring the `(init-state ...)` plumbing. OP is a binary operator
;; (current-iterate next-iterate) -> widened, applied at loop headers in
;; place of raw join so analyses over infinite-height lattices (intervals)
;; terminate. Absent ⇒ pure MFP (raw join everywhere), behavior unchanged.
(define-record-type <widen>
  (widen op)
  widen?
  (op widen-op))

;; ─── Reverse postorder ────────────────────

(define (reverse-postorder blocks protocol)
  "Compute reverse postorder of BLOCKS under CFG PROTOCOL.
DFS from the first block's index, prepending each node to the result
after its successors are visited — this builds reverse postorder
directly (no separate reverse step). Used by `run-analysis` to seed
worklist priority.

Only blocks reachable from `(car blocks)` via `succs-of` appear in
the returned list. Callers relying on orphan-block handling should
either ensure `(car blocks)` dominates the rest of the CFG or run
their own reachability pass upstream.

Parameters:
  blocks : list
  protocol : cfg-protocol
Returns: list
Category: algebra
Keywords: reverse postorder, DFS, worklist, dataflow

See also: `run-analysis'."
  (unless (cfg-protocol? protocol)
    (error "reverse-postorder: expected cfg-protocol" protocol))
  (let ((block-map (map (lambda (b) (cons (cfg-index-of protocol b) b)) blocks)))
    (define (succs-of-idx idx)
      (let ((entry (assv idx block-map)))
        (if entry
            (or (cfg-succs-of protocol (cdr entry)) '())
            (error "reverse-postorder: successor index not in blocks (malformed CFG)" idx))))
    (let ((visited '()) (result '()))
      (define (dfs idx)
        (unless (memv idx visited)
          (set! visited (cons idx visited))
          (for-each dfs (succs-of-idx idx))
          (set! result (cons idx result))))
      (if (null? blocks)
          '()
          (begin (dfs (cfg-index-of protocol (car blocks)))
                 result)))))

;; ─── Result accessors ─────────────────────

(define (analysis-in result block-idx)
  "Query the in-state at BLOCK-IDX from a `run-analysis' result.
Returns #f if the block is not present in RESULT.

Ambiguity note: if a lattice's bottom value is itself `#f` (e.g. a
2-element truth-value lattice where `lattice-bottom` is `#f`), this
accessor cannot distinguish `block missing` from `block present at
bottom`. That's an accepted trade-off: the accessor keeps the
single-value `assv`-style shape (as in `assv`/`assq`/`hash-table-ref/
default`) rather than introducing multi-value returns or a sentinel
for a rare edge case. Callers that need to disambiguate should use
`(assv block-idx (analysis-states r))` for an explicit presence
check; an `#f` here with an entry present means the state is
genuinely `#f`.

Parameters:
  result : list
  block-idx : any
Returns: any
Category: algebra
Keywords: dataflow, analysis, in state

See also: `analysis-out', `analysis-states', `run-analysis'."
  ;; `assv` yields the (idx in out) triple; `cadr` extracts `in`.
  ;; `(and entry ...)` collapses to #f when the block is absent —
  ;; see docstring on the ambiguity with #f-valued bottoms.
  (let ((entry (assv block-idx result)))
    (and entry (cadr entry))))

(define (analysis-out result block-idx)
  "Query the out-state at BLOCK-IDX from a `run-analysis' result.
Returns #f if the block is not present in RESULT.

Ambiguity note: if a lattice's bottom value is itself `#f` (e.g. a
2-element truth-value lattice where `lattice-bottom` is `#f`), this
accessor cannot distinguish `block missing` from `block present at
bottom`. That's an accepted trade-off: the accessor keeps the
single-value `assv`-style shape (as in `assv`/`assq`/`hash-table-ref/
default`) rather than introducing multi-value returns or a sentinel
for a rare edge case. Callers that need to disambiguate should use
`(assv block-idx (analysis-states r))` for an explicit presence
check; an `#f` here with an entry present means the state is
genuinely `#f`.

Parameters:
  result : list
  block-idx : any
Returns: any
Category: algebra
Keywords: dataflow, analysis, out state

See also: `analysis-in', `analysis-states', `run-analysis'."
  ;; `assv` yields the (idx in out) triple; `caddr` extracts `out`.
  ;; `(and entry ...)` collapses to #f when the block is absent —
  ;; see docstring on the ambiguity with #f-valued bottoms.
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

Optional args may appear in any order:
  - `(init-state VALUE)` — tagged initial state for seed block(s).
    Forward analyses seed `in[entry] = VALUE`; backward analyses seed
    `in[exit] = VALUE` (the boundary condition). Defaults to
    `lattice-bottom`. The tagged wrapper is required — bare symbols
    and other values are rejected — so that lattices whose elements
    are symbols (e.g. `sign-lattice`) can safely use this API.
  - `'check-monotone` — enable monotonicity-violation detection
    during iteration; raises if `transfer` produces a lower out-state
    on re-visit.
  - `(widen OP)` — tagged widening operator `(lambda (cur next) widened)`
    applied at loop headers (back-edge targets) in place of raw join.
    Required for termination on infinite-height lattices such as the
    interval lattice; without it those analyses do not converge. When
    absent, behavior is identical to pure MFP (raw join everywhere).
    See `interval-widen` for the standard interval operator.

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
  (run-analysis 'forward lat xfer fn proto (init-state 'neg) 'check-monotone)

See also: `make-cfg-protocol', `init-state', `analysis-in',
`analysis-out', `reverse-postorder'."
  (unless (memq direction '(forward backward))
    (error "run-analysis: direction must be 'forward or 'backward" direction))
  (unless (lattice? lattice)
    (error "run-analysis: expected lattice" lattice))
  (unless (procedure? transfer)
    (error "run-analysis: expected procedure for transfer" transfer))
  (unless (cfg-protocol? protocol)
    (error "run-analysis: expected cfg-protocol" protocol))
  (let*-values
      (((initial-state check-mono widen-fn)
        (let loop ((args args) (initial #f) (check-mono #f) (widen-fn #f))
          (cond
            ((null? args) (values (or initial (lattice-bottom lattice)) check-mono widen-fn))
            ((init-state? (car args))
             (when initial
               (error "run-analysis: multiple (init-state ...) arguments" args))
             (loop (cdr args) (init-state-value (car args)) check-mono widen-fn))
            ((widen? (car args))
             (when widen-fn
               (error "run-analysis: multiple (widen ...) arguments" args))
             (let ((op (widen-op (car args))))
               (unless (procedure? op)
                 (error "run-analysis: (widen OP) requires a procedure" op))
               (loop (cdr args) initial check-mono op)))
            ((eq? (car args) 'check-monotone)
             (loop (cdr args) initial #t widen-fn))
            (else
             (error "run-analysis: unexpected optional argument (use `(init-state x)` or `(widen op)`)"
                    (car args)))))))
    (let* ((blocks (cfg-blocks-of protocol fn))
           (forward? (eq? direction 'forward))
           (block-map (map (lambda (b) (cons (cfg-index-of protocol b) b)) blocks))
           (block-ref (lambda (idx)
                        (let ((e (assv idx block-map)))
                          (unless e
                            (error "run-analysis: index referenced by preds/succs is not in blocks (malformed CFG)" idx))
                          (cdr e))))
           (rpo (reverse-postorder blocks protocol))
           (order (if forward? rpo (reverse rpo)))
           (rank-map (let loop ((os order) (r 0) (m '()))
                       (if (null? os) m
                           (loop (cdr os) (+ r 1)
                                 (cons (cons (car os) r) m)))))
           (rank-of (lambda (idx)
                      (let ((e (assv idx rank-map)))
                        (unless e
                          (error "run-analysis: index not in rank map (broken CFG protocol?)" idx))
                        (cdr e))))
           (flow-preds (lambda (b)
                         (or (if forward?
                                 (cfg-preds-of protocol b)
                                 (cfg-succs-of protocol b))
                             '())))
           (flow-succs (lambda (b)
                         (or (if forward?
                                 (cfg-succs-of protocol b)
                                 (cfg-preds-of protocol b))
                             '())))
           ;; Widening points: a block is a back-edge target (loop header)
           ;; iff some flow-predecessor sits at or after it in flow order
           ;; (rank-of p >= rank-of b). Widening at these points forces
           ;; termination on infinite-height lattices (design §5). Computed
           ;; once, and only when a widen operator was supplied — pure MFP
           ;; pays nothing. For irreducible CFGs this flags a sound superset
           ;; of the natural-loop headers (RPO-rank back-edges), which costs
           ;; precision but never termination.
           ;;
           ;; rank-or-false tolerates indices absent from the RPO rank-map —
           ;; blocks (or predecessors) unreachable from entry. Pure MFP already
           ;; tolerates such orphans (their out-state stays bottom and joins
           ;; harmlessly), so the widening detector must not crash on them. An
           ;; orphan cannot lie on a cycle reachable from entry, so it is never
           ;; a widening point.
           (rank-or-false (lambda (idx)
                            (let ((e (assv idx rank-map)))
                              (and e (cdr e)))))
           (widening-points
             (if widen-fn
                 (filter-map
                   (lambda (b)
                     (let* ((idx (cfg-index-of protocol b))
                            (ridx (rank-or-false idx)))
                       (and ridx
                            (let any-back ((ps (flow-preds b)))
                              (cond ((null? ps) #f)
                                    ((let ((rp (rank-or-false (car ps))))
                                       (and rp (>= rp ridx)))
                                     #t)
                                    (else (any-back (cdr ps)))))
                            idx)))
                   blocks)
                 '()))
           (widening-point? (lambda (idx) (and widen-fn (memv idx widening-points) #t)))
           (entry-idx (if (null? blocks) #f (cfg-index-of protocol (car blocks))))
           (exit-idxs (filter-map
                        (lambda (b)
                          (let ((s (or (cfg-succs-of protocol b) '())))
                            (and (null? s) (cfg-index-of protocol b))))
                        blocks))
           (seed-idxs (if forward?
                          (if entry-idx (list entry-idx) '())
                          exit-idxs))
           (bot (lattice-bottom lattice))
           ;; Uniform seeding: initial-state sits at `in` for seed blocks
           ;; regardless of direction. Under forward, seed is the entry
           ;; block; initial-state is `in[entry]`. Under backward, seed
           ;; is an exit block; initial-state is `in[exit]` — the
           ;; boundary condition, which in program order is the state
           ;; after the exit block. Transfer produces `out` on the first
           ;; visit. Q-a fix — previously backward seeded `out`, which
           ;; the main loop immediately overwrote.
           (states (map (lambda (b)
                          (let ((idx (cfg-index-of protocol b)))
                            (if (memv idx seed-idxs)
                                (list idx initial-state bot)
                                (list idx bot bot))))
                        blocks)))
      (define (get-in idx) (cadr (assv idx states)))
      (define (get-out idx) (caddr (assv idx states)))
      (define (set-state! idx in-val out-val)
        (set! states
          (map (lambda (entry)
                 (if (eqv? (car entry) idx)
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
                   (joined-in (if (null? pred-idxs)
                                  (if (memv idx seed-idxs) initial-state bot)
                                  (let join-preds ((ps pred-idxs)
                                                   (acc (if (memv idx seed-idxs)
                                                            initial-state
                                                            bot)))
                                    (if (null? ps) acc
                                        (join-preds (cdr ps)
                                          (lattice-join lattice acc
                                            (get-out (car ps))))))))
                   ;; At loop headers, widen the previous in-state against the
                   ;; newly joined one instead of taking the raw join — bounds
                   ;; the ascending chain so infinite-height lattices converge.
                   ;; Elsewhere (and always under pure MFP) this is the raw join.
                   (in-val (if (widening-point? idx)
                               (widen-fn (get-in idx) joined-in)
                               joined-in))
                   (out-val (transfer blk in-val))
                   (old-out (get-out idx)))
              (when (and check-mono
                         (not (lattice-leq? lattice old-out out-val)))
                (error "run-analysis: monotonicity violation"
                       (list 'block idx 'in in-val
                             'old-out old-out 'new-out out-val)))
              (set-state! idx in-val out-val)
              (if (lattice-leq? lattice out-val old-out)
                  (loop rest-wl)
                  (loop (worklist-insert-all rest-wl
                          (flow-succs blk))))))))))
