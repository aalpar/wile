;;; algebra-dataflow-test.scm — (wile algebra dataflow)

(import (scheme base)
        (chibi test)
        (wile algebra lattice)
        (wile algebra interval)
        (wile algebra dataflow))

(test-begin "dataflow")

;;; --- Fixtures -----------------------------------------------------------

;; Alist-shaped CFG: (idx (preds ...) (succs ...))
(define diamond-cfg
  '((0 () (1 2))
    (1 (0) (3))
    (2 (0) (3))
    (3 (1 2) ())))

(define linear-cfg
  '((0 () (1))
    (1 (0) (2))
    (2 (1) (3))
    (3 (2) ())))

(define single-block-cfg
  '((0 () ())))

(define three-block-cfg
  '((0 () (1)) (1 (0) (2)) (2 (1) ())))

;; Loop 1 <-> 2 with an exit arm 3. The successor order (2 3) matters: it is
;; what makes a block-1 revisit enqueue 2 before 3.
(define loop-exit-cfg
  '((0 () (1))
    (1 (0 2) (2 3))
    (2 (1) (1))
    (3 (1) ())))

(define test-protocol
  (make-cfg-protocol
    (lambda (fn) fn)   ; fn is the block list itself
    car                ; index-of
    cadr               ; preds-of
    caddr))            ; succs-of

;; 2-element truth-value lattice for tests (semantically the boolean
;; reachability lattice — not to be confused with the powerset
;; (boolean-lattice n) from (wile algebra lattice)).
(define (truth-value-lattice)
  (make-lattice
    (lambda (a b) (or a b))          ; join
    (lambda (a b) (and a b))         ; meet
    #f                                ; bottom
    #t                                ; top
    (lambda (a b) (or (not a) b))))  ; leq (implication)

;;; --- cfg-protocol record -------------------------------------------------

(test-group "cfg-protocol — record"
  (test #t (cfg-protocol? test-protocol))
  (test #f (cfg-protocol? '(not a protocol))))

(test-group "cfg-protocol — wrapper procs"
  (test 0       (cfg-index-of test-protocol '(0 () (1 2))))
  (test '()    (cfg-preds-of test-protocol '(0 () (1 2))))
  (test '(1 2) (cfg-succs-of test-protocol '(0 () (1 2))))
  (test diamond-cfg (cfg-blocks-of test-protocol diamond-cfg)))

(test-group "cfg-protocol — raw -fn accessors"
  ;; Raw closures are available too, for callers that need them.
  (test 0 ((cfg-protocol-index-of-fn test-protocol) '(0 () (1 2))))
  (test '(1 2) ((cfg-protocol-succs-of-fn test-protocol) '(0 () (1 2)))))

;;; --- reverse-postorder ---------------------------------------------------

(test-group "reverse-postorder — diamond"
  (let ((rpo (reverse-postorder diamond-cfg test-protocol)))
    ;; RPO begins at the entry block (0).
    (test 0 (car rpo))
    ;; Join block (3) comes last; all blocks present.
    (test 4 (length rpo))
    (test #t (if (memv 3 rpo) #t #f))
    (test 3 (list-ref rpo 3))))

(test-group "reverse-postorder — linear"
  (test '(0 1 2 3) (reverse-postorder linear-cfg test-protocol)))

(test-group "reverse-postorder — single block"
  (test '(0) (reverse-postorder single-block-cfg test-protocol)))

(test-group "reverse-postorder — empty"
  (test '() (reverse-postorder '() test-protocol)))

;;; --- run-analysis forward, trivial ---------------------------------------

(test-group "run-analysis — forward identity transfer, bottom lattice"
  ;; Identity transfer with bottom-initialized: everything stays bottom.
  (let* ((L (truth-value-lattice))
         (xfer (lambda (blk in) in))
         (result (run-analysis 'forward L xfer diamond-cfg test-protocol)))
    (test #f (analysis-out result 0))
    (test #f (analysis-out result 3))
    (test 4 (length result))))

(test-group "run-analysis — forward reachability"
  ;; Transfer sets out-state to #t unconditionally: reachability.
  ;; With entry seeded #t, every block should reach #t.
  (let* ((L (truth-value-lattice))
         (xfer (lambda (blk in) #t))
         (result (run-analysis 'forward L xfer diamond-cfg test-protocol
                                (init-state #t))))
    (test #t (analysis-out result 0))
    (test #t (analysis-out result 1))
    (test #t (analysis-out result 2))
    (test #t (analysis-out result 3))))

;;; --- run-analysis backward -----------------------------------------------

(test-group "run-analysis — backward, exit-seeded, lift transfer"
  ;; Backward with a transfer that unconditionally raises to #t
  ;; (models 'this block kills and regenerates'). With #t flowing
  ;; from exit backwards, predecessors' in/out both reach #t.
  (let* ((L (truth-value-lattice))
         (xfer (lambda (blk in) #t))
         (result (run-analysis 'backward L xfer diamond-cfg test-protocol)))
    (test #t (analysis-out result 3))
    (test #t (analysis-out result 2))
    (test #t (analysis-out result 1))
    (test #t (analysis-out result 0))))

;;; --- analysis-* accessors -----------------------------------------------

(test-group "analysis-in/out/states on missing block"
  (let* ((L (truth-value-lattice))
         (xfer (lambda (blk in) in))
         (result (run-analysis 'forward L xfer linear-cfg test-protocol)))
    (test #f (analysis-in result 999))
    (test #f (analysis-out result 999))
    ;; analysis-states returns the full alist.
    (test 4 (length (analysis-states result)))))

;;; --- run-analysis with initial-state + flag in either order -------------

(test-group "run-analysis — flag-then-nothing, no initial"
  ;; Caller passes only 'check-monotone; initial defaults to bottom.
  (let* ((L (truth-value-lattice))
         (xfer (lambda (blk in) in))  ; monotone (identity)
         (result (run-analysis 'forward L xfer linear-cfg test-protocol
                                'check-monotone)))
    ;; Identity transfer on bottom-seeded analysis: all bottom.
    (test #f (analysis-out result 0))
    (test #f (analysis-out result 3))))

;;; --- Monotonicity check --------------------------------------------------

(test-group "run-analysis — monotone violation is detected"
  ;; Cyclic CFG (0→1, 1→0) guarantees block 0 is re-visited.
  ;; Transfer outputs #t on first visit, #f on second — non-monotone.
  (let* ((cyclic-cfg '((0 (1) (1)) (1 (0) (0))))
         (L (truth-value-lattice))
         (block0-visits 0)
         (xfer (lambda (blk in)
                 (cond
                   ((equal? (car blk) 0)
                    (set! block0-visits (+ block0-visits 1))
                    (if (= block0-visits 1) #t #f))
                   (else in)))))
    (test-error (run-analysis 'forward L xfer cyclic-cfg test-protocol
                              'check-monotone))))

;;; --- Linear CFG ---------------------------------------------------------

(test-group "run-analysis — linear CFG forward"
  ;; Propagate #t from entry through a 4-block chain.
  (let* ((L (truth-value-lattice))
         (xfer (lambda (blk in) in))
         (result (run-analysis 'forward L xfer linear-cfg test-protocol
                                (init-state #t))))
    (test #t (analysis-in result 0))
    (test #t (analysis-out result 0))
    (test #t (analysis-in result 3))
    (test #t (analysis-out result 3))))

;;; --- Single-block CFG ---------------------------------------------------

(test-group "run-analysis — single block"
  (let* ((L (truth-value-lattice))
         (xfer (lambda (blk in) in))
         (result (run-analysis 'forward L xfer single-block-cfg test-protocol
                                (init-state #t))))
    (test 1 (length result))
    (test #t (analysis-in result 0))
    (test #t (analysis-out result 0))))

;;; --- run-analysis + make-cfg-protocol validation ------------------------

(test-group "make-cfg-protocol — procedure validation"
  (test-error (make-cfg-protocol 42 car cadr caddr))
  (test-error (make-cfg-protocol (lambda (fn) fn) #f cadr caddr))
  (test-error (make-cfg-protocol (lambda (fn) fn) car 'not-a-proc caddr))
  (test-error (make-cfg-protocol (lambda (fn) fn) car cadr "nope")))

(test-group "run-analysis — argument validation"
  (let ((L (truth-value-lattice))
        (xfer (lambda (blk in) in)))
    (test-error (run-analysis 'diagonal L xfer diamond-cfg test-protocol))
    (test-error (run-analysis 'forward  "not a lattice" xfer diamond-cfg test-protocol))
    (test-error (run-analysis 'forward  L 42 diamond-cfg test-protocol))
    (test-error (run-analysis 'forward  L xfer diamond-cfg 'not-a-protocol))))

;;; --- Positive fixpoint on a cyclic CFG ---------------------------------

(test-group "run-analysis — cyclic CFG reaches fixpoint"
  ;; 0 ↔ 1 mutually referential. Monotone "lift to #t on first visit"
  ;; transfer: once any block sees #t it stays #t, so the fixpoint is
  ;; all-#t; the solver must converge rather than spin.
  (let* ((cyclic-cfg '((0 (1) (1)) (1 (0) (0))))
         (L (truth-value-lattice))
         (xfer (lambda (blk in) (or in #t)))
         (result (run-analysis 'forward L xfer cyclic-cfg test-protocol
                                (init-state #t))))
    (test 2 (length result))
    (test #t (analysis-out result 0))
    (test #t (analysis-out result 1))))

;;; --- init-state wrapper validation -------------------------------------

(test-group "run-analysis — bare initial state rejected (Q-b)"
  ;; Raw #t (not wrapped with init-state) now raises.
  (let* ((L (truth-value-lattice))
         (xfer (lambda (blk in) in)))
    (test-error (run-analysis 'forward L xfer linear-cfg test-protocol #t))
    (test-error (run-analysis 'forward L xfer linear-cfg test-protocol 'not-a-flag))))

(test-group "run-analysis — duplicate init-state rejected"
  (let* ((L (truth-value-lattice))
         (xfer (lambda (blk in) in)))
    (test-error (run-analysis 'forward L xfer linear-cfg test-protocol
                              (init-state #t) (init-state #f)))))

(test-group "init-state — record"
  (test #t (init-state? (init-state 'neg)))
  (test #f (init-state? 'neg))
  (test #f (init-state? #t))
  (test 'neg (init-state-value (init-state 'neg)))
  (test 42  (init-state-value (init-state 42))))

;;; --- Q-a fix: backward initial-state now propagates --------------------

(test-group "run-analysis — backward initial-state propagates (Q-a fix)"
  ;; With identity transfer and initial-state=#t seeded at the exit,
  ;; every block must finish at #t — the fix in seeding puts
  ;; initial-state at `in` rather than `out` for backward seeds.
  (let* ((L (truth-value-lattice))
         (xfer (lambda (blk in) in))
         (result (run-analysis 'backward L xfer diamond-cfg test-protocol
                                (init-state #t))))
    (test #t (analysis-in result 3))
    (test #t (analysis-out result 3))
    ;; Predecessors of the exit (blocks 1, 2) receive #t via join.
    (test #t (analysis-out result 1))
    (test #t (analysis-out result 2))
    (test #t (analysis-out result 0))))

;;; --- widen wrapper record ----------------------------------------------

(test-group "widen — record"
  (test #t (widen? (widen interval-widen)))
  (test #f (widen? 'interval-widen))
  (test #f (widen? (init-state 5)))
  (test interval-widen (widen-op (widen interval-widen))))

;;; --- Interval analysis: widening forces termination --------------------

(test-group "run-analysis — interval increment loop converges under widening"
  ;; Self-looping header models `x := 0; while(?) x := x + 1`.
  ;; Block 0: entry, identity (out = seeded [0,0]).
  ;; Block 1: loop header with a self back-edge; transfer x := x + 1.
  ;; Over the infinite-height interval lattice this only terminates with
  ;; a widening operator. The result proves x >= 0 at the header.
  (let* ((loop-cfg '((0 () (1))
                     (1 (0 1) (1))))
         (L (interval-lattice))
         (xfer (lambda (blk in)
                 (if (= (car blk) 1)
                     (interval-add in '(1 . 1))
                     in)))
         (result (run-analysis 'forward L xfer loop-cfg test-protocol
                                (init-state (abstract-interval 0))
                                (widen interval-widen))))
    ;; in[1] = [0, +inf): lower bound pinned at 0 (proves x >= 0),
    ;; upper bound widened to infinity.
    (test '(0 . pos-inf) (analysis-in result 1))
    (test '(1 . pos-inf) (analysis-out result 1))))

(test-group "run-analysis — interval loop WITHOUT widening caps, never hangs"
  ;; Same self-looping increment CFG as above, but with NO (widen ...). Over
  ;; the infinite-height interval lattice the in-state ascends forever
  ;; ([0,0] < [0,1] < [0,2] < ...), so MFP never reaches a fixpoint. The
  ;; per-block re-visit cap must turn that silent non-termination into a
  ;; remedy-pointing error instead of spinning. (Runs to the 100000-revisit
  ;; cap before raising — a few seconds.)
  (let* ((loop-cfg '((0 () (1))
                     (1 (0 1) (1))))
         (L (interval-lattice))
         (xfer (lambda (blk in)
                 (if (= (car blk) 1)
                     (interval-add in '(1 . 1))
                     in))))
    (test-error (run-analysis 'forward L xfer loop-cfg test-protocol
                              (init-state (abstract-interval 0))))))

(test-group "run-analysis — duplicate (widen ...) rejected"
  (let* ((L (interval-lattice))
         (xfer (lambda (blk in) in)))
    (test-error (run-analysis 'forward L xfer linear-cfg test-protocol
                              (widen interval-widen) (widen interval-widen)))))

(test-group "run-analysis — (widen OP) requires a procedure"
  (let* ((L (interval-lattice))
         (xfer (lambda (blk in) in)))
    (test-error (run-analysis 'forward L xfer linear-cfg test-protocol (widen #f)))
    (test-error (run-analysis 'forward L xfer linear-cfg test-protocol (widen 42)))))

;;; --- No-widen regression: finite-height behavior unchanged -------------

(test-group "run-analysis — widening absent leaves MFP unchanged"
  ;; A finite-height (truth) analysis run *without* (widen ...) is pure MFP.
  ;; Supplying a widen operator on a CFG with a loop header must still
  ;; converge to the same fixpoint (widening an idempotent-join lattice at
  ;; the header cannot raise the result above top).
  (let* ((cyclic-cfg '((0 (1) (1)) (1 (0) (0))))
         (L (truth-value-lattice))
         (xfer (lambda (blk in) (or in #t)))
         (plain  (run-analysis 'forward L xfer cyclic-cfg test-protocol
                                (init-state #t)))
         (widened (run-analysis 'forward L xfer cyclic-cfg test-protocol
                                (init-state #t)
                                (widen (lambda (cur next) (or cur next))))))
    (test (analysis-out plain 0) (analysis-out widened 0))
    (test (analysis-out plain 1) (analysis-out widened 1))
    (test #t (analysis-out widened 0))))

;;; --- Widening at a non-self-loop header (header != latch) ---------------

(test-group "run-analysis — widening at a separate-latch loop header"
  ;; 0 -> 1 (header) -> 2 (latch, x:=x+1) -> back to 1. The back-edge is
  ;; 2 -> 1, so the widening point is block 1, NOT a self-loop. Exercises the
  ;; rank-based back-edge detection on the canonical loop shape.
  (let* ((loop-cfg '((0 ()    (1))
                     (1 (0 2)  (2))
                     (2 (1)    (1))))
         (L (interval-lattice))
         (xfer (lambda (blk in)
                 (if (= (car blk) 2)
                     (interval-add in '(1 . 1))   ; latch increments
                     in)))
         (result (run-analysis 'forward L xfer loop-cfg test-protocol
                                (init-state (abstract-interval 0))
                                (widen interval-widen))))
    (test '(0 . pos-inf) (analysis-in result 1))))

;;; --- Decrement loop: lower bound widens to neg-inf ---------------------

(test-group "run-analysis — decrement loop widens lower bound to neg-inf"
  (let* ((loop-cfg '((0 () (1))
                     (1 (0 1) (1))))
         (L (interval-lattice))
         (xfer (lambda (blk in)
                 (if (= (car blk) 1)
                     (interval-sub in '(1 . 1))   ; x := x - 1
                     in)))
         (result (run-analysis 'forward L xfer loop-cfg test-protocol
                                (init-state (abstract-interval 0))
                                (widen interval-widen))))
    (test '(neg-inf . 0) (analysis-in result 1))))

;;; --- Acyclic CFG: a supplied widen operator fires nowhere --------------

(test-group "run-analysis — widen on acyclic CFG matches pure MFP"
  ;; A diamond has no back-edges, so no widening points: supplying a widen
  ;; operator must produce byte-identical results to pure MFP. The widen op
  ;; below would corrupt the result if it ever fired (returns top).
  (let* ((L (interval-lattice))
         (xfer (lambda (blk in) (interval-add in '(1 . 1))))
         (plain   (run-analysis 'forward L xfer diamond-cfg test-protocol
                                 (init-state (abstract-interval 0))))
         (widened (run-analysis 'forward L xfer diamond-cfg test-protocol
                                 (init-state (abstract-interval 0))
                                 (widen (lambda (cur next) (cons 'neg-inf 'pos-inf))))))
    (test (analysis-out plain 3) (analysis-out widened 3))))

;;; --- Regression: widening tolerates unreachable-from-entry preds --------

(test-group "run-analysis — widening with an unreachable predecessor (regression)"
  ;; Block 1's preds include 9, which is unreachable from entry 0 (not in the
  ;; RPO rank-map). Pure MFP tolerates this; the widening detector must too
  ;; (it previously crashed calling rank-of on the orphan index).
  (let* ((cfg '((0 () (1))
                (1 (0 9) ())
                (9 () (1))))
         (L (interval-lattice))
         (xfer (lambda (blk in) in)))
    (test '(0 . 0)
          (analysis-in
            (run-analysis 'forward L xfer cfg test-protocol
                          (init-state (abstract-interval 0))
                          (widen interval-widen))
            1))))

;;; --- Regression: a first visit that generates a fact propagates ---------

;; run-analysis skipped the successor enqueue whenever out <= old-out, and a
;; block's FIRST visit compares against its own bottom seed. A transfer that
;; generates a fact from bottom input (out = #t from in = #f) therefore
;; propagated only if the generating block was not visited first, with the
;; worklist emptying after one visit otherwise.
(define (first-visit-outs direction cfg gen-idx)
  (let* ((calls 0)
         (xfer (lambda (blk in)
                 (set! calls (+ calls 1))
                 (if (= (car blk) gen-idx) #t in)))
         (r (run-analysis direction (truth-value-lattice) xfer cfg test-protocol)))
    (list (map (lambda (i) (analysis-out r i)) (map car cfg)) calls)))

(test-group "run-analysis — first visit generating a fact propagates (I176)"
  ;; Generator mid-chain: block 0 is visited first and yields bottom, so
  ;; block 1's generated fact must still reach block 2. Was (#f #f #f), 1 call.
  (test '((#f #t #t) 3) (first-visit-outs 'forward three-block-cfg 1))
  ;; Generator at the end of the chain.
  (test '((#f #f #t) 3) (first-visit-outs 'forward three-block-cfg 2))
  ;; Backward mirror: the exit block is visited first.
  (test '((#t #t #f) 3) (first-visit-outs 'backward three-block-cfg 1))
  ;; Diamond: the generated fact reaches the join through both arms.
  (test '((#f #t #f #t) 4) (first-visit-outs 'forward diamond-cfg 1))
  ;; CONTROL (not a gate): the generator is the entry block, whose first visit
  ;; already differs from bottom, so master answered this correctly too.
  (test '((#t #t #t) 3) (first-visit-outs 'forward three-block-cfg 0))
  ;; Nothing generated: every reachable block is now visited once (was one
  ;; visit, the entry), and the states are unchanged.
  (test '((#f #f #f) 3) (first-visit-outs 'forward three-block-cfg 99)))

;; Loops: states only here; one cycle's call count is pinned further down.
(test-group "run-analysis — first visit generating a fact, cyclic CFG (I176)"
  (test '(#f #t #t #t) (car (first-visit-outs 'forward loop-exit-cfg 2)))
  (test '(#f #f #f #f) (car (first-visit-outs 'forward loop-exit-cfg 99))))

;; A lattice whose bottom is not #f: a guard that tested the out-state for
;; truthiness instead of comparing it with the lattice would pass the rows
;; above and fail here.
(test-group "run-analysis — first visit generating a fact, non-boolean bottom (I176)"
  (let* ((rank '((lo . 0) (hi . 1)))
         (rk (lambda (x) (cdr (assq x rank))))
         (L (make-lattice
              (lambda (a b) (if (>= (rk a) (rk b)) a b))
              (lambda (a b) (if (<= (rk a) (rk b)) a b))
              'lo 'hi
              (lambda (a b) (<= (rk a) (rk b)))))
         (calls 0)
         (xfer (lambda (blk in)
                 (set! calls (+ calls 1))
                 (if (= (car blk) 1) 'hi in)))
         (r (run-analysis 'forward L xfer three-block-cfg test-protocol)))
    (test '(lo hi hi) (map (lambda (i) (analysis-out r i)) '(0 1 2)))
    (test 3 calls)))

;; Backward: the first visit now enqueues flow-succs unconditionally, so a
;; predecessor the RPO never reaches in this direction must not crash the
;; worklist ordering. Block 9 is forward-unreachable.
(test-group "run-analysis — backward with a forward-unreachable predecessor (I176)"
  (let ((cfg '((0 () (1))
               (1 (0 9) ())
               (9 () (1)))))
    (test '(#f #f #f)
          (map (lambda (i)
                 (analysis-out
                   (run-analysis 'backward (truth-value-lattice)
                                 (lambda (blk in) in) cfg test-protocol)
                   i))
               '(0 1 9)))))

;; An exit block outside the entry's RPO is seeded alongside the real exit;
;; the second insert used to reach rank-of with the orphan (master too).
(test-group "run-analysis — backward with a forward-unreachable exit (I176)"
  (let ((cfg '((0 () (1))
               (1 (0) ())
               (9 () ()))))
    (test '(#f #f #f)
          (map (lambda (i)
                 (analysis-out
                   (run-analysis 'backward (truth-value-lattice)
                                 (lambda (blk in) in) cfg test-protocol)
                   i))
               '(0 1 9)))))

;;; --- Backward seeding: every block, and every cycle widened ---------------

;; Backward used to start the worklist from the exit blocks only, so a block
;; with no path to an exit was never transferred and reported bottom: dead, to
;; a liveness analysis. The transfer below is always #t and counts its calls.
(define (always-outs direction cfg)
  (let* ((calls 0)
         (r (run-analysis direction (truth-value-lattice)
                          (lambda (blk in) (set! calls (+ calls 1)) #t)
                          cfg test-protocol)))
    (list (map (lambda (i) (analysis-out r i)) (map car cfg)) calls)))

;; Block 1 loops on itself and never reaches exit 2.
(define no-exit-path-cfg '((0 () (1 2)) (1 (0 1) (1)) (2 (0) ())))

(test-group "run-analysis — backward visits a block with no path to an exit"
  ;; Was ((#t #f #t) 2): block 1 never visited.
  (test '((#t #t #t) 4) (always-outs 'backward no-exit-path-cfg))
  ;; A CFG with no exit at all (a Go function ending in `for {}`). Was ((#f #f) 0).
  (test '((#t #t) 3) (always-outs 'backward '((0 () (1)) (1 (0 1) (1)))))
  ;; CONTROL: forward seeding is unchanged.
  (test '((#t #t #t) 4) (always-outs 'forward no-exit-path-cfg)))

(define (interval-incr blk in)
  (interval-add in '(1 . 1)))

(test-group "run-analysis — a forward-unreachable cycle run backward is widened"
  ;; Block 9 loops on itself and is unreachable from entry 0. It used to get no
  ;; rank, so it was never a widening point and the run hit the revisit cap
  ;; although (widen ...) was supplied.
  (let ((cfg '((0 () (1)) (1 (0 9) ()) (9 (9) (1 9)))))
    (test '((0 (1 . 1) (2 . 2)) (1 (0 . 0) (1 . 1)) (9 (1 . pos-inf) (2 . pos-inf)))
          (run-analysis 'backward (interval-lattice) interval-incr cfg test-protocol
                        (init-state '(0 . 0)) (widen interval-widen)))
    ;; CONTROL: forward never enqueues the orphan.
    (test '((1 . 1) (2 . 2) interval-bot)
          (let ((r (run-analysis 'forward (interval-lattice) interval-incr cfg
                                 test-protocol
                                 (init-state '(0 . 0)) (widen interval-widen))))
            (map (lambda (i) (analysis-out r i)) '(0 1 9)))))
  ;; An orphan self-loop 8 feeding an orphan exit 9.
  (test '((0 (0 . 0) (1 . 1)) (8 (1 . pos-inf) (2 . pos-inf)) (9 (0 . 0) (1 . 1)))
        (run-analysis 'backward (interval-lattice) interval-incr
                      '((0 () ()) (8 (8) (8 9)) (9 (8) ())) test-protocol
                      (init-state '(0 . 0)) (widen interval-widen))))

;; Every block here reaches an exit, so the seeds are the exit blocks alone.
;; Seeding every block let loop 3-6 run ahead of the exit data and widen on
;; its own value: block 1's out came back (neg-inf . 7) instead of (-4 . 7).
(test-group "run-analysis — backward seeds only exits when every block reaches one"
  (let* ((cfg '((0 (0 5) (7 0 1)) (1 (0) (4 5 2)) (2 (1 4) ()) (3 (3 6) (3 6))
                (4 (1 6) (5 2 6)) (5 (1 4) (6 0)) (6 (3 4 5) (3 4)) (7 (0) ())))
         (ks '((1 . -2) (0 . 3) (2 . -3) (3 . 2) (0 . -2) (3 . 1) (1 . -2) (2 . 1)))
         (L (interval-lattice))
         (xfer (lambda (blk in)
                 (let ((k (list-ref ks (car blk))))
                   (if (and (eq? in 'interval-bot) (< (car k) 3))
                       in
                       (case (car k)
                         ((3) (lattice-join L
                                (if (eq? in 'interval-bot)
                                    'interval-bot
                                    (interval-add in '(1 . 1)))
                                (cons (cdr k) (cdr k))))
                         ((0) (interval-add in (cons (cdr k) (cdr k))))
                         ((1) (lattice-meet L in (cons -5 (+ 5 (cdr k)))))
                         (else (lattice-join L in (cons (cdr k) (cdr k)))))))))
         (r (run-analysis 'backward L xfer cfg test-protocol
                          (init-state '(0 . 0)) (widen interval-widen))))
    (test '((-4 . 3) (-4 . 7) (-3 . 0) (-4 . pos-inf) (-7 . 2) (-4 . 4) (-5 . 3) (0 . 1))
          (map (lambda (i) (analysis-out r i)) '(0 1 2 3 4 5 6 7)))))

;; Orphans rank AHEAD of the reachable blocks in forward order. Ranked after
;; them, orphan 9 would make block 1 a forward widening point (its predecessor
;; 9 would sit later in flow order) and be one itself backward. The widen
;; operator here returns top, so firing anywhere changes the answer.
(test-group "run-analysis — an orphan predecessor makes no widening point"
  (let ((cfg '((0 () (1)) (1 (0 9) ()) (9 () (1))))
        (top-widen (lambda (cur next) (cons 'neg-inf 'pos-inf))))
    (for-each
      (lambda (direction)
        (test (run-analysis direction (interval-lattice) interval-incr cfg
                            test-protocol (init-state '(0 . 0)))
              (run-analysis direction (interval-lattice) interval-incr cfg
                            test-protocol (init-state '(0 . 0)) (widen top-widen))))
      '(forward backward))))

;; A revisit whose out-state did not rise must not propagate. The worklist
;; order is deterministic, so the count pins it: 4 first visits plus block 1's
;; revisit from block 2. A guard that let a second visit through as well
;; ((> vc 2) for (> vc 1)) gives 8.
(test-group "run-analysis — a non-rising revisit does not propagate (call count)"
  (test '((#f #f #f #f) 5) (first-visit-outs 'forward loop-exit-cfg 99)))

;;; --- Revisit cap names every cause ---------------------------------------

(define (string-has? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let scan ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) #t)
            (else (scan (+ i 1)))))))

;; A flat lattice {bot, a, b, top} and a transfer that flips a <-> b on a self
;; loop: non-monotone, never converges, and widening cannot help. The cap
;; message used to blame only infinite height and suggest (widen OP).
(test-group "run-analysis — revisit cap names a non-monotone transfer"
  (let* ((flat (make-lattice
                 (lambda (x y)
                   (cond ((eq? x 'bot) y) ((eq? y 'bot) x) ((eq? x y) x) (else 'top)))
                 (lambda (x y)
                   (cond ((eq? x 'top) y) ((eq? y 'top) x) ((eq? x y) x) (else 'bot)))
                 'bot 'top
                 (lambda (x y) (or (eq? x 'bot) (eq? y 'top) (eq? x y)))))
         (flip (lambda (blk in) (if (eq? in 'a) 'b 'a)))
         (cfg '((0 (0) (0)))))
    (test #t
          (guard (e ((error-object? e)
                     (string-has? (error-object-message e) "check-monotone")))
            (run-analysis 'forward flat flip cfg test-protocol)
            'no-error))
    ;; CONTROL: 'check-monotone locates it.
    (test-error (run-analysis 'forward flat flip cfg test-protocol 'check-monotone))))

(test-end)
(test-exit)
