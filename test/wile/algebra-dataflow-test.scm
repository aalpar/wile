;;; algebra-dataflow-test.scm — (wile algebra dataflow)

(import (scheme base)
        (chibi test)
        (wile algebra lattice)
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

(test-end)
(test-exit)
