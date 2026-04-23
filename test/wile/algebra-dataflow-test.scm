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

(test-group "cfg-protocol — accessors"
  (test 0 ((cfg-protocol-index-of test-protocol) '(0 () (1 2))))
  (test '() ((cfg-protocol-preds-of test-protocol) '(0 () (1 2))))
  (test '(1 2) ((cfg-protocol-succs-of test-protocol) '(0 () (1 2))))
  (test diamond-cfg ((cfg-protocol-blocks-of test-protocol) diamond-cfg)))

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
         (result (run-analysis 'forward L xfer diamond-cfg test-protocol #t)))
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
         (result (run-analysis 'forward L xfer linear-cfg test-protocol #t)))
    (test #t (analysis-in result 0))
    (test #t (analysis-out result 0))
    (test #t (analysis-in result 3))
    (test #t (analysis-out result 3))))

;;; --- Single-block CFG ---------------------------------------------------

(test-group "run-analysis — single block"
  (let* ((L (truth-value-lattice))
         (xfer (lambda (blk in) in))
         (result (run-analysis 'forward L xfer single-block-cfg test-protocol #t)))
    (test 1 (length result))
    (test #t (analysis-in result 0))
    (test #t (analysis-out result 0))))

(test-end)
(test-exit)
