;; Bootstrap macros whose templates reference bootstrap PROCEDURES (not Go
;; primitives): unless -> not, guard -> with-exception-handler. These load
;; AFTER bootstrap_procedures.scm so their free identifiers pin to the sealed
;; base at macro-definition time (like case -> memv, a Go primitive), instead
;; of degrading to a nil pin that a use-site redefinition can capture
;; (R7RS 4.3.2 referential transparency).

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test) (begin result1 result2 ...)))))

;; Exception handling (R7RS §4.2.7 guard macro)
;;
;; guard-aux is defined BEFORE guard (D0, 2026-07-22) so that guard's template,
;; which freely references guard-aux, pins that reference to guard-aux's
;; definition-site binding at macro-definition time (SyntaxSymbol.ResolvedBinding
;; non-nil). With the pin populated and consulted on the macro path (D2), a
;; use-site (define-syntax guard-aux …) can no longer capture guard's private
;; helper (R7RS 4.3.2). guard-aux references only itself + begin/let/if, so it
;; compiles standalone here.
(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux re-raise var (else result ...))
     (begin result ...))
    ((guard-aux re-raise var (test => proc) clause ...)
     (let ((t test))
       (if t
           (proc t)
           (guard-aux re-raise var clause ...))))
    ((guard-aux re-raise var (test result ...) clause ...)
     (if test
         (begin result ...)
         (guard-aux re-raise var clause ...)))
    ((guard-aux re-raise var)
     (re-raise))))

;; Uses the double call/cc pattern from R7RS §7.3 so that when no clause
;; matches, the exception is re-raised in the dynamic extent of the
;; original raise (where the previous exception handler is current).
;;
;; - guard-k: escapes to the guard form's return site
;; - handler-k: captures the handler's dynamic extent for re-raise
;; - Both paths wrap their result in a thunk; the outer ((call/cc ...))
;;   calls whichever thunk wins
(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     ;; The OUTER escape uses call-with-exit (an escape continuation), not call/cc,
     ;; so the handler's outward escape to the guard form propagates through a
     ;; with-continuation-barrier: exceptions/escapes are pass-through, full call/cc
     ;; jumps are blocked (Racket + prim_barrier.go). guard-k is invoked once,
     ;; outward, during the body's dynamic extent — exact call-with-exit usage.
     ;; handler-k (inner, re-raise path) stays call/cc.
     ((call-with-exit
        (lambda (guard-k)
          (with-exception-handler
           (lambda (condition)
             ((call/cc
                (lambda (handler-k)
                  (guard-k
                   (lambda ()
                     (let ((var condition))
                       (guard-aux
                        (lambda ()
                          (handler-k
                           (lambda ()
                             (raise-continuable condition))))
                        var clause ...))))))))
           (lambda ()
             (let ((results (call-with-values (lambda () e1 e2 ...) list)))
               (lambda () (apply values results)))))))))))
