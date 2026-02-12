;;; r6rs-compat.scm - R6RS compatibility shim for R7RS Wile
;;;
;;; Provides R6RS-compatible signatures for procedures that differ between
;;; R6RS and R7RS. Load this before running R6RS-era code.
;;;
;;; Usage: (load "examples/lib/r6rs-compat.scm")

;; Save original R7RS procedures
(define r7rs:error error)

;; R6RS error: (error who message irritant ...)
;; R7RS error: (error message irritant ...)
;;
;; This version accepts both forms:
;; - If first arg is a string, assume R7RS form and pass through
;; - Otherwise, assume R6RS form with who parameter
(define (error . args)
  (cond
   ((null? args)
    (r7rs:error "unspecified error"))

   ((string? (car args))
    ;; R7RS form: (error message irritants...)
    (apply r7rs:error args))

   (else
    ;; R6RS form: (error who message irritants...)
    (let ((who (car args))
          (rest (cdr args)))
      (cond
       ((null? rest)
        (r7rs:error "unspecified error"))

       ((not (string? (car rest)))
        (r7rs:error "error: message must be a string"
                    (if who who 'unknown)
                    (car rest)))

       (else
        (let ((message (car rest))
              (irritants (cdr rest)))
          (apply r7rs:error
                 (if who
                     (string-append
                      (cond ((string? who) who)
                            ((symbol? who) (symbol->string who))
                            (else "unknown"))
                      ": "
                      message)
                     message)
                 irritants))))))))

;; Additional R6RS compatibility procedures can be added here as needed.
;; Examples of R6RS/R7RS differences that might need shims:
;;
;; - bytevector procedures (R6RS has different names)
;; - library system (R6RS uses different syntax)
;; - records (R6RS has more complex record system)
;; - I/O system (R6RS has different port procedures)
;;
;; For now, only 'error' is provided as it's the most commonly encountered
;; difference in benchmark suites.
