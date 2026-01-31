;;; rule-engine.scm - Forward-chaining rule engine
;;;
;;; Demonstrates: symbols-as-data, pattern matching with logic variables,
;;;               fixed-point iteration, define-record-type, closures,
;;;               the kind of system you'd embed Scheme to build
;;;
;;; Usage: ./dist/scheme --file examples/applications/rule-engine.scm

;; -----------------------------------------------------------------------
;; Logic variables (symbols starting with ?)
;; -----------------------------------------------------------------------

(define (variable? x)
  (and (symbol? x)
       (let ((s (symbol->string x)))
         (and (> (string-length s) 1)
              (char=? (string-ref s 0) #\?)))))

;; -----------------------------------------------------------------------
;; Pattern matching against facts
;;
;; Returns extended bindings on success, #f on failure.
;; Bindings is an alist of (?var . value) pairs.
;; -----------------------------------------------------------------------

(define (match-pattern pattern fact bindings)
  (cond
    ((not bindings) #f)
    ((variable? pattern)
     (let ((b (assq pattern bindings)))
       (if b
           (if (equal? (cdr b) fact) bindings #f)
           (cons (cons pattern fact) bindings))))
    ((and (pair? pattern) (pair? fact))
     (match-pattern (cdr pattern) (cdr fact)
       (match-pattern (car pattern) (car fact) bindings)))
    ((equal? pattern fact) bindings)
    (else #f)))

;; Match all conditions of a rule against the fact base.
;; Returns a list of all valid binding sets.
(define (match-conditions conditions facts bindings)
  (if (null? conditions)
      (list bindings)
      (let ((pattern (car conditions))
            (rest (cdr conditions)))
        (apply append
          (map (lambda (fact)
                 (let ((new-bindings (match-pattern pattern fact bindings)))
                   (if new-bindings
                       (match-conditions rest facts new-bindings)
                       '())))
               facts)))))

;; -----------------------------------------------------------------------
;; Substitution — instantiate a template with bindings
;; -----------------------------------------------------------------------

(define (instantiate template bindings)
  (cond
    ((variable? template)
     (let ((b (assq template bindings)))
       (if b (cdr b) template)))
    ((pair? template)
     (cons (instantiate (car template) bindings)
           (instantiate (cdr template) bindings)))
    (else template)))

;; -----------------------------------------------------------------------
;; Rules
;; -----------------------------------------------------------------------

(define-record-type <rule>
  (make-rule name conditions consequents)
  rule?
  (name rule-name)
  (conditions rule-conditions)
  (consequents rule-consequents))

;; -----------------------------------------------------------------------
;; Engine — forward chaining to fixed point
;; -----------------------------------------------------------------------

(define (fire-rules rules facts)
  (let loop ((facts facts))
    (let ((new-facts
           (apply append
             (map (lambda (rule)
                    (let ((all-bindings
                           (match-conditions (rule-conditions rule)
                                             facts '())))
                      (apply append
                        (map (lambda (bindings)
                               (let ((derived
                                      (map (lambda (t)
                                             (instantiate t bindings))
                                           (rule-consequents rule))))
                                 ;; Only keep truly new facts
                                 (let keep ((ds derived) (new '()))
                                   (if (null? ds) new
                                       (keep (cdr ds)
                                             (if (member (car ds) facts)
                                                 new
                                                 (if (member (car ds) new)
                                                     new
                                                     (cons (car ds) new))))))))
                             all-bindings))))
                  rules))))
      (if (null? new-facts)
          facts
          (begin
            (for-each (lambda (f)
                        (display "  [inferred] ")
                        (display f)
                        (newline))
                      new-facts)
            (loop (append facts new-facts)))))))

;; -----------------------------------------------------------------------
;; Demo 1: Medical diagnostic system
;; -----------------------------------------------------------------------

(display "=== Forward-Chaining Rule Engine ===\n\n")

(display "--- Medical diagnosis ---\n")

(let ((rules
       (list
        (make-rule 'fever-check
          '((symptom ?patient fever) (symptom ?patient cough))
          '((suspect ?patient flu)))
        (make-rule 'flu-treatment
          '((suspect ?patient flu))
          '((recommend ?patient rest)
            (recommend ?patient fluids)))
        (make-rule 'allergy-check
          '((symptom ?patient sneezing) (symptom ?patient itchy-eyes))
          '((suspect ?patient allergy)))
        (make-rule 'allergy-treatment
          '((suspect ?patient allergy))
          '((recommend ?patient antihistamine)))))
      (facts
       '((symptom alice fever)
         (symptom alice cough)
         (symptom bob sneezing)
         (symptom bob itchy-eyes))))

  (display "  Initial facts:\n")
  (for-each (lambda (f) (display "    ") (display f) (newline)) facts)
  (display "  Running rules...\n")
  (let ((result (fire-rules rules facts)))
    (display "  Final fact count: ")
    (display (length result))
    (newline)))

;; -----------------------------------------------------------------------
;; Demo 2: Access control policy
;; -----------------------------------------------------------------------

(display "\n--- Access control policy ---\n")

(let ((rules
       (list
        (make-rule 'admin-access
          '((role ?user admin) (resource ?res confidential))
          '((access ?user ?res granted)))
        (make-rule 'viewer-read
          '((role ?user viewer) (resource ?res public))
          '((access ?user ?res granted)))
        (make-rule 'viewer-deny
          '((role ?user viewer) (resource ?res confidential))
          '((access ?user ?res denied)))
        (make-rule 'audit
          '((access ?user ?res granted))
          '((log audit-entry ?user accessed ?res)))))
      (facts
       '((role alice admin)
         (role bob viewer)
         (resource secret-doc confidential)
         (resource readme public))))

  (display "  Initial facts:\n")
  (for-each (lambda (f) (display "    ") (display f) (newline)) facts)
  (display "  Running rules...\n")
  (let ((result (fire-rules rules facts)))
    (display "  Final fact count: ")
    (display (length result))
    (newline)))

;; -----------------------------------------------------------------------
;; Demo 3: Family relationships
;; -----------------------------------------------------------------------

(display "\n--- Family relationship inference ---\n")

(let ((rules
       (list
        (make-rule 'grandparent
          '((parent ?gp ?p) (parent ?p ?c))
          '((grandparent ?gp ?c)))
        (make-rule 'sibling
          '((parent ?p ?a) (parent ?p ?b))
          '((sibling ?a ?b)))))
      (facts
       '((parent tom bob)
         (parent tom alice)
         (parent bob carol)
         (parent bob dave)
         (parent alice eve))))

  (display "  Initial facts:\n")
  (for-each (lambda (f) (display "    ") (display f) (newline)) facts)
  (display "  Running rules...\n")
  (let ((result (fire-rules rules facts)))
    ;; Show just the inferred relationships
    (display "  Grandparents and siblings:\n")
    (for-each (lambda (f)
                (if (or (eq? (car f) 'grandparent)
                        (eq? (car f) 'sibling))
                    (begin (display "    ") (display f) (newline))))
              result)))

(display "\nBusiness rules as Scheme data — hot-reloadable, composable.\n")
