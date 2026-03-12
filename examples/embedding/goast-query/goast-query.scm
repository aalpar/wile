;;; goast-query.scm - Parse and query Go source as s-expressions
;;;
;;; Three queries show progressively richer analysis:
;;;
;;;   1. Parse a source string and extract all function names.
;;;   2. Separate exported from unexported functions.
;;;   3. Type-check a real package and find functions that return error.
;;;
;;; Usage: ./dist/wile -f examples/embedding/goast-query/goast-query.scm

;; Nodes are tagged alists: (tag (key . val) ...).
;; node-field extracts a field value by key.
(define (node-field node key)
  (let ((e (assoc key (cdr node))))
    (if e (cdr e) #f)))

;; Collect non-#f results of applying f to each element.
(define (collect-matching lst f)
  (let loop ((xs lst) (acc '()))
    (if (null? xs)
      (reverse acc)
      (let ((v (f (car xs))))
        (loop (cdr xs) (if v (cons v acc) acc))))))

;; ---------------------------------------------------------------------------
;; 1. Parse a source string; collect all function names.
;; ---------------------------------------------------------------------------
(display "--- 1. Function names from source string ---")
(newline)

(define file
  (go-parse-string
    "package demo
func Add(a, b int) int { return a + b }
func Sub(a, b int) int { return a - b }
func helper() {}
func Mul(a, b int) int { return a * b }"))

(define all-names
  (collect-matching
    (node-field file 'decls)
    (lambda (decl)
      (and (eq? (car decl) 'func-decl)
           (node-field decl 'name)))))

(display "  All:        ")
(display all-names)
(newline)
(newline)

;; ---------------------------------------------------------------------------
;; 2. Separate exported from unexported.
;; Exported Go identifiers start with an ASCII uppercase letter.
;; ---------------------------------------------------------------------------
(display "--- 2. Exported vs unexported ---")
(newline)

(define (exported? name)
  (let ((c (string-ref name 0)))
    (and (char>=? c #\A) (char<=? c #\Z))))

(define exported
  (collect-matching all-names (lambda (n) (and (exported? n) n))))

(define unexported
  (collect-matching all-names (lambda (n) (and (not (exported? n)) n))))

(display "  Exported:   ")
(display exported)
(newline)
(display "  Unexported: ")
(display unexported)
(newline)
(newline)

;; ---------------------------------------------------------------------------
;; 3. Type-check a real package; find functions that return error.
;;
;; go-typecheck-package runs `go list` under the hood, so the package
;; must be on-disk and in a module the current module can resolve.
;; ---------------------------------------------------------------------------
(display "--- 3. Functions returning error (type-checked package) ---")
(newline)

(define (result-is-error? field)
  (let ((t (node-field field 'type)))
    (and (pair? t)
         (eq? (car t) 'ident)
         (equal? (node-field t 'name) "error"))))

(define (returns-error? decl)
  (let ((results (node-field (node-field decl 'type) 'results)))
    (and (pair? results)
         (let loop ((fields results))
           (cond
             ((null? fields) #f)
             ((result-is-error? (car fields)) #t)
             (else (loop (cdr fields))))))))

(define (package-error-funcs pkg)
  (apply append
    (map (lambda (file)
           (collect-matching
             (node-field file 'decls)
             (lambda (decl)
               (and (eq? (car decl) 'func-decl)
                    (returns-error? decl)
                    (node-field decl 'name)))))
         (node-field pkg 'files))))

(define pkg
  (car (go-typecheck-package "github.com/aalpar/wile/extensions/goast")))

(display "  Error-returning: ")
(display (package-error-funcs pkg))
(newline)
