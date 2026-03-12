;;; state-trace-detect.scm — Split-state pattern detection via goast
;;;
;;; Finds:
;;;   Pass 1: Boolean clusters — structs with >=2 bool fields (enum candidates)
;;;   Pass 2: If-chain field sweeps — cascading conditionals checking
;;;           multiple fields of the same receiver (scalar relation candidates)
;;;
;;; Usage: wile -f examples/embedding/goast-query/state-trace-detect.scm

;; ── Target ───────────────────────────────────────────────
;; Any go-list pattern. "." = current package, "./..." = recursive.
(define target "github.com/aalpar/wile/machine")

;; ── AST utilities ────────────────────────────────────────

(define (nf node key)
  (let ((e (assoc key (cdr node))))
    (if e (cdr e) #f)))

(define (tag? node t)
  (and (pair? node) (eq? (car node) t)))

(define (filter-map f lst)
  (let loop ((xs lst) (acc '()))
    (if (null? xs) (reverse acc)
      (let ((v (f (car xs))))
        (loop (cdr xs) (if v (cons v acc) acc))))))

(define (flat-map f lst)
  (apply append (map f lst)))

;; Depth-first walk over goast s-expressions.
;; Calls (visitor node) on each tagged-alist node.
;; Collects non-#f results into a flat list.
;;
;; Distinguishes three shapes:
;;   (symbol (k . v) ...)  -> tagged node: visit, then recurse into field values
;;   ((node1) (node2) ...) -> list of nodes: recurse into each
;;   atom / ()             -> leaf: skip
(define (walk val visitor)
  (cond
    ((not (pair? val)) '())
    ((symbol? (car val))
     (let ((here (visitor val))
           (children (flat-map
                       (lambda (kv)
                         (if (pair? kv) (walk (cdr kv) visitor) '()))
                       (cdr val))))
       (if here (cons here children) children)))
    ((pair? (car val))
     (flat-map (lambda (child) (walk child visitor)) val))
    (else '())))

;; ── Pass 1: Boolean Clusters ─────────────────────────────

(define (bool-field-names field-node)
  ;; If this struct field has type bool, return its name(s); else '()
  (if (and (tag? field-node 'field)
           (let ((t (nf field-node 'type)))
             (and (tag? t 'ident) (equal? (nf t 'name) "bool"))))
    (let ((ns (nf field-node 'names)))
      (if (pair? ns) ns '()))
    '()))

(define (find-bool-clusters file-ast)
  ;; Walk file, find type-spec with struct-type containing >=2 bool fields
  (walk file-ast
    (lambda (node)
      (and (tag? node 'type-spec)
           (let ((stype (nf node 'type)))
             (and (tag? stype 'struct-type)
                  (let* ((fields (nf stype 'fields))
                         (bools (flat-map bool-field-names
                                  (if (pair? fields) fields '()))))
                    (and (>= (length bools) 2)
                         (list (nf node 'name) bools)))))))))

;; ── Pass 2: If-Chain Field Sweeps ────────────────────────

(define (if-chain-conditions node)
  ;; Follow the else-if spine, collecting each condition
  (if (not (tag? node 'if-stmt)) '()
    (cons (nf node 'cond)
          (let ((el (nf node 'else)))
            (if (and el (tag? el 'if-stmt))
              (if-chain-conditions el)
              '())))))

(define (selectors-in expr)
  ;; Extract all (receiver . field) pairs from selector-exprs
  (walk expr
    (lambda (node)
      (and (tag? node 'selector-expr)
           (let ((x (nf node 'x)))
             (and (tag? x 'ident)
                  (cons (nf x 'name) (nf node 'sel))))))))

(define (unique lst)
  (let loop ((xs lst) (seen '()))
    (cond ((null? xs) (reverse seen))
          ((member (car xs) seen) (loop (cdr xs) seen))
          (else (loop (cdr xs) (cons (car xs) seen))))))

(define (find-field-sweep-chains file-ast)
  ;; Find if-chains where >=2 conditions check fields of the same receiver
  (walk file-ast
    (lambda (node)
      (and (tag? node 'if-stmt)
           (let ((el (nf node 'else)))
             (and el (tag? el 'if-stmt)))
           (let* ((conds (if-chain-conditions node))
                  (all-sels (flat-map selectors-in conds))
                  (receivers (unique (map car all-sels)))
                  (grouped
                    (filter-map
                      (lambda (recv)
                        (let ((fields (filter-map
                                        (lambda (sel)
                                          (and (equal? (car sel) recv) (cdr sel)))
                                        all-sels)))
                          (and (>= (length fields) 2)
                               (list recv fields (length conds)))))
                      receivers)))
             (and (pair? grouped) grouped))))))

;; ── Main ─────────────────────────────────────────────────

(display "Loading and type-checking ") (display target) (display " ...")
(newline)
(define pkgs (go-typecheck-package target))
(newline)

(display "== State-Trace: Split State Detection ==")
(newline) (newline)

;; Pass 1
(display "-- Pass 1: Boolean Clusters --") (newline)
(define cluster-count 0)
(for-each
  (lambda (pkg)
    (for-each
      (lambda (file)
        (for-each
          (lambda (c)
            (set! cluster-count (+ cluster-count 1))
            (display "  struct ") (display (car c))
            (display ": bool fields ") (display (cadr c))
            (newline))
          (find-bool-clusters file)))
      (nf pkg 'files)))
  pkgs)
(if (= cluster-count 0) (begin (display "  (none found)") (newline)))
(newline)

;; Pass 2
(display "-- Pass 2: If-Chain Field Sweeps --") (newline)
(define sweep-count 0)
(for-each
  (lambda (pkg)
    (for-each
      (lambda (file)
        (for-each
          (lambda (sweep)
            (for-each
              (lambda (entry)
                (set! sweep-count (+ sweep-count 1))
                (display "  receiver ") (display (car entry))
                (display ": fields ") (display (cadr entry))
                (display " across ") (display (caddr entry))
                (display "-branch chain") (newline))
              sweep))
          (find-field-sweep-chains file)))
      (nf pkg 'files)))
  pkgs)
(if (= sweep-count 0) (begin (display "  (none found)") (newline)))
(newline)

;; Summary
(display "-- Summary --") (newline)
(display "  Boolean clusters: ") (display cluster-count) (newline)
(display "  Field sweep chains: ") (display sweep-count) (newline)
