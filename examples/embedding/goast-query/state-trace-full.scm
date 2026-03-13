;;; state-trace-full.scm — Cross-layer split-state detection
;;;
;;; Uses four goast* layers:
;;;   Pass 1 (AST):  Boolean clusters — structs with >=2 bool fields
;;;   Pass 2 (AST):  If-chain field sweeps — cascading checks on same receiver
;;;   Pass 3 (SSA):  Mutation independence — are clustered fields stored independently?
;;;   Pass 4 (CFG):  Check ordering — do field reads follow a fixed dominance order?
;;;
;;; Usage: ./dist/wile -f examples/embedding/goast-query/state-trace-full.scm

;; ── Target ───────────────────────────────────────────────
(define target "github.com/aalpar/wile/machine")

;; ── Shared utilities ─────────────────────────────────────

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

(define (member? x lst)
  (cond ((null? lst) #f)
        ((equal? x (car lst)) #t)
        (else (member? x (cdr lst)))))

(define (unique lst)
  (let loop ((xs lst) (seen '()))
    (cond ((null? xs) (reverse seen))
          ((member? (car xs) seen) (loop (cdr xs) seen))
          (else (loop (cdr xs) (cons (car xs) seen))))))

(define (has-char? s c)
  (let loop ((i 0))
    (cond ((>= i (string-length s)) #f)
          ((char=? (string-ref s i) c) #t)
          (else (loop (+ i 1))))))

;; Generate all ordered pairs from a list (each unordered pair once).
(define (ordered-pairs lst)
  (if (null? lst) '()
    (append
      (map (lambda (b) (list (car lst) b)) (cdr lst))
      (ordered-pairs (cdr lst)))))

;; ══════════════════════════════════════════════════════════
;; Pass 1: Boolean Clusters (AST layer)
;; ══════════════════════════════════════════════════════════

(define (bool-field-names field-node)
  (if (and (tag? field-node 'field)
           (let ((t (nf field-node 'type)))
             (and (tag? t 'ident) (equal? (nf t 'name) "bool"))))
    (let ((ns (nf field-node 'names)))
      (if (pair? ns) ns '()))
    '()))

(define (find-bool-clusters file-ast)
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

;; ══════════════════════════════════════════════════════════
;; Pass 2: If-Chain Field Sweeps (AST layer)
;; ══════════════════════════════════════════════════════════

(define (if-chain-conditions node)
  (if (not (tag? node 'if-stmt)) '()
    (cons (nf node 'cond)
          (let ((el (nf node 'else)))
            (if (and el (tag? el 'if-stmt))
              (if-chain-conditions el)
              '())))))

(define (selectors-in expr)
  (walk expr
    (lambda (node)
      (and (tag? node 'selector-expr)
           (let ((x (nf node 'x)))
             (and (tag? x 'ident)
                  (cons (nf x 'name) (nf node 'sel))))))))

(define (find-field-sweep-chains file-ast)
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

;; ══════════════════════════════════════════════════════════
;; Pass 3: Mutation Independence (SSA layer)
;;
;; For each boolean cluster, scan SSA functions for
;; ssa-field-addr + ssa-store patterns. Report which
;; functions store to which subset of the cluster's fields.
;; If any function stores to field A but not field B,
;; that's evidence of independent mutation.
;; ══════════════════════════════════════════════════════════

;; Walk SSA s-expressions (same walk works — tagged alists).
;; Collect all ssa-field-addr instructions from one function.
(define (collect-field-addrs ssa-func)
  (walk ssa-func
    (lambda (node)
      (and (tag? node 'ssa-field-addr)
           (list (nf node 'name)    ;; SSA register name (e.g. "t3")
                 (nf node 'field)   ;; field name (e.g. "Handled")
                 (nf node 'x))))))  ;; receiver SSA name (e.g. "p")

;; Collect all ssa-store instructions from one function.
;; Returns list of (addr-name val-name).
(define (collect-stores ssa-func)
  (walk ssa-func
    (lambda (node)
      (and (tag? node 'ssa-store)
           (list (nf node 'addr)
                 (nf node 'val))))))

;; For a single SSA function, find which fields of a given struct
;; are stored to. Returns a list of field names that are written.
(define (stored-fields-in-func ssa-func cluster-fields)
  (let* ((field-addrs (collect-field-addrs ssa-func))
         (stores (collect-stores ssa-func))
         ;; Build set of addr register names that are store targets
         (store-addrs (map car stores))
         ;; Find field-addrs whose register is a store target
         ;; AND whose field name is in the cluster
         (stored (filter-map
                   (lambda (fa)
                     (let ((reg (car fa))
                           (field (cadr fa)))
                       (and (member? reg store-addrs)
                            (member? field cluster-fields)
                            field)))
                   field-addrs)))
    (unique stored)))

;; Analyze mutation independence for one cluster across all SSA functions.
;; Returns a list of (func-name . stored-fields) for functions that
;; store to a non-empty proper subset of the cluster's fields.
(define (find-independent-mutations ssa-funcs cluster-name cluster-fields)
  (let ((field-count (length cluster-fields)))
    (filter-map
      (lambda (fn)
        (let* ((fname (nf fn 'name))
               (stored (stored-fields-in-func fn cluster-fields))
               (n (length stored)))
          ;; Interesting if function stores to SOME but not ALL fields
          (and (> n 0)
               (< n field-count)
               (list fname stored))))
      ssa-funcs)))

;; ══════════════════════════════════════════════════════════
;; Main
;; ══════════════════════════════════════════════════════════

(display "Loading and type-checking ") (display target) (display " ...")
(newline)
(define pkgs (go-typecheck-package target))

(display "Building SSA for ") (display target) (display " ...")
(newline)
(define ssa-funcs (go-ssa-build target))
(newline)

(display "══════════════════════════════════════════════════")
(newline)
(display "  State-Trace: Cross-Layer Split State Detection  ")
(newline)
(display "══════════════════════════════════════════════════")
(newline) (newline)

;; ── Pass 1 ───────────────────────────────────────────────
(display "── Pass 1: Boolean Clusters (AST) ──") (newline)
(define all-clusters '())
(for-each
  (lambda (pkg)
    (for-each
      (lambda (file)
        (for-each
          (lambda (c)
            (set! all-clusters (cons c all-clusters))
            (display "  struct ") (display (car c))
            (display ": bool fields ") (display (cadr c))
            (newline))
          (find-bool-clusters file)))
      (nf pkg 'files)))
  pkgs)
(if (null? all-clusters) (begin (display "  (none found)") (newline)))
(newline)

;; ── Pass 2 ───────────────────────────────────────────────
(display "── Pass 2: If-Chain Field Sweeps (AST) ──") (newline)
(define all-sweeps '())
(for-each
  (lambda (pkg)
    (for-each
      (lambda (file)
        (for-each
          (lambda (sweep)
            (for-each
              (lambda (entry)
                (set! all-sweeps (cons entry all-sweeps))
                (display "  receiver ") (display (car entry))
                (display ": fields ") (display (cadr entry))
                (display " across ") (display (caddr entry))
                (display "-branch chain") (newline))
              sweep))
          (find-field-sweep-chains file)))
      (nf pkg 'files)))
  pkgs)
(if (null? all-sweeps) (begin (display "  (none found)") (newline)))
(newline)

;; ── Pass 3 ───────────────────────────────────────────────
(display "── Pass 3: Mutation Independence (SSA) ──") (newline)
(define independence-count 0)
(for-each
  (lambda (cluster)
    (let* ((struct-name (car cluster))
           (fields (cadr cluster))
           (indeps (find-independent-mutations ssa-funcs struct-name fields)))
      (if (pair? indeps)
        (begin
          (display "  struct ") (display struct-name) (display ":") (newline)
          (for-each
            (lambda (entry)
              (set! independence-count (+ independence-count 1))
              (display "    ") (display (car entry))
              (display " stores only: ") (display (cadr entry))
              (newline))
            indeps)))))
  all-clusters)
(if (= independence-count 0) (begin (display "  (none found)") (newline)))
(newline)

;; ══════════════════════════════════════════════════════════
;; Pass 4: Check Ordering via Dominance (SSA + CFG)
;;
;; For each boolean cluster, find functions that access 2+
;; of the cluster's fields. Build the CFG for each such
;; function, extract the dominator tree, and check whether
;; one field's access always dominates the other's.
;;
;; If field A's block dominates field B's block, A is
;; guaranteed to be checked/accessed before B on every
;; execution path — evidence of a fixed priority ordering.
;; ══════════════════════════════════════════════════════════

;; For an SSA function, find which basic blocks access cluster fields.
;; Returns ((field-name . block-index) ...) — one entry per field-addr.
(define (field-access-sites ssa-func cluster-fields)
  (let ((blocks (nf ssa-func 'blocks)))
    (if (not (pair? blocks)) '()
      (flat-map
        (lambda (block)
          (let ((idx (nf block 'index))
                (instrs (nf block 'instrs)))
            (if (not (pair? instrs)) '()
              (filter-map
                (lambda (instr)
                  (and (tag? instr 'ssa-field-addr)
                       (let ((field (nf instr 'field)))
                         (and (member? field cluster-fields)
                              (cons field idx)))))
                instrs))))
        blocks))))

;; Keep only the first access per field (earliest block encountered).
(define (first-per-field sites)
  (let loop ((xs sites) (seen '()) (acc '()))
    (cond
      ((null? xs) (reverse acc))
      ((member? (caar xs) seen) (loop (cdr xs) seen acc))
      (else (loop (cdr xs)
                  (cons (caar xs) seen)
                  (cons (car xs) acc))))))

;; Build CFG + dominator tree and check dominance between field-access blocks.
;; Returns #f on failure, or a list of dominance results:
;;   ((field-a block-a field-b block-b a-dom-b? b-dom-a?) ...)
(define (check-dominance func-name sites)
  (guard (exn (#t #f))
    (let* ((cfg (go-cfg target func-name))
           (dom (go-cfg-dominators cfg))
           (pairs (ordered-pairs sites)))
      (map
        (lambda (pair)
          (let ((fa (car pair))
                (fb (cadr pair)))
            (list (car fa) (cdr fa)
                  (car fb) (cdr fb)
                  (go-cfg-dominates? dom (cdr fa) (cdr fb))
                  (go-cfg-dominates? dom (cdr fb) (cdr fa)))))
        pairs))))

;; Format a single dominance result as a readable string.
(define (display-dominance-result r)
  (let ((field-a (list-ref r 0))
        (block-a (list-ref r 1))
        (field-b (list-ref r 2))
        (block-b (list-ref r 3))
        (a-dom-b (list-ref r 4))
        (b-dom-a (list-ref r 5)))
    (display "      ")
    (display field-a) (display " [block ") (display block-a)
    (display "] -> ") (display field-b)
    (display " [block ") (display block-b) (display "]: ")
    (cond
      ((and a-dom-b (not b-dom-a)) (display "dominates"))
      ((and b-dom-a (not a-dom-b)) (display "dominated-by"))
      ((and a-dom-b b-dom-a)       (display "same-block"))
      (else                        (display "no-dominance")))
    (newline)))

;; ── Pass 4 ───────────────────────────────────────────────
(display "── Pass 4: Check Ordering (SSA + CFG) ──") (newline)
(define dominance-count 0)
(for-each
  (lambda (cluster)
    (let* ((struct-name (car cluster))
           (fields (cadr cluster))
           ;; Find SSA functions that access 2+ cluster fields
           (interesting
             (filter-map
               (lambda (fn)
                 (let* ((fname (nf fn 'name))
                        (sites (field-access-sites fn fields))
                        (deduped (first-per-field sites)))
                   (and (>= (length deduped) 2)
                        (not (has-char? fname #\$))
                        (list fname deduped))))
               ssa-funcs)))
      (if (pair? interesting)
        (begin
          (display "  struct ") (display struct-name) (display ":") (newline)
          (for-each
            (lambda (entry)
              (let* ((fname (car entry))
                     (sites (cadr entry))
                     (results (check-dominance fname sites)))
                (if results
                  (begin
                    (display "    func ") (display fname) (display ":") (newline)
                    (for-each
                      (lambda (r)
                        (set! dominance-count (+ dominance-count 1))
                        (display-dominance-result r))
                      results))
                  (begin
                    (display "    func ") (display fname)
                    (display ": (CFG unavailable)") (newline)))))
            interesting)))))
  all-clusters)
(if (= dominance-count 0) (begin (display "  (none found)") (newline)))
(newline)

;; ── Summary ──────────────────────────────────────────────
(display "── Summary ──") (newline)
(display "  Boolean clusters:          ") (display (length all-clusters)) (newline)
(display "  Field sweep chains:        ") (display (length all-sweeps)) (newline)
(display "  Independent mutation sites: ") (display independence-count) (newline)
(display "  Dominance orderings:       ") (display dominance-count) (newline)
