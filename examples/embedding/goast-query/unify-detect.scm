;;; unify-detect.scm — Prototype: Procedure unification detection via goast
;;;
;;; Parses two Go source strings, extracts functions by name,
;;; computes a recursive AST diff, classifies differences, and scores.
;;;
;;; Usage: wile -f examples/embedding/goast-query/unify-detect.scm

;; ══════════════════════════════════════════════════════════
;; Shared utilities (reused from state-trace examples)
;; ══════════════════════════════════════════════════════════

(define (nf node key)
  (let ((e (assoc key (cdr node))))
    (if e (cdr e) #f)))

(define (tag? node t)
  (and (pair? node) (symbol? (car node)) (eq? (car node) t)))

(define (tagged-node? v)
  (and (pair? v) (symbol? (car v))))

(define (node-list? v)
  (and (pair? v) (pair? (car v)) (not (symbol? (car v)))))

(define (filter-map f lst)
  (let loop ((xs lst) (acc '()))
    (if (null? xs) (reverse acc)
      (let ((v (f (car xs))))
        (loop (cdr xs) (if v (cons v acc) acc))))))

(define (flat-map f lst)
  (apply append (map f lst)))

;; ══════════════════════════════════════════════════════════
;; Core diff engine
;;
;; Compares two goast s-expressions recursively.
;; Returns: (shared-count diff-count diffs)
;;   where each diff is: (category path val-a val-b)
;; ══════════════════════════════════════════════════════════

;; Merge two diff results: sum counts, concatenate diff lists.
(define (merge-results a b)
  (list (+ (car a) (car b))
        (+ (cadr a) (cadr b))
        (append (caddr a) (caddr b))))

;; A single shared node (no diffs).
(define (shared-result)
  (list 1 0 '()))

;; A single diff.
(define (diff-result category path val-a val-b)
  (list 0 1 (list (list category path val-a val-b))))

;; Fold merge-results over a list.
(define (merge-all results)
  (let loop ((rs results) (acc (list 0 0 '())))
    (if (null? rs) acc
      (loop (cdr rs) (merge-results acc (car rs))))))

;; Compare two field alists: for each field in node-a, find the same
;; key in node-b and diff the values. Fields in b but not a are extra.
(define (fields-diff fields-a fields-b path)
  (let ((results
          (filter-map
            (lambda (pair-a)
              (if (not (pair? pair-a)) #f
                (let* ((key (car pair-a))
                       (val-a (cdr pair-a))
                       (entry-b (assoc key fields-b)))
                  (if entry-b
                    (ast-diff val-a (cdr entry-b)
                              (append path (list key)))
                    (diff-result 'missing-field
                                 (append path (list key))
                                 val-a #f)))))
            fields-a)))
    ;; Fields in b but not a
    (let ((extra
            (filter-map
              (lambda (pair-b)
                (if (not (pair? pair-b)) #f
                  (let ((key (car pair-b)))
                    (if (assoc key fields-a) #f
                      (diff-result 'extra-field
                                   (append path (list key))
                                   #f (cdr pair-b))))))
              fields-b)))
      (merge-all (append results extra)))))

;; Compare two lists element-wise (positional zip).
;; Extra elements in either list are structural diffs.
(define (list-diff lst-a lst-b path idx)
  (cond
    ;; Both empty — done.
    ((and (null? lst-a) (null? lst-b))
     (list 0 0 '()))
    ;; a has extra elements.
    ((null? lst-b)
     (merge-all
       (map (lambda (ea)
              (diff-result 'extra-element
                           (append path (list idx))
                           ea #f))
            lst-a)))
    ;; b has extra elements.
    ((null? lst-a)
     (merge-all
       (map (lambda (eb)
              (diff-result 'extra-element
                           (append path (list idx))
                           #f eb))
            lst-b)))
    ;; Both have elements — diff heads, recurse tails.
    (else
      (merge-results
        (ast-diff (car lst-a) (car lst-b)
                  (append path (list idx)))
        (list-diff (cdr lst-a) (cdr lst-b) path (+ idx 1))))))

;; ── Main dispatch ─────────────────────────────────────────

(define (ast-diff node-a node-b path)
  (cond
    ;; Both are tagged alist nodes.
    ((and (tagged-node? node-a) (tagged-node? node-b))
     (if (eq? (car node-a) (car node-b))
       ;; Same tag: count the tag match, then diff fields.
       (merge-results
         (shared-result)
         (fields-diff (cdr node-a) (cdr node-b) path))
       ;; Different tag: structural divergence.
       (diff-result 'structural path node-a node-b)))

    ;; Both are lists of child nodes (e.g., statement lists).
    ((and (list? node-a) (list? node-b))
     (list-diff node-a node-b path 0))

    ;; Both are #f (nil/absent field).
    ((and (eq? node-a #f) (eq? node-b #f))
     (shared-result))

    ;; Both are identical atoms.
    ((equal? node-a node-b)
     (shared-result))

    ;; Both are strings but different.
    ((and (string? node-a) (string? node-b))
     (diff-result (classify-string-diff node-a node-b path)
                  path node-a node-b))

    ;; Both are symbols but different.
    ((and (symbol? node-a) (symbol? node-b))
     (diff-result 'operator path node-a node-b))

    ;; Both are booleans but different.
    ((and (boolean? node-a) (boolean? node-b))
     (diff-result 'literal-value path node-a node-b))

    ;; Both are numbers but different.
    ((and (number? node-a) (number? node-b))
     (diff-result 'literal-value path node-a node-b))

    ;; Type mismatch between atoms or atom vs node.
    (else
      (diff-result 'structural path node-a node-b))))

;; ══════════════════════════════════════════════════════════
;; Difference classification
;;
;; Classifies string diffs based on context (field path).
;; Without type annotations, we use the path to infer
;; whether a string is a type name, identifier, or literal.
;; ══════════════════════════════════════════════════════════

;; Fields whose string values are identifiers (variable/param names).
(define identifier-fields '(name sel label))

;; Fields whose string values are type names or type strings.
(define type-fields '(type inferred-type asserted-type obj-pkg
                      signature))

;; Does the path pass through a type-position field?
;; If the ident's 'name is reached via a 'type ancestor,
;; it's a type name, not a variable name.
;; e.g. path (...  type  name) means the ident is in type position.
(define (in-type-position? path)
  (let loop ((xs path))
    (cond
      ((null? xs) #f)
      ((null? (cdr xs)) #f)  ;; last element is the leaf field, skip it
      ((and (symbol? (car xs)) (memq (car xs) type-fields))
       #t)
      (else (loop (cdr xs))))))

;; Classify a string diff by its position in the AST.
(define (classify-string-diff str-a str-b path)
  (let ((field (if (null? path) #f (last-element path))))
    (cond
      ;; Explicit type fields at the leaf.
      ((and (symbol? field) (memq field type-fields))
       'type-name)
      ;; Ident name reached through a type-position ancestor.
      ((and (symbol? field) (eq? field 'name)
            (in-type-position? path))
       'type-name)
      ;; Known identifier fields.
      ((and (symbol? field) (memq field identifier-fields))
       'identifier)
      ;; String literal values (in lit nodes, the 'value field).
      ((and (symbol? field) (eq? field 'value))
       'literal-value)
      ;; Token/operator fields.
      ((and (symbol? field) (eq? field 'tok))
       'operator)
      ;; Fallback: treat as identifier (conservative — free rename).
      (else 'identifier))))

(define (last-element lst)
  (if (null? (cdr lst)) (car lst)
    (last-element (cdr lst))))

;; ══════════════════════════════════════════════════════════
;; Scoring
;; ══════════════════════════════════════════════════════════

(define diff-weights
  '((type-name . 1)
    (literal-value . 1)
    (identifier . 0)
    (operator . 2)
    (structural . 100)
    (missing-field . 50)
    (extra-field . 50)
    (extra-element . 50)))

(define (diff-weight category)
  (let ((entry (assoc category diff-weights)))
    (if entry (cdr entry) 10)))

(define (unique lst)
  (let loop ((xs lst) (seen '()))
    (cond ((null? xs) (reverse seen))
          ((member (car xs) seen) (loop (cdr xs) seen))
          (else (loop (cdr xs) (cons (car xs) seen))))))

(define (score-diffs shared-count diff-count diffs)
  (let* ((total (+ shared-count diff-count))
         (similarity (if (> total 0)
                       (exact->inexact (/ shared-count total))
                       0.0))
         (weighted-cost
           (apply + (map (lambda (d) (diff-weight (car d))) diffs)))
         ;; Count distinct parameter types needed.
         (type-diffs
           (filter-map
             (lambda (d)
               (and (eq? (car d) 'type-name)
                    (cons (caddr d) (cadddr d))))
             diffs))
         (unique-type-params (unique type-diffs))
         (value-diffs
           (filter-map
             (lambda (d)
               (and (eq? (car d) 'literal-value)
                    (cons (caddr d) (cadddr d))))
             diffs))
         (unique-value-params (unique value-diffs))
         (param-count (+ (length unique-type-params)
                         (length unique-value-params))))
    (list similarity param-count weighted-cost
          unique-type-params unique-value-params)))

;; ══════════════════════════════════════════════════════════
;; Function extraction
;; ══════════════════════════════════════════════════════════

;; Extract all func-decl nodes from a parsed file.
(define (extract-funcs file-ast)
  (filter-map
    (lambda (decl)
      (and (tag? decl 'func-decl) decl))
    (nf file-ast 'decls)))

;; Find a func-decl by name.
(define (find-func file-ast name)
  (let loop ((funcs (extract-funcs file-ast)))
    (cond
      ((null? funcs) #f)
      ((equal? (nf (car funcs) 'name) name) (car funcs))
      (else (loop (cdr funcs))))))

;; ══════════════════════════════════════════════════════════
;; Reporting
;; ══════════════════════════════════════════════════════════

(define (display-diff-entry d)
  (let ((category (list-ref d 0))
        (path (list-ref d 1))
        (val-a (list-ref d 2))
        (val-b (list-ref d 3)))
    (display "    ")
    (display category)
    (display "  at ")
    (display path)
    (newline)
    (display "      a: ")
    (display (summarize val-a))
    (newline)
    (display "      b: ")
    (display (summarize val-b))
    (newline)))

;; Truncate large AST nodes for display.
(define (summarize val)
  (cond
    ((not val) "#f")
    ((string? val) val)
    ((symbol? val) (symbol->string val))
    ((number? val) (number->string val))
    ((boolean? val) (if val "#t" "#f"))
    ((and (pair? val) (symbol? (car val)))
     (string-append "(" (symbol->string (car val)) " ...)"))
    ((pair? val) "(list ...)")
    (else "?")))

(define (display-result name-a name-b shared-count diff-count diffs)
  (let ((score (score-diffs shared-count diff-count diffs)))
    (newline)
    (display "  ")
    (display name-a) (display " <-> ") (display name-b)
    (newline)
    (display "  shared nodes:    ") (display shared-count) (newline)
    (display "  diff nodes:      ") (display diff-count) (newline)
    (display "  similarity:      ") (display (list-ref score 0)) (newline)
    (display "  params needed:   ") (display (list-ref score 1)) (newline)
    (display "  weighted cost:   ") (display (list-ref score 2)) (newline)
    (if (pair? (list-ref score 3))
      (begin
        (display "  type params:     ") (display (list-ref score 3))
        (newline)))
    (if (pair? (list-ref score 4))
      (begin
        (display "  value params:    ") (display (list-ref score 4))
        (newline)))
    (newline)
    ;; Group diffs by category.
    (let ((categories (unique (map car diffs))))
      (for-each
        (lambda (cat)
          (let ((count (length (filter (lambda (d) (eq? (car d) cat))
                                       diffs))))
            (display "  [") (display cat) (display "] x")
            (display count) (display ":") (newline)
            (for-each
              (lambda (d)
                (if (eq? (car d) cat)
                  (display-diff-entry d)))
              diffs)
            (newline)))
        categories))))

;; filter: R7RS (srfi 1) not guaranteed, inline it.
(define (filter pred lst)
  (let loop ((xs lst) (acc '()))
    (if (null? xs) (reverse acc)
      (loop (cdr xs)
            (if (pred (car xs))
              (cons (car xs) acc)
              acc)))))

;; ══════════════════════════════════════════════════════════
;; Test: pncounter.Increment vs gcounter.Increment
;;
;; Inline Go source — self-contained, no disk dependencies.
;; Stripped to Increment + supporting types to focus the diff.
;; ══════════════════════════════════════════════════════════

(define source-pncounter "
package pncounter

type Dot struct {
	ID  string
	Seq uint64
}

type CounterValue struct {
	N int64
}

type Counter struct {
	id    string
	store map[Dot]CounterValue
}

func (p *Counter) Increment(n int64) *Counter {
	var oldDot Dot
	var oldValue int64
	hasOld := false

	for d, v := range p.store {
		if d.ID == p.id {
			oldDot = d
			oldValue = v.N
			hasOld = true
			break
		}
	}

	newVal := CounterValue{N: oldValue + n}
	if hasOld {
		delete(p.store, oldDot)
	}
	p.store[Dot{ID: p.id}] = newVal

	delta := make(map[Dot]CounterValue)
	delta[Dot{ID: p.id}] = newVal

	return &Counter{store: delta}
}
")

(define source-gcounter "
package gcounter

type Dot struct {
	ID  string
	Seq uint64
}

type GValue struct {
	N uint64
}

type Counter struct {
	id    string
	store map[Dot]GValue
}

func (g *Counter) Increment(n uint64) *Counter {
	var oldDot Dot
	var oldValue uint64
	hasOld := false

	for d, v := range g.store {
		if d.ID == g.id {
			oldDot = d
			oldValue = v.N
			hasOld = true
			break
		}
	}

	newVal := GValue{N: oldValue + n}
	if hasOld {
		delete(g.store, oldDot)
	}
	g.store[Dot{ID: g.id}] = newVal

	delta := make(map[Dot]GValue)
	delta[Dot{ID: g.id}] = newVal

	return &Counter{store: delta}
}
")

;; ── Run ───────────────────────────────────────────────────

(display "Parsing source A (pncounter) ...") (newline)
(define file-a (go-parse-string source-pncounter))

(display "Parsing source B (gcounter) ...") (newline)
(define file-b (go-parse-string source-gcounter))

(define func-a (find-func file-a "Increment"))
(define func-b (find-func file-b "Increment"))

(if (not func-a) (error "Increment not found in source A"))
(if (not func-b) (error "Increment not found in source B"))

(display "Diffing Increment functions ...") (newline)

(let ((result (ast-diff func-a func-b '(Increment))))
  (display-result "pncounter.Increment" "gcounter.Increment"
                  (car result) (cadr result) (caddr result)))

;; ── Also diff the full files to find all matching function pairs ──

(display "══════════════════════════════════════════")
(newline)
(display "  Full file comparison: all shared names  ")
(newline)
(display "══════════════════════════════════════════")
(newline)

(let* ((funcs-a (extract-funcs file-a))
       (funcs-b (extract-funcs file-b))
       (pairs (filter-map
                (lambda (fa)
                  (let* ((name (nf fa 'name))
                         (fb (find-func file-b name)))
                    (and fb (list name fa fb))))
                funcs-a)))
  (if (null? pairs)
    (begin (display "  (no functions with matching names)") (newline))
    (for-each
      (lambda (entry)
        (let* ((name (car entry))
               (fa (cadr entry))
               (fb (caddr entry))
               (result (ast-diff fa fb (list (string->symbol name)))))
          (display-result
            (string-append "A." name)
            (string-append "B." name)
            (car result) (cadr result) (caddr result))))
      pairs)))
