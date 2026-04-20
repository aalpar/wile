;; Copyright 2026 Aaron Alpar
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; wile-axis-b.scm — Phase 3.B analyzer script.
;;;
;;; Enumerates wile's PrimitiveSpec registry via (registered-primitives)
;;; and narrows each primitive's result-writing sinks via go-ssa-narrow
;;; to produce:
;;;   - plans/axis-b-raw.scm                 (structured per-primitive data)
;;;   - plans/2026-04-19-axis-b-inventory.md (bucketed markdown inventory)
;;;   - stdout summary                       (bucket counts + reason tally)
;;;
;;; Invocation (from wile repo root):
;;;   wile-goast -f audit/wile-axis-b.scm
;;;
;;; Env overrides:
;;;   WILE_AXIS_B_NAMES      — comma-separated allowlist (default: all)
;;;                            Example: WILE_AXIS_B_NAMES=cons,null?,length,car
;;;   WILE_AXIS_B_RAW_OUTPUT — raw output path (default: plans/axis-b-raw.scm)
;;;   WILE_AXIS_B_INVENTORY  — inventory path  (default: plans/2026-04-19-axis-b-inventory.md)
;;;
;;; This script lives in wile because the analysis it performs is
;;; wile-specific. It iterates wile's PrimitiveSpec values (via the
;;; (registered-primitives) / primitive-spec-* wile primitives) and
;;; classifies them using wile's TypeConstraint vocabulary. The generic
;;; Go-SSA primitives it invokes — go-ssa-build and go-ssa-narrow —
;;; live in wile-goast.
;;;
;;; See plans/2026-04-19-axis-b-analyzer-design.md for the overall design.

(import (wile goast)
        (wile goast ssa)
        (wile goast utils))

;; ---------------------------------------------------------------------------
;; Path resolution
;; ---------------------------------------------------------------------------

;; env-or returns the env var value if set and non-empty, else the default.
(define (env-or name default)
  (let ((v (get-environment-variable name)))
    (if (and v (not (string=? v "")))
        v
        default)))

(define default-raw-output-path "plans/axis-b-raw.scm")
(define default-inventory-path  "plans/2026-04-19-axis-b-inventory.md")

(define (raw-output-path) (env-or "WILE_AXIS_B_RAW_OUTPUT" default-raw-output-path))
(define (inventory-path)  (env-or "WILE_AXIS_B_INVENTORY"  default-inventory-path))

;; ---------------------------------------------------------------------------
;; Entry construction — fed by (registered-primitives), the wile
;; introspection primitive for the PrimitiveSpec registry.
;; ---------------------------------------------------------------------------

;; spec->entry builds a (name declared-return go-function go-source) tuple
;; from a primitive-spec value — the same shape the earlier static manifest
;; file used, so the rest of the pipeline is unchanged.
(define (spec->entry spec)
  (list (primitive-spec-name spec)
        (primitive-spec-return-type spec)
        (primitive-spec-go-function spec)
        (primitive-spec-go-source spec)))

(define (entry-name e)            (list-ref e 0))
(define (entry-declared-return e) (list-ref e 1))
(define (entry-go-function e)     (list-ref e 2))
(define (entry-go-source e)       (list-ref e 3))

;; split-comma splits a comma-separated string into a list of strings.
;; Returns () for an empty input.
(define (split-comma s)
  (let ((n (string-length s)))
    (if (= n 0) '()
        (let loop ((i 0) (start 0) (acc '()))
          (cond
            ((= i n) (reverse (cons (substring s start n) acc)))
            ((char=? (string-ref s i) #\,)
             (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
            (else (loop (+ i 1) start acc)))))))

;; string-prefix? — true if s starts with prefix.
(define (string-prefix? prefix s)
  (let ((pn (string-length prefix))
        (sn (string-length s)))
    (and (<= pn sn)
         (string=? prefix (substring s 0 pn)))))

;; wile-primitive? — a registered primitive is "wile's" if its Impl
;; resolves to a function under github.com/aalpar/wile/, OR if it has
;; no Impl (binding-only — assoc, caar, etc. — all registered by wile).
;; Primitives under github.com/aalpar/wile-goast/ are wile-goast's and
;; must be excluded from wile's own audit.
(define wile-package-prefix "github.com/aalpar/wile/")

(define (wile-primitive? entry)
  (let ((go-fn (entry-go-function entry)))
    (or (string=? go-fn "")
        (string-prefix? wile-package-prefix go-fn))))

;; load-entries returns the (name declared go-fn go-src) list for every
;; wile primitive in the registry, filtered by WILE_AXIS_B_NAMES if set.
;; Excludes wile-goast's own primitives (go-ssa-*, go-parse-*, etc.)
;; since those aren't part of wile's primitive surface.
(define (load-entries)
  (let* ((all (map spec->entry (registered-primitives)))
         (wile-only (filter wile-primitive? all))
         (filter-str (env-or "WILE_AXIS_B_NAMES" "")))
    (cond
      ((string=? filter-str "") wile-only)
      (else
       (let ((allowed (split-comma filter-str)))
         (let loop ((es wile-only) (acc '()))
           (cond
             ((null? es) (reverse acc))
             ((member (entry-name (car es)) allowed)
              (loop (cdr es) (cons (car es) acc)))
             (else (loop (cdr es) acc)))))))))

;; ---------------------------------------------------------------------------
;; Sink-call enumeration
;; ---------------------------------------------------------------------------

;; string-index finds the first index of ch in s, or #f. Local helper —
;; defined before use because EvalMultiple compiles each top-level define
;; independently and forward references fail.
(define (string-index s ch)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (cond
        ((= i n) #f)
        ((char=? (string-ref s i) ch) i)
        (else (loop (+ i 1)))))))

;; strip-type-annotation turns "0:int" into "0", "t3" into "t3". Constant
;; operands are rendered by the mapper with a "<value>:<type>" suffix.
(define (strip-type-annotation s)
  (let ((idx (string-index s #\:)))
    (if idx (substring s 0 idx) s)))

;; Result-writing sink methods on wile's CallContext / *MachineContext. The
;; mapper exposes these as ssa-call instructions whose 'method field (invoke
;; mode) or 'func field (static call to a method-expression) equals one of
;; these strings. Research in plans/2026-04-19-pr-3-axis-b-script-impl.md.
(define sink-method-names '("SetValue" "SetValues"))

;; sink-call? returns #t if instr is an ssa-call to one of the sink methods.
;; Matches either invoke-mode (method field) or static-call-mode (func field).
(define (sink-call? instr)
  (and (tag? instr 'ssa-call)
       (let ((mode (nf instr 'mode)))
         (cond
           ((eq? mode 'invoke)
            (and (member (nf instr 'method) sink-method-names) #t))
           ((eq? mode 'call)
            (and (member (nf instr 'func) sink-method-names) #t))
           (else #f)))))

;; extract-arg-name returns the name of the first positional argument passed
;; to a sink call. The mapper renders args as a list of bare strings
;; ("t3", "0:int") or the literal value — we extract the string part before ':'.
;; Returns #f if the args list is empty.
(define (extract-arg-name instr)
  (let ((args (nf instr 'args)))
    (if (or (not args) (null? args))
        #f
        (let ((first (car args)))
          (cond
            ((string? first) (strip-type-annotation first))
            ((symbol? first) (symbol->string first))
            (else #f))))))

;; find-sink-calls walks an (ssa-func ...) alist and returns a list of
;; (sink-call-info (method <name>) (value-arg <name-string>)) tuples —
;; one per reachable call to SetValue / SetValues.
(define (find-sink-calls ssa-func)
  (let loop ((bs (nf ssa-func 'blocks)) (acc '()))
    (cond
      ((or (not bs) (null? bs)) (reverse acc))
      (else
       (let instr-loop ((is (nf (car bs) 'instrs)) (acc2 acc))
         (cond
           ((or (not is) (null? is))
            (loop (cdr bs) acc2))
           ((sink-call? (car is))
            (let ((arg (extract-arg-name (car is)))
                  (m   (or (nf (car is) 'method) (nf (car is) 'func))))
              (instr-loop (cdr is)
                          (cons (list 'sink-call-info
                                      (cons 'method m)
                                      (cons 'value-arg arg))
                                acc2))))
           (else (instr-loop (cdr is) acc2))))))))

;; ---------------------------------------------------------------------------
;; Function resolution from manifest entry to SSA function
;; ---------------------------------------------------------------------------

;; build-func-index returns an alist mapping (fn-name-string . ssa-func-alist).
;; Linear construction, then O(log N) lookups via assoc on the hot path.
(define (build-func-index ssa-funcs)
  (map (lambda (f) (cons (nf f 'name) f)) ssa-funcs))

;; resolve-function looks up a manifest entry in a func-index. Returns the
;; matching (ssa-func ...) alist, or #f for binding-only primitives (Impl is
;; nil — about 47 in the current manifest) or for primitives whose Go
;; function lives in an SSA-package not loaded in this run.
(define (resolve-function entry func-index)
  (let* ((go-fn (entry-go-function entry))
         (hit (assoc go-fn func-index)))
    (if hit (cdr hit) #f)))

;; ---------------------------------------------------------------------------
;; Per-primitive narrowing pipeline
;; ---------------------------------------------------------------------------

;; string-member? — membership by string=? (not eq?).
(define (string-member? s lst)
  (cond
    ((null? lst) #f)
    ((string=? s (car lst)) #t)
    (else (string-member? s (cdr lst)))))

;; union-strings deduplicates a list of strings (preserves first-seen order).
(define (union-strings xs)
  (let loop ((xs xs) (acc '()))
    (cond
      ((null? xs) (reverse acc))
      ((string-member? (car xs) acc) (loop (cdr xs) acc))
      (else (loop (cdr xs) (cons (car xs) acc))))))

;; union-symbols deduplicates a list of symbols.
(define (union-symbols xs)
  (let loop ((xs xs) (acc '()))
    (cond
      ((null? xs) (reverse acc))
      ((memq (car xs) acc) (loop (cdr xs) acc))
      (else (loop (cdr xs) (cons (car xs) acc))))))

;; merge-narrow-results — Scheme-side mirror of Go-side mergeResults. Unions
;; types + reasons across all inputs; confidence picks 'widened if any is
;; widened, else 'no-paths if all are no-paths, else 'narrow.
(define (merge-narrow-results rs)
  (if (null? rs)
      (list 'narrow-result
            (cons 'types '())
            (cons 'confidence 'no-paths)
            (cons 'reasons '()))
      (let loop ((rs rs)
                 (types '())
                 (reasons '())
                 (any-widened #f)
                 (all-no-paths #t))
        (cond
          ((null? rs)
           (let ((conf (cond (any-widened 'widened)
                             (all-no-paths 'no-paths)
                             (else 'narrow))))
             (list 'narrow-result
                   (cons 'types (union-strings (reverse types)))
                   (cons 'confidence conf)
                   (cons 'reasons (union-symbols (reverse reasons))))))
          (else
           (let* ((r (car rs))
                  (r-types (or (nf r 'types) '()))
                  (r-reasons (or (nf r 'reasons) '()))
                  (r-conf (nf r 'confidence))
                  (now-any-widened (or any-widened (eq? r-conf 'widened)))
                  (now-all-no-paths (and all-no-paths
                                         (eq? r-conf 'no-paths))))
             (loop (cdr rs)
                   (append (reverse r-types) types)
                   (append (reverse r-reasons) reasons)
                   now-any-widened
                   now-all-no-paths)))))))

;; ---------------------------------------------------------------------------
;; Go → wile type name mapping  (defined before analyze-primitive uses it)
;; ---------------------------------------------------------------------------

;; Maps the fully-qualified Go pointer-type strings that go-ssa-narrow
;; emits into the wile-facing type names from values/value_type.go. Keep
;; synchronized with wile/values/value_type.go typeNames table.
(define go-type->wile-type-table
  '(;; Numbers
    ("*github.com/aalpar/wile/values.Integer"             . "integer")
    ("*github.com/aalpar/wile/values.BigInteger"          . "integer")
    ("*github.com/aalpar/wile/values.Float"               . "flonum")
    ("*github.com/aalpar/wile/values.BigFloat"            . "flonum")
    ("*github.com/aalpar/wile/values.Rational"            . "rational")
    ("*github.com/aalpar/wile/values.Complex"             . "complex")
    ("*github.com/aalpar/wile/values.BigComplex"          . "complex")
    ;; Basic values
    ("*github.com/aalpar/wile/values.Boolean"             . "boolean")
    ("*github.com/aalpar/wile/values.Character"           . "character")
    ("*github.com/aalpar/wile/values.String"              . "string")
    ("*github.com/aalpar/wile/values.Symbol"              . "symbol")
    ("*github.com/aalpar/wile/values.Byte"                . "byte")
    ;; Collections
    ("*github.com/aalpar/wile/values.Pair"                . "pair")
    ("*github.com/aalpar/wile/values.Vector"              . "vector")
    ("*github.com/aalpar/wile/values.ByteVector"          . "bytevector")
    ("*github.com/aalpar/wile/values.Hashtable"           . "hashtable")
    ;; Ports — map to textual/binary input/output per R7RS
    ("*github.com/aalpar/wile/values.CharacterInputPort"  . "textual-input-port")
    ("*github.com/aalpar/wile/values.CharacterOutputPort" . "textual-output-port")
    ("*github.com/aalpar/wile/values.BinaryInputPort"     . "binary-input-port")
    ("*github.com/aalpar/wile/values.BinaryOutputPort"    . "binary-output-port")
    ("*github.com/aalpar/wile/values.StringInputPort"     . "textual-input-port")
    ("*github.com/aalpar/wile/values.StringOutputPort"    . "textual-output-port")
    ("*github.com/aalpar/wile/values.ByteVectorInputPort" . "binary-input-port")
    ("*github.com/aalpar/wile/values.ByteVectorOutputPort" . "binary-output-port")
    ;; Closures and procedures
    ("*github.com/aalpar/wile/values.MachineClosure"      . "procedure")
    ("*github.com/aalpar/wile/values.CaseLambdaClosure"   . "procedure")
    ("*github.com/aalpar/wile/machine.MachineClosure"     . "procedure")
    ;; Singletons (Void / EofObject / EmptyList) — record their wile form
    ("*github.com/aalpar/wile/values.voidType"            . "void")
    ("github.com/aalpar/wile/values.emptyListType"        . "list")
    ;; Opaque / record / advanced
    ("*github.com/aalpar/wile/values.Record"              . "record")
    ("*github.com/aalpar/wile/values.RecordType"          . "record-type")
    ("*github.com/aalpar/wile/values.Promise"             . "promise")
    ("*github.com/aalpar/wile/values.Box"                 . "box")
    ("*github.com/aalpar/wile/values.OpaqueValue"         . "opaque")))

;; go-type->wile-type returns the wile-facing name for a Go type string,
;; or the original string if unmapped (so unmapped types are visible in the
;; output and can be added to the table later).
(define (go-type->wile-type go-type)
  (let ((hit (assoc go-type go-type->wile-type-table)))
    (if hit (cdr hit) go-type)))

;; map-go-types applies go-type->wile-type across a list, deduplicates, and
;; preserves first-seen order.
(define (map-go-types go-types)
  (union-strings (map go-type->wile-type go-types)))

;; ---------------------------------------------------------------------------
;; Bucket classification
;; ---------------------------------------------------------------------------

;; Seven buckets per plans/2026-04-19-axis-b-analyzer-design.md §5.
;; Decision tree:
;;   no-paths confidence               -> Side-effecting
;;   widened confidence                -> Helper-widened
;;   narrowed set has 0 types          -> Helper-widened  (defensive)
;;   narrowed set has 1 type           -> Single
;;   narrowed set = {T, boolean}       -> Maybe(T)    (Racket-style #f sentinel)
;;   narrowed set has 2-3 types        -> Narrow-union
;;   narrowed set has 4+ types         -> Broad-union

(define (classify-bucket wile-types confidence reasons)
  (cond
    ((eq? confidence 'no-paths)
     'Side-effecting)
    ((eq? confidence 'widened)
     'Helper-widened)
    ((null? wile-types)
     'Helper-widened)
    ((= (length wile-types) 1)
     'Single)
    ((and (= (length wile-types) 2)
          (or (string-member? "boolean" wile-types)
              (string-member? "list" wile-types)))
     ;; {T, boolean} — the common Maybe(T) shape where #f signals absence.
     ;; {T, list} — also flagged since empty list sometimes plays the same role.
     'Maybe)
    ((<= (length wile-types) 3)
     'Narrow-union)
    (else
     'Broad-union)))

;; narrow-sink-call invokes go-ssa-narrow on the value-arg of a sink call.
;; Returns the narrow-result alist. Errors from go-ssa-narrow (typically
;; "no value named X" for Const operands like nil literals) are caught
;; and converted to a widened result with reason 'narrow-error.
(define (narrow-sink-call ssa-func sink-info)
  (let ((arg-name (nf sink-info 'value-arg)))
    (cond
      ((not arg-name)
       (list 'narrow-result
             (cons 'types '())
             (cons 'confidence 'no-paths)
             (cons 'reasons '(missing-arg))))
      (else
       (guard (exn (#t (list 'narrow-result
                             (cons 'types '())
                             (cons 'confidence 'widened)
                             (cons 'reasons '(narrow-error)))))
         (go-ssa-narrow ssa-func arg-name))))))

;; analyze-primitive orchestrates resolution → sink enumeration → narrowing →
;; merge, returning an (axis-b-entry ...) alist. For unresolved entries
;; (binding-only, or out-of-package), emits an axis-b-entry with
;; (status unresolved).
(define (analyze-primitive entry func-index)
  (let ((fn (resolve-function entry func-index)))
    (if (not fn)
        (list 'axis-b-entry
              (cons 'name (entry-name entry))
              (cons 'declared-return-type (entry-declared-return entry))
              (cons 'go-function (entry-go-function entry))
              (cons 'go-source (entry-go-source entry))
              (cons 'status 'unresolved)
              (cons 'narrowed '())
              (cons 'confidence 'no-paths)
              (cons 'reasons '()))
        (let* ((sinks (find-sink-calls fn))
               (narrows (map (lambda (s) (narrow-sink-call fn s)) sinks))
               (merged (merge-narrow-results narrows))
               (go-types (nf merged 'types))
               (wile-types (map-go-types go-types))
               (conf (nf merged 'confidence))
               (reasons (nf merged 'reasons))
               (bucket (classify-bucket wile-types conf reasons)))
          (list 'axis-b-entry
                (cons 'name (entry-name entry))
                (cons 'declared-return-type (entry-declared-return entry))
                (cons 'go-function (entry-go-function entry))
                (cons 'go-source (entry-go-source entry))
                (cons 'status 'resolved)
                (cons 'sink-count (length sinks))
                (cons 'narrowed-go go-types)
                (cons 'narrowed wile-types)
                (cons 'confidence conf)
                (cons 'reasons reasons)
                (cons 'bucket bucket))))))

;; ---------------------------------------------------------------------------
;; Raw S-expression output
;; ---------------------------------------------------------------------------

;; write-scheme-string writes s with escaped quotes/backslashes to port.
(define (write-scheme-string port s)
  (display "\"" port)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (cond
        ((= i n) #f)
        (else
         (let ((c (string-ref s i)))
           (cond
             ((char=? c #\") (display "\\\"" port))
             ((char=? c #\\) (display "\\\\" port))
             (else (display c port)))
           (loop (+ i 1)))))))
  (display "\"" port))

;; write-string-list writes a list of strings as ("a" "b" "c").
(define (write-string-list port xs)
  (display "(" port)
  (let loop ((xs xs) (first? #t))
    (cond
      ((null? xs) (display ")" port))
      (else
       (when (not first?) (display " " port))
       (write-scheme-string port (car xs))
       (loop (cdr xs) #f)))))

;; write-symbol-list writes a list of symbols as (a b c).
(define (write-symbol-list port xs)
  (display "(" port)
  (let loop ((xs xs) (first? #t))
    (cond
      ((null? xs) (display ")" port))
      (else
       (when (not first?) (display " " port))
       (display (car xs) port)
       (loop (cdr xs) #f)))))

;; emit-raw-entry writes one axis-b-entry as an S-expression primitive record.
(define (emit-raw-entry port e)
  (display "(primitive\n" port)
  (display "  (name " port)
  (write-scheme-string port (nf e 'name))
  (display ")\n" port)
  (display "  (impl\n" port)
  (display "    (go-function " port)
  (write-scheme-string port (nf e 'go-function))
  (display ")\n" port)
  (display "    (go-source " port)
  (write-scheme-string port (nf e 'go-source))
  (display "))\n" port)
  (display "  (declared-return-type " port)
  (write-scheme-string port (nf e 'declared-return-type))
  (display ")\n" port)
  (display "  (narrowed-return-types " port)
  (write-string-list port (or (nf e 'narrowed) '()))
  (display ")\n" port)
  (display "  (confidence " port)
  (display (nf e 'confidence) port)
  (display ")\n" port)
  (display "  (reasons " port)
  (write-symbol-list port (or (nf e 'reasons) '()))
  (display ")\n" port)
  (display "  (bucket " port)
  (display (or (nf e 'bucket) 'Unresolved) port)
  (display "))\n" port))

;; write-raw-output writes a header comment and a list of primitive records
;; to path. Each record spans multiple lines for readability.
(define (write-raw-output entries path)
  (let ((port (open-output-file path)))
    (display ";; Axis-B raw inventory — generated by wile-axis-b.scm\n" port)
    (display ";; See wile-goast/plans/2026-04-19-pr-3-axis-b-script-impl.md\n" port)
    (display ";;\n" port)
    (display ";; Format: one (primitive ...) record per analyzed manifest entry.\n" port)
    (display ";; Do not edit by hand — re-run wile-goast --run wile-axis-b.\n" port)
    (newline port)
    (for-each (lambda (e) (emit-raw-entry port e)) entries)
    (close-output-port port)))

;; ---------------------------------------------------------------------------
;; Markdown inventory
;; ---------------------------------------------------------------------------

(define bucket-order
  '(Single Maybe Narrow-union Broad-union Helper-widened Side-effecting Unresolved))

(define bucket-descriptions
  '((Single         . "Exactly one concrete wile type across all paths — no type-system gap.")
    (Maybe          . "Two types where one is boolean (Racket #f-sentinel pattern) — candidate for Maybe(T) in TypeConstraint vocabulary.")
    (Narrow-union   . "2-3 distinct types — candidate for enumerated TypeUnion with small arity.")
    (Broad-union    . "4+ distinct types — expensive to encode; worth shipping only if count is high.")
    (Helper-widened . "Analyzer couldn't narrow — typically interface-method dispatch, sub-context delegation, or helper return of interface-typed values. Not a type-system gap; an analysis-tool gap.")
    (Side-effecting . "No result-writing sink reached — likely error-only returns, panic-terminated paths, or binding-only primitive.")
    (Unresolved     . "Go function not found in loaded SSA packages (binding-only primitive or missing package).")))

;; group-by-bucket partitions entries into an alist (bucket . entries).
(define (group-by-bucket entries)
  (let loop ((es entries) (groups '()))
    (cond
      ((null? es) groups)
      (else
       (let* ((e (car es))
              (b (or (nf e 'bucket) 'Unresolved))
              (hit (assq b groups)))
         (loop (cdr es)
               (if hit
                   (begin
                     (set-cdr! hit (cons e (cdr hit)))
                     groups)
                   (cons (cons b (list e)) groups))))))))

(define (bucket-entries groups b)
  (let ((hit (assq b groups)))
    (if hit (reverse (cdr hit)) '())))

(define (display-list-joined port xs sep)
  (let loop ((xs xs) (first? #t))
    (cond
      ((null? xs) #f)
      (else
       (when (not first?) (display sep port))
       (display (car xs) port)
       (loop (cdr xs) #f)))))

(define (emit-markdown-inventory entries path)
  (let* ((port (open-output-file path))
         (groups (group-by-bucket entries))
         (total (length entries)))
    (display "# Axis-B Inventory\n\n" port)
    (display "Generated by `wile-goast --run wile-axis-b`. " port)
    (display "See `wile-goast/plans/2026-04-19-pr-3-axis-b-script-impl.md`.\n\n" port)
    (display (string-append "Total primitives analyzed: "
                            (number->string total) "\n\n") port)
    (display "| Bucket | Count |\n|---|---|\n" port)
    (for-each
     (lambda (b)
       (let ((entries-in (bucket-entries groups b)))
         (display "| " port) (display b port)
         (display " | " port) (display (length entries-in) port)
         (display " |\n" port)))
     bucket-order)
    (newline port)
    (for-each
     (lambda (b)
       (let* ((entries-in (bucket-entries groups b))
              (count (length entries-in)))
         (when (> count 0)
           (display "## " port) (display b port)
           (display " (" port) (display count port)
           (display ")\n\n" port)
           (let ((desc (assq b bucket-descriptions)))
             (when desc
               (display (cdr desc) port)
               (display "\n\n" port)))
           (display "| Primitive | Declared | Narrowed | Reasons |\n" port)
           (display "|---|---|---|---|\n" port)
           (for-each
            (lambda (e)
              (display "| `" port) (display (nf e 'name) port) (display "` | " port)
              (display (nf e 'declared-return-type) port) (display " | " port)
              (let ((nt (or (nf e 'narrowed) '())))
                (if (null? nt)
                    (display "—" port)
                    (display-list-joined port nt ", ")))
              (display " | " port)
              (let ((rs (or (nf e 'reasons) '())))
                (if (null? rs)
                    (display "—" port)
                    (display-list-joined port rs ", ")))
              (display " |\n" port))
            entries-in)
           (newline port))))
     bucket-order)
    (display "## Type-system recommendations\n\n" port)
    (display "TODO — distill from the bucket counts above. Typical questions:\n\n" port)
    (display "- Does `Maybe` have enough entries to justify a `TypeMaybe(T)` constructor?\n" port)
    (display "- Is `Narrow-union` dominated by 2-3 recurring pairs that could be named?\n" port)
    (display "- What's the Helper-widened reason-tag distribution pointing to for PR-2'?\n" port)
    (close-output-port port)))

;; ---------------------------------------------------------------------------
;; Stdout summary
;; ---------------------------------------------------------------------------

(define (emit-summary entries)
  (let ((groups (group-by-bucket entries))
        (total (length entries)))
    (display "\nSummary: ") (display total) (display " primitives\n")
    (for-each
     (lambda (b)
       (let ((n (length (bucket-entries groups b))))
         (when (> n 0)
           (display "  ") (display b)
           (display ": ") (display n)
           (newline))))
     bucket-order)
    ;; Reason-tag histogram
    (let ((reason-tally '()))
      (for-each
       (lambda (e)
         (for-each
          (lambda (r)
            (let ((hit (assq r reason-tally)))
              (if hit
                  (set-cdr! hit (+ 1 (cdr hit)))
                  (set! reason-tally (cons (cons r 1) reason-tally)))))
          (or (nf e 'reasons) '())))
       entries)
      (when (pair? reason-tally)
        (display "\nReason tags:\n")
        (for-each
         (lambda (pair) (display "  ") (display (car pair))
                        (display ": ") (display (cdr pair)) (newline))
         reason-tally)))))

;; ---------------------------------------------------------------------------
;; Main entry
;; ---------------------------------------------------------------------------

(define (main)
  (let ((entries (load-entries)))
    (display "wile-axis-b: loaded ")
    (display (length entries))
    (display " primitives from (registered-primitives)")
    (newline)
    (parameterize ((current-go-target "github.com/aalpar/wile/..."))
      (display "wile-axis-b: building SSA for wile/... (this takes a minute)")
      (newline)
      (let* ((funcs (go-ssa-build))
             (idx (build-func-index funcs)))
        (display "wile-axis-b: indexed ") (display (length funcs))
        (display " SSA functions") (newline)
        (let ((results (map (lambda (e) (analyze-primitive e idx)) entries)))
          (display "wile-axis-b: analyzed ") (display (length results))
          (display " primitives") (newline)
          (let ((rpath (raw-output-path)))
            (write-raw-output results rpath)
            (display "wile-axis-b: wrote raw S-expression to ")
            (display rpath) (newline))
          (let ((ipath (inventory-path)))
            (emit-markdown-inventory results ipath)
            (display "wile-axis-b: wrote markdown inventory to ")
            (display ipath) (newline))
          (emit-summary results))))))

(main)
