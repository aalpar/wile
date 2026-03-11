;;; association-lists.scm - Association list patterns
;;;
;;; Demonstrates: assoc, assq, assv, alist operations, key-value storage
;;; Wile-specific: Full R7RS alist support
;;;
;;; Usage: ./dist/wile --file examples/data-structures/association-lists.scm

;; Association lists (alists) store key-value pairs as lists of cons cells.
;; Simple, functional alternative to hash tables for small datasets.

(display "=== Association Lists in Wile ===\n")
(newline)

;; Example 1: Basic alist structure
(display "Example 1: Creating and querying an alist\n")

(define phonebook
  '((alice . "555-1234")
    (bob . "555-5678")
    (carol . "555-9012")))

(display "  Phonebook:\n")
(for-each
 (lambda (entry)
   (display "    ")
   (display (car entry))
   (display ": ")
   (display (cdr entry))
   (newline))
 phonebook)
(newline)

(display "  Looking up 'bob': ")
(let ((entry (assoc 'bob phonebook)))
  (if entry
      (display (cdr entry))
      (display "not found")))
(newline)
(newline)

;; Example 2: Different equality predicates
(display "Example 2: assoc vs assq vs assv\n")

(define alist1 '((a . 1) (b . 2) (c . 3)))
(define alist2 '(("x" . 10) ("y" . 20) ("z" . 30)))
(define alist3 '((1.0 . "one") (2.0 . "two") (3.0 . "three")))

(display "  assq (eq? comparison, for symbols):\n")
(display "    (assq 'b '((a . 1) (b . 2))) = ")
(display (assq 'b alist1))
(newline)
(newline)

(display "  assv (eqv? comparison, for numbers/chars):\n")
(display "    (assv 2.0 '((1.0 . \"one\") (2.0 . \"two\"))) = ")
(display (assv 2.0 alist3))
(newline)
(newline)

(display "  assoc (equal? comparison, for any values):\n")
(display "    (assoc \"y\" '((\"x\" . 10) (\"y\" . 20))) = ")
(display (assoc "y" alist2))
(newline)
(newline)

;; Example 3: Adding entries
(display "Example 3: Adding entries to an alist\n")

(define (alist-set alist key value)
  ;; Add or update entry (functional - returns new alist)
  (cons (cons key value) alist))

(define config '((verbose . #f) (debug . #f)))
(display "  Initial: ")
(display config)
(newline)

(set! config (alist-set config 'verbose #t))
(display "  After setting verbose=#t: ")
(display config)
(newline)
(display "  Note: old 'verbose . #f' still present (shadowed)\n")
(newline)

;; Example 4: Removing entries
(display "Example 4: Removing entries\n")

(define (alist-remove alist key)
  ;; Remove all entries with key (functional)
  (let loop ((lst alist) (result '()))
    (cond
     ((null? lst) (reverse result))
     ((equal? (caar lst) key) (loop (cdr lst) result))
     (else (loop (cdr lst) (cons (car lst) result))))))

(define data '((a . 1) (b . 2) (c . 3) (b . 99)))
(display "  Original: ")
(display data)
(newline)
(display "  After removing 'b: ")
(display (alist-remove data 'b))
(newline)
(newline)

;; Example 5: Updating entries
(display "Example 5: Updating entries (functional)\n")

(define (alist-update alist key value)
  ;; Remove old entry and add new one
  (cons (cons key value) (alist-remove alist key)))

(define scores '((alice . 100) (bob . 85) (carol . 92)))
(display "  Original scores: ")
(display scores)
(newline)
(set! scores (alist-update scores 'bob 90))
(display "  After updating bob to 90: ")
(display scores)
(newline)
(newline)

;; Example 6: Nested alists (JSON-like structures)
(display "Example 6: Nested alists\n")

(define user
  '((name . "Alice")
    (age . 30)
    (address . ((street . "123 Main St")
                (city . "Anytown")
                (zip . "12345")))))

(display "  User data:\n")
(display "    Name: ")
(display (cdr (assoc 'name user)))
(newline)
(display "    Age: ")
(display (cdr (assoc 'age user)))
(newline)
(display "    City: ")
(let ((address (cdr (assoc 'address user))))
  (display (cdr (assoc 'city address))))
(newline)
(newline)

;; Example 7: Merging alists
(display "Example 7: Merging alists\n")

(define (alist-merge alist1 alist2)
  ;; alist2 entries take precedence
  (let loop ((lst alist2) (result alist1))
    (if (null? lst)
        result
        (loop (cdr lst)
              (alist-update result (caar lst) (cdar lst))))))

(define defaults '((timeout . 30) (retries . 3) (verbose . #f)))
(define overrides '((timeout . 60) (debug . #t)))

(display "  Defaults: ")
(display defaults)
(newline)
(display "  Overrides: ")
(display overrides)
(newline)
(display "  Merged: ")
(display (alist-merge defaults overrides))
(newline)
(newline)

;; Example 8: Alist as simple cache
(display "Example 8: Using alist as a cache\n")

(define cache '())

(define (cached-fib n)
  (let ((cached (assoc n cache)))
    (if cached
        (begin
          (display "    [cache hit for n=")
          (display n)
          (display "]\n")
          (cdr cached))
        (begin
          (display "    [computing fib(")
          (display n)
          (display ")]\n")
          (let ((result
                 (if (<= n 1)
                     n
                     (+ (cached-fib (- n 1))
                        (cached-fib (- n 2))))))
            (set! cache (cons (cons n result) cache))
            result)))))

(display "  Computing fib(6) with caching:\n")
(display "  Result: ")
(display (cached-fib 6))
(newline)
(display "  Computing fib(8) (reuses cache):\n")
(display "  Result: ")
(display (cached-fib 8))
(newline)
(newline)

;; Example 9: Environment representation
(display "Example 9: Alist as environment/scope\n")

(define (make-env)
  '())

(define (env-lookup env var)
  (let ((binding (assoc var env)))
    (if binding
        (cdr binding)
        (error "Unbound variable" var))))

(define (env-extend env var val)
  (cons (cons var val) env))

(define env (make-env))
(set! env (env-extend env 'x 10))
(set! env (env-extend env 'y 20))
(set! env (env-extend env 'z 30))

(display "  Environment: ")
(display env)
(newline)
(display "  (env-lookup env 'y) = ")
(display (env-lookup env 'y))
(newline)
(newline)

;; Example 10: Frequency counting
(display "Example 10: Frequency counting with alist\n")

(define (count-frequencies lst)
  (let loop ((items lst) (counts '()))
    (if (null? items)
        counts
        (let* ((item (car items))
               (entry (assoc item counts))
               (new-count (if entry (+ 1 (cdr entry)) 1)))
          (loop (cdr items)
                (alist-update counts item new-count))))))

(define words '(the quick brown fox jumps over the lazy dog the))
(display "  Words: ")
(display words)
(newline)
(display "  Frequencies: ")
(display (count-frequencies words))
(newline)
(newline)

;; Example 11: Sorting alist by values
(display "Example 11: Sorting alist by values\n")

;; Simple insertion sort (appropriate for small alists)
(define (insert-sorted item lst less-than?)
  (cond
   ((null? lst) (list item))
   ((less-than? item (car lst)) (cons item lst))
   (else (cons (car lst) (insert-sorted item (cdr lst) less-than?)))))

(define (insertion-sort lst less-than?)
  (let loop ((remaining lst) (sorted '()))
    (if (null? remaining)
        sorted
        (loop (cdr remaining)
              (insert-sorted (car remaining) sorted less-than?)))))

(define (alist-sort-by-value alist)
  (insertion-sort alist (lambda (a b) (< (cdr a) (cdr b)))))

(define grades '((alice . 92) (bob . 87) (carol . 95) (dave . 84)))
(display "  Grades: ")
(display grades)
(newline)
(display "  Sorted: ")
(display (alist-sort-by-value grades))
(newline)
(newline)

;; Summary
(display "=== Summary ===\n")
(display "Association lists:\n")
(display "  • Simple key-value storage using lists\n")
(display "  • assoc/assq/assv for lookups with different equality\n")
(display "  • Functional updates (cons new entry)\n")
(display "  • Perfect for small datasets (<100 entries)\n")
(display "  • Easy to serialize/deserialize\n")
(display "  • Use hash tables for larger datasets\n")
(newline)
(display "Alists are the simplest associative data structure!\n")
