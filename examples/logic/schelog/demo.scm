;;; demo.scm - Schelog demonstration
;;;
;;; Demonstrates: Prolog-style logic programming in Scheme
;;;               Relations, unification, backtracking, cut
;;;
;;; Usage:
;;;   cd <wile-root>
;;;   ./dist/wile -i -f examples/logic/schelog/schelog.scm -f examples/logic/schelog/demo.scm
;;;
;;; For interactive exploration:
;;;   ./dist/wile -i -f examples/logic/schelog/schelog.scm
;;;   > (%which (x) (%member x '(a b c)))
;;;   > (%more)

(display "=== Schelog: Prolog in Scheme ===\n\n")

;; -----------------------------------------------------------------------
;; Example 1: Family relationships
;; -----------------------------------------------------------------------

(display "--- Family relationships ---\n")

;; Define parent facts
(define %parent
  (%rel ()
    [('tom 'bob)]
    [('tom 'liz)]
    [('bob 'ann)]
    [('bob 'pat)]
    [('pat 'jim)]))

;; Define grandparent rule
(define %grandparent
  (%rel (x y z)
    [(x y) (%parent x z) (%parent z y)]))

;; Query: Who are Tom's children?
(display "Tom's children: ")
(display (%which (child) (%parent 'tom child)))
(newline)

;; Query: Who are Bob's grandchildren?
(display "Bob's grandchildren: ")
(let ((result (%which (gc) (%grandparent 'bob gc))))
  (display result)
  (newline))

;; -----------------------------------------------------------------------
;; Example 2: List membership
;; -----------------------------------------------------------------------

(display "\n--- List membership ---\n")

;; %member is built into schelog, let's use it
(display "Is 'b' in '(a b c)? ")
(display (if (%which () (%member 'b '(a b c))) "yes" "no"))
(newline)

(display "Is 'x' in '(a b c)? ")
(display (if (%which () (%member 'x '(a b c))) "yes" "no"))
(newline)

;; Find all members
(display "Members of '(1 2 3): ")
(let loop ((result (%which (x) (%member x '(1 2 3)))))
  (when result
    (display (cdr (car result)))
    (display " ")
    (loop (%more))))
(newline)

;; -----------------------------------------------------------------------
;; Example 3: Append relation
;; -----------------------------------------------------------------------

(display "\n--- Append relation ---\n")

;; %append is built into schelog
(display "Append '(a b) and '(c d): ")
(display (%which (z) (%append '(a b) '(c d) z)))
(newline)

;; Use append to split a list
(display "Split '(1 2 3) into two parts:\n")
(let loop ((result (%which (x y) (%append x y '(1 2 3)))))
  (when result
    (display "  ")
    (display result)
    (newline)
    (loop (%more))))

;; -----------------------------------------------------------------------
;; Example 4: Arithmetic
;; -----------------------------------------------------------------------

(display "\n--- Arithmetic constraints ---\n")

;; Find X where X + 3 = 7
(display "X + 3 = 7, X = ")
(%let (x)
  (%which () (%is x (- 7 3)))
  (display (schelog:deref* x)))
(newline)

;; -----------------------------------------------------------------------
;; Example 5: Negation as failure
;; -----------------------------------------------------------------------

(display "\n--- Negation as failure ---\n")

(define %likes
  (%rel ()
    [('mary 'food)]
    [('mary 'wine)]
    [('john 'wine)]
    [('john 'mary)]))

(display "Mary likes food? ")
(display (if (%which () (%likes 'mary 'food)) "yes" "no"))
(newline)

(display "John likes food? ")
(display (if (%which () (%likes 'john 'food)) "yes" "no"))
(newline)

(display "John does NOT like food? ")
(display (if (%which () (%not (%likes 'john 'food))) "yes" "no"))
(newline)

(display "\nSchelog — Prolog's power with Scheme's elegance.\n")
