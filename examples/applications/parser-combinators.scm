;;; parser-combinators.scm - Composable parsing from first-class procedures
;;;
;;; Demonstrates: closures as parsers, define-record-type, case-lambda,
;;;               higher-order composition, string operations
;;;
;;; Usage: ./dist/wile --file examples/applications/parser-combinators.scm

;; -----------------------------------------------------------------------
;; Parse results
;;
;; A parser is: (string, position) -> (value . new-position) | #f
;; Success returns (value . position), failure returns #f.
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Primitive parsers
;; -----------------------------------------------------------------------

;; Match a single character satisfying a predicate
(define (parse-char pred)
  (lambda (str pos)
    (if (>= pos (string-length str))
        #f
        (let ((c (string-ref str pos)))
          (if (pred c)
              (cons c (+ pos 1))
              #f)))))

;; Match a literal character
(define (parse-literal ch)
  (parse-char (lambda (c) (char=? c ch))))

;; Match a literal string
(define (parse-string target)
  (lambda (str pos)
    (let ((len (string-length target)))
      (if (> (+ pos len) (string-length str))
          #f
          (let loop ((i 0))
            (if (= i len)
                (cons target (+ pos len))
                (if (char=? (string-ref str (+ pos i))
                            (string-ref target i))
                    (loop (+ i 1))
                    #f)))))))

;; Always succeed with a value (doesn't consume input)
(define (parse-return val)
  (lambda (str pos) (cons val pos)))

;; Always fail
(define parse-fail
  (lambda (str pos) #f))

;; -----------------------------------------------------------------------
;; Combinators
;; -----------------------------------------------------------------------

;; Sequential: run p1, then p2, combine results
(define (parse-seq p1 p2 combine)
  (lambda (str pos)
    (let ((r1 (p1 str pos)))
      (if r1
          (let ((r2 (p2 str (cdr r1))))
            (if r2
                (cons (combine (car r1) (car r2)) (cdr r2))
                #f))
          #f))))

;; Alternative: try p1, if it fails try p2
(define (parse-alt p1 p2)
  (lambda (str pos)
    (let ((r1 (p1 str pos)))
      (if r1 r1 (p2 str pos)))))

;; Zero or more repetitions
(define (parse-many p)
  (lambda (str pos)
    (let loop ((pos pos) (acc '()))
      (let ((r (p str pos)))
        (if r
            (loop (cdr r) (cons (car r) acc))
            (cons (reverse acc) pos))))))

;; One or more repetitions
(define (parse-many1 p)
  (lambda (str pos)
    (let ((r (p str pos)))
      (if r
          (let ((rest ((parse-many p) str (cdr r))))
            (cons (cons (car r) (car rest)) (cdr rest)))
          #f))))

;; Transform the result of a parser
(define (parse-map f p)
  (lambda (str pos)
    (let ((r (p str pos)))
      (if r
          (cons (f (car r)) (cdr r))
          #f))))

;; Lazy parser for recursive grammars (breaks mutual recursion)
(define (parse-lazy thunk)
  (lambda (str pos)
    ((thunk) str pos)))

;; Left-associative binary operator chain:  expr (op expr)*
(define (parse-chainl p op)
  (lambda (str pos)
    (let ((r (p str pos)))
      (if r
          (let loop ((left (car r)) (pos (cdr r)))
            (let ((r-op (op str pos)))
              (if r-op
                  (let ((r-right (p str (cdr r-op))))
                    (if r-right
                        (loop ((car r-op) left (car r-right))
                              (cdr r-right))
                        (cons left pos)))
                  (cons left pos))))
          #f))))

;; Optional parser: succeed with default if parser fails
(define (parse-optional p default)
  (lambda (str pos)
    (let ((r (p str pos)))
      (if r r (cons default pos)))))

;; Skip whitespace, then apply parser
(define (lexeme p)
  (lambda (str pos)
    (let ((r ((parse-many (parse-char (lambda (c)
                (or (char=? c #\space)
                    (char=? c #\tab))))) str pos)))
      (p str (cdr r)))))

;; -----------------------------------------------------------------------
;; Arithmetic expression parser
;;
;; Grammar:
;;   expr   = term (('+' | '-') term)*
;;   term   = factor (('*' | '/') factor)*
;;   factor = number | '-' factor | '(' expr ')'
;;   number = digit+
;; -----------------------------------------------------------------------

(define parse-digit
  (parse-char (lambda (c) (and (char>=? c #\0) (char<=? c #\9)))))

(define parse-number
  (lexeme
   (parse-map
    (lambda (digits) (string->number (list->string digits)))
    (parse-many1 parse-digit))))

(define parse-addop
  (lexeme
   (parse-alt
    (parse-map (lambda (c) +) (parse-literal #\+))
    (parse-map (lambda (c) -) (parse-literal #\-)))))

(define parse-mulop
  (lexeme
   (parse-alt
    (parse-map (lambda (c) *) (parse-literal #\*))
    (parse-map (lambda (c) /) (parse-literal #\/)))))

(define (parse-factor str pos)
  ((parse-alt
    parse-number
    (parse-alt
     ;; Parenthesized expression
     (parse-seq
      (parse-seq (lexeme (parse-literal #\())
                 (parse-lazy (lambda () parse-expr))
                 (lambda (open expr) expr))
      (lexeme (parse-literal #\)))
      (lambda (expr close) expr))
     ;; Unary minus
     (parse-seq
      (lexeme (parse-literal #\-))
      (parse-lazy (lambda () parse-factor))
      (lambda (neg val) (- val)))))
   str pos))

(define (parse-term str pos)
  ((parse-chainl parse-factor parse-mulop) str pos))

(define (parse-expr str pos)
  ((parse-chainl parse-term parse-addop) str pos))

;; -----------------------------------------------------------------------
;; Top-level parse function
;; -----------------------------------------------------------------------

(define (parse input)
  (let ((r (parse-expr input 0)))
    (if (and r (= (cdr r) (string-length input)))
        (car r)
        (if r
            (error "Unexpected input at position" (cdr r))
            (error "Parse failed" input)))))

;; -----------------------------------------------------------------------
;; Demo
;; -----------------------------------------------------------------------

(define (eval-show expr)
  (display "  \"")
  (display expr)
  (display "\" = ")
  (display (parse expr))
  (newline))

(display "=== Parser Combinators ===\n\n")

(display "--- Arithmetic expressions ---\n")
(eval-show "42")
(eval-show "2 + 3")
(eval-show "2 + 3 * 4")
(eval-show "(2 + 3) * 4")
(eval-show "100 - 30 - 20")
(eval-show "10 / 2 / 5")
(eval-show "-(3 + 4)")
(eval-show "1 + 2 * 3 + 4 * 5 + 6")

(display "\n--- Showing it's a real parser ---\n")
(display "  Parsed \"2+3*4\" correctly as ")
(display (parse "2+3*4"))
(display " (not ")
(display (* (+ 2 3) 4))
(display ")\n")
(display "  Left-associative: \"10-3-2\" = ")
(display (parse "10-3-2"))
(display " (not ")
(display (- 10 (- 3 2)))
(display ")\n")

(display "\nParsers as closures — composition without frameworks.\n")
