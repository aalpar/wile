;;; string-ports.scm - String port examples
;;;
;;; Demonstrates: String input/output ports, in-memory I/O
;;;
;;; Usage: ./dist/scheme --file examples/io/string-ports.scm

;; Writing to a string port
(display "Writing to string port:")
(newline)

(define output-string
  (call-with-port (open-output-string)
    (lambda (port)
      (display "Hello" port)
      (display ", " port)
      (display "World" port)
      (display "!" port)
      (newline port)
      (display "Number: " port)
      (display 42 port)
      (newline port)
      (write '(a b c) port)
      (newline port)
      (get-output-string port))))

(display "Result string:")
(newline)
(display output-string)

;; Reading from a string port
(display "Reading from string port:")
(newline)

(define input-data "(+ 1 2 3)\n(* 4 5)\n")
(call-with-port (open-input-string input-data)
  (lambda (port)
    (let loop ((expr (read port))
               (count 1))
      (unless (eof-object? expr)
        (display "Expression ")
        (display count)
        (display ": ")
        (write expr)
        (display " => ")
        (display (eval expr (interaction-environment)))
        (newline)
        (loop (read port) (+ count 1))))))

;; Building complex output incrementally
(display "Building complex output:")
(newline)

(define (format-table rows)
  (call-with-port (open-output-string)
    (lambda (port)
      (for-each
        (lambda (row)
          (display "| " port)
          (for-each
            (lambda (cell)
              (display cell port)
              (display " | " port))
            row)
          (newline port))
        rows)
      (get-output-string port))))

(define table-data
  '(("Name" "Age" "City")
    ("Alice" 30 "NYC")
    ("Bob" 25 "LA")
    ("Charlie" 35 "Chicago")))

(display (format-table table-data))

;; Parsing structured text
(display "Parsing structured text:")
(newline)

(define csv-line "John,Doe,42,Engineer\n")
(call-with-port (open-input-string csv-line)
  (lambda (port)
    (define (read-until-comma-or-newline)
      (let loop ((chars '()))
        (let ((ch (read-char port)))
          (cond
            ((eof-object? ch)
             (list->string (reverse chars)))
            ((or (char=? ch #\,) (char=? ch #\newline))
             (list->string (reverse chars)))
            (else
             (loop (cons ch chars)))))))

    (let loop ((fields '()))
      (let ((field (read-until-comma-or-newline)))
        (if (string=? field "")
            (begin
              (display "Parsed fields: ")
              (write (reverse fields))
              (newline))
            (loop (cons field fields)))))))

;; Using string ports for serialization
(display "Object serialization:")
(newline)

(define (serialize-record name age hobbies)
  (call-with-port (open-output-string)
    (lambda (port)
      (write (list 'record name age hobbies) port)
      (get-output-string port))))

(define (deserialize-record str)
  (call-with-port (open-input-string str)
    (lambda (port)
      (let ((data (read port)))
        (if (and (list? data)
                 (eq? (car data) 'record)
                 (= (length data) 4))
            (cdr data)
            (error "Invalid record format"))))))

(define serialized (serialize-record "Alice" 30 '(reading coding hiking)))
(display "Serialized: ")
(display serialized)
(newline)

(define deserialized (deserialize-record serialized))
(display "Deserialized: ")
(write deserialized)
(newline)

(display "String port examples complete!")
(newline)
