;;; file-io.scm - File input/output examples
;;;
;;; Demonstrates: File reading, writing, line-by-line processing
;;; Wile-specific: R7RS file I/O primitives
;;;
;;; Usage: ./dist/scheme --file examples/io/file-io.scm

(define test-file "examples/io/test-data.txt")

;; Writing to a file
(display "Writing to file...")
(newline)
(call-with-output-file test-file
  (lambda (port)
    (display "First line of text" port)
    (newline port)
    (display "Second line with number: " port)
    (display 42 port)
    (newline port)
    (write '(a list of symbols) port)
    (newline port)))

;; Reading entire file as string
(display "Reading entire file:")
(newline)
(call-with-input-file test-file
  (lambda (port)
    (let loop ((char (read-char port)))
      (unless (eof-object? char)
        (display char)
        (loop (read-char port))))))
(newline)

;; Reading line by line
(display "Reading line by line:")
(newline)
(call-with-input-file test-file
  (lambda (port)
    (let loop ((line (read-line port))
               (line-num 1))
      (unless (eof-object? line)
        (display "Line ")
        (display line-num)
        (display ": ")
        (display line)
        (newline)
        (loop (read-line port) (+ line-num 1))))))

;; Reading S-expressions
(display "Reading S-expressions:")
(newline)
(call-with-input-file test-file
  (lambda (port)
    ;; Skip first two lines (they're strings)
    (read-line port)
    (read-line port)
    ;; Read the S-expression
    (let ((expr (read port)))
      (display "Read: ")
      (write expr)
      (newline)
      (display "First element: ")
      (display (car expr))
      (newline))))

;; Appending to a file
(display "Appending to file...")
(newline)
(let ((port (open-output-file test-file)))
  (display "Appended line" port)
  (newline port)
  (close-output-port port))

;; Using with-exception-handler for error handling
(display "File operations with error handling:")
(newline)
(with-exception-handler
  (lambda (exn)
    (display "Error: ")
    (display exn)
    (newline)
    #f)
  (lambda ()
    ;; Try to read a non-existent file
    (call-with-input-file "nonexistent.txt"
      (lambda (port)
        (read-line port)))))

;; Clean up
(display "Cleaning up test file...")
(newline)
(delete-file test-file)

(display "File I/O examples complete!")
(newline)
