;;; binary-io.scm - Binary I/O with bytevectors
;;;
;;; Demonstrates: Bytevector operations, binary file I/O
;;;
;;; Usage: ./dist/scheme --file examples/io/binary-io.scm

(define test-file "examples/io/test-binary.dat")

;; Creating and manipulating bytevectors
(display "Creating bytevectors:")
(newline)

(define bv1 (make-bytevector 10 0))
(display "Empty bytevector (length 10): ")
(display bv1)
(newline)

(define bv2 (bytevector 1 2 3 4 5))
(display "Initialized bytevector: ")
(display bv2)
(newline)

;; Setting and getting values
(bytevector-u8-set! bv1 0 255)
(bytevector-u8-set! bv1 1 128)
(bytevector-u8-set! bv1 2 64)

(display "After setting bytes: ")
(display bv1)
(newline)

(display "First byte: ")
(display (bytevector-u8-ref bv1 0))
(newline)

;; Writing binary data to file
(display "Writing binary data to file...")
(newline)
(call-with-port (open-binary-output-file test-file)
  (lambda (port)
    ;; Write header bytes
    (write-u8 255 port)
    (write-u8 254 port)

    ;; Write a bytevector
    (write-bytevector bv2 port)

    ;; Write more individual bytes
    (write-u8 0 port)
    (write-u8 1 port)))

;; Reading binary data from file
(display "Reading binary data from file:")
(newline)
(call-with-port (open-binary-input-file test-file)
  (lambda (port)
    (display "Header byte 1: ")
    (display (read-u8 port))
    (newline)

    (display "Header byte 2: ")
    (display (read-u8 port))
    (newline)

    ;; Read the bytevector
    (let ((read-bv (read-bytevector 5 port)))
      (display "Read bytevector: ")
      (display read-bv)
      (newline))

    (display "Remaining byte 1: ")
    (display (read-u8 port))
    (newline)

    (display "Remaining byte 2: ")
    (display (read-u8 port))
    (newline)

    ;; Check for EOF
    (let ((eof-check (read-u8 port)))
      (display "EOF reached: ")
      (display (eof-object? eof-check))
      (newline))))

;; Bytevector copying
(display "Bytevector operations:")
(newline)

(define src (bytevector 10 20 30 40 50))
(define dst (make-bytevector 5 0))

(bytevector-copy! dst 0 src)
(display "Copied bytevector: ")
(display dst)
(newline)

;; Partial copy
(define dst2 (make-bytevector 3 0))
(bytevector-copy! dst2 0 src 1 4)
(display "Partial copy (src[1:4]): ")
(display dst2)
(newline)

;; String to bytevector conversion (UTF-8)
(display "String/bytevector conversion:")
(newline)

(define text "Hello, 世界!")
(define utf8-bytes (string->utf8 text))
(display "Text as UTF-8 bytes: ")
(display utf8-bytes)
(newline)

(define decoded (utf8->string utf8-bytes))
(display "Decoded back to string: ")
(display decoded)
(newline)

;; Working with multi-byte integers
(display "Multi-byte integer operations:")
(newline)

;; Note: Wile may or may not have these procedures
;; This is a demonstration of the R7RS API
(with-exception-handler
  (lambda (exn)
    (display "Multi-byte operations not fully supported")
    (newline))
  (lambda ()
    ;; Try to use u16/u32 operations if available
    (let ((bv (make-bytevector 8 0)))
      ;; These are R7RS but may not be in Wile yet
      (display "Demonstrating multi-byte API (if available)")
      (newline))))

;; Bytevector comparison
(display "Bytevector comparison:")
(newline)

(define bv-a (bytevector 1 2 3))
(define bv-b (bytevector 1 2 3))
(define bv-c (bytevector 1 2 4))

(display "bv-a equal to bv-b: ")
(display (equal? bv-a bv-b))
(newline)

(display "bv-a equal to bv-c: ")
(display (equal? bv-a bv-c))
(newline)

;; Clean up
(display "Cleaning up test file...")
(newline)
(delete-file test-file)

(display "Binary I/O examples complete!")
(newline)
