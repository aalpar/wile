;;; ports-test.scm - R7RS 6.13 Input and output: edge cases and detailed coverage
;;;
;;; Edge cases and detailed coverage extracted from Go test suite
;;; (internal/extensions/io/prim_ports_test.go, prim_read_write_test.go).
;;; Complements the canonical R7RS tests in integration/testdata/r7rs-tests.scm.

(import (scheme base) (scheme read) (scheme write) (chibi test))

(test-begin "ports")

;; ── Port predicates for non-port types ──────────────────────────

(test-group "port? non-port types"
  (test #f (port? 42))
  (test #f (port? "hello"))
  (test #f (port? #t))
  (test #f (port? (eof-object))))

(test-group "input-port? non-port types"
  (test #f (input-port? 42))
  (test #f (input-port? (open-output-string)))
  (test #f (input-port? (open-output-bytevector))))

(test-group "output-port? non-port types"
  (test #f (output-port? 42))
  (test #f (output-port? (open-input-string "x")))
  (test #f (output-port? (open-input-bytevector #u8(1)))))

;; ── Bytevector port predicates ──────────────────────────────────

(test-group "textual-port? bytevector ports"
  (test #f (textual-port? (open-input-bytevector #u8(1))))
  (test #f (textual-port? (open-output-bytevector)))
  (test #f (textual-port? 42)))

(test-group "binary-port? string ports"
  (test #f (binary-port? (open-input-string "x")))
  (test #f (binary-port? (open-output-string)))
  (test #f (binary-port? 42)))

(test-group "bytevector port predicates"
  (test #t (port? (open-input-bytevector #u8(1))))
  (test #t (port? (open-output-bytevector)))
  (test #t (input-port? (open-input-bytevector #u8(1))))
  (test #t (output-port? (open-output-bytevector)))
  (test #t (binary-port? (open-input-bytevector #u8(1 2 3))))
  (test #t (binary-port? (open-output-bytevector))))

;; ── Port lifecycle: bytevector ports ────────────────────────────

(test-group "port-open? bytevector ports"
  (test #t (input-port-open? (open-input-bytevector #u8(1))))
  (test #f
    (let ((p (open-input-bytevector #u8(1))))
      (close-port p)
      (input-port-open? p)))
  (test #t (output-port-open? (open-output-bytevector)))
  (test #f
    (let ((p (open-output-bytevector)))
      (close-port p)
      (output-port-open? p))))

;; ── close-input-port, close-output-port ─────────────────────────

(test-group "close-input-port"
  (test #f
    (let ((p (open-input-string "x")))
      (close-input-port p)
      (input-port-open? p))))

(test-group "close-output-port"
  (test #f
    (let ((p (open-output-string)))
      (close-output-port p)
      (output-port-open? p))))

(test-group "close-port on bytevector ports"
  (test #f
    (let ((p (open-input-bytevector #u8(1))))
      (close-port p)
      (input-port-open? p)))
  (test #f
    (let ((p (open-output-bytevector)))
      (close-port p)
      (output-port-open? p))))

;; ── Error conditions: close-port, port-open? ────────────────────

(test-group "close-port errors"
  (test-error (close-port 42))
  (test-error (close-port "hello")))

(test-group "input-port-open? errors"
  (test-error (input-port-open? 42))
  (test-error (input-port-open? "hello"))
  (test-error (input-port-open? (open-output-string))))

(test-group "output-port-open? errors"
  (test-error (output-port-open? 42))
  (test-error (output-port-open? "hello"))
  (test-error (output-port-open? (open-input-string "x"))))

;; ── EOF predicates: non-eof values ──────────────────────────────

(test-group "eof-object? non-eof values"
  (test #f (eof-object? 42))
  (test #f (eof-object? #f))
  (test #f (eof-object? "hello"))
  (test #f (eof-object? '()))
  (test #t (eof-object? (read-char (open-input-string ""))))
  (test #t (eof-object? (read-u8 (open-input-bytevector #u8())))))

;; ── String ports: detailed coverage ─────────────────────────────

(test-group "string port write multiple"
  (test "hello world"
    (let ((p (open-output-string)))
      (write-string "hello" p)
      (write-string " world" p)
      (get-output-string p))))

(test-group "string port empty output"
  (test "" (get-output-string (open-output-string))))

(test-group "string port write-char"
  (test "AB"
    (let ((p (open-output-string)))
      (write-char #\A p)
      (write-char #\B p)
      (get-output-string p))))

(test-group "string port type checks"
  (test #t (textual-port? (open-input-string "test")))
  (test #t (input-port? (open-input-string "test")))
  (test #t (textual-port? (open-output-string)))
  (test #t (output-port? (open-output-string))))

(test-group "string port errors"
  (test-error (open-input-string 42))
  (test-error (get-output-string (open-input-string "x")))
  (test-error (get-output-string 42)))

;; ── Bytevector ports: detailed coverage ─────────────────────────

(test-group "bytevector port write and get"
  (test #u8(1 2 3)
    (let ((p (open-output-bytevector)))
      (write-bytevector #u8(1 2 3) p)
      (get-output-bytevector p))))

(test-group "bytevector port empty output"
  (test #u8()
    (let ((p (open-output-bytevector)))
      (get-output-bytevector p))))

(test-group "bytevector port type checks"
  (test #t (binary-port? (open-input-bytevector #u8(1 2 3))))
  (test #t (input-port? (open-input-bytevector #u8(1))))
  (test #t (binary-port? (open-output-bytevector)))
  (test #t (output-port? (open-output-bytevector))))

(test-group "bytevector port errors"
  (test-error (open-input-bytevector 42))
  (test-error (get-output-bytevector (open-input-bytevector #u8(1))))
  (test-error (get-output-bytevector 42)))

;; ── call-with-port ──────────────────────────────────────────────

(test-group "call-with-port returns result"
  (test #\h
    (call-with-port (open-input-string "hello")
      (lambda (p) (read-char p)))))

(test-group "call-with-port closes input port"
  (test #f
    (let ((p (open-input-string "hello")))
      (call-with-port p (lambda (port) (read-char port)))
      (input-port-open? p))))

(test-group "call-with-port closes output port"
  (test #f
    (let ((p (open-output-string)))
      (call-with-port p (lambda (port) (write-string "hi" port)))
      (output-port-open? p))))

(test-group "call-with-port string result"
  (test "hello world"
    (call-with-port (open-input-string "hello world")
      (lambda (p) (read-line p)))))

(test-group "call-with-port accumulates output"
  (test "abc"
    (call-with-port (open-output-string)
      (lambda (port)
        (write-string "abc" port)
        (get-output-string port)))))

(test-group "call-with-port errors"
  (test-error (call-with-port (open-input-string "x") 42)))

;; ── read-bytevector edge cases ──────────────────────────────────

(test-group "read-bytevector fewer than k bytes at EOF"
  (test #u8(1 2)
    (read-bytevector 10 (open-input-bytevector #u8(1 2)))))

(test-group "read-bytevector k=0"
  (test #u8()
    (read-bytevector 0 (open-input-bytevector #u8(1 2 3)))))

(test-group "read-bytevector successive reads"
  (test #t
    (let ((p (open-input-bytevector #u8(1 2))))
      (let ((bv1 (read-bytevector 10 p))
            (bv2 (read-bytevector 10 p)))
        (and (equal? bv1 #u8(1 2))
             (eof-object? bv2))))))

(test-group "read-bytevector full and single"
  (test #u8(10 20 30 40 50)
    (read-bytevector 5 (open-input-bytevector #u8(10 20 30 40 50))))
  (test #u8(99)
    (read-bytevector 1 (open-input-bytevector #u8(99)))))

(test-group "read-bytevector errors"
  (test-error (read-bytevector -1 (open-input-bytevector #u8(1 2))))
  (test-error (read-bytevector 5 (open-input-string "hello")))
  (test-error (read-bytevector "5" (open-input-bytevector #u8(1))))
  (test-error (read-bytevector 5 42)))

;; ── read-bytevector! edge cases ─────────────────────────────────

(test-group "read-bytevector! partial read returns count"
  (test 2
    (let ((bv (bytevector 0 0 0 0 0)))
      (read-bytevector! bv (open-input-bytevector #u8(10 20))))))

(test-group "read-bytevector! partial read fills correctly"
  (test #u8(10 20 3 4 5)
    (let ((bv (bytevector 1 2 3 4 5)))
      (read-bytevector! bv (open-input-bytevector #u8(10 20)))
      bv)))

(test-group "read-bytevector! with start and end"
  (test #u8(1 2 10 20 5)
    (let ((bv (bytevector 1 2 3 4 5)))
      (read-bytevector! bv (open-input-bytevector #u8(10 20)) 2 4)
      bv)))

(test-group "read-bytevector! successive reads"
  (test #t
    (let ((bv (bytevector 0 0 0 0 0))
          (p (open-input-bytevector #u8(1 2))))
      (let ((n1 (read-bytevector! bv p 0 5))
            (n2 (read-bytevector! bv p 0 5)))
        (and (equal? n1 2)
             (eof-object? n2)
             (equal? bv #u8(1 2 0 0 0)))))))

(test-group "read-bytevector! into middle"
  (test #u8(99 1 2 3 99)
    (let ((bv (bytevector 99 99 99 99 99)))
      (read-bytevector! bv (open-input-bytevector #u8(1 2 3)) 1 4)
      bv)))

(test-group "read-bytevector! single byte"
  (test #u8(42)
    (let ((bv (bytevector 0)))
      (read-bytevector! bv (open-input-bytevector #u8(42)))
      bv)))

(test-group "read-bytevector! zero length (start == end)"
  (test 0
    (let ((bv (bytevector 1 2 3)))
      (read-bytevector! bv (open-input-bytevector #u8(10 20)) 1 1))))

(test-group "read-bytevector! partial read returns correct count"
  (test 3
    (let ((bv (bytevector 0 0 0 0 0 0 0 0 0 0)))
      (read-bytevector! bv (open-input-bytevector #u8(1 2 3))))))

(test-group "read-bytevector! errors"
  (test-error (read-bytevector! "not-a-bv" (open-input-bytevector #u8(1))))
  (test-error (read-bytevector! (bytevector 1 2) (open-input-string "hi")))
  (test-error (read-bytevector! (bytevector 1 2) (open-input-bytevector #u8(1)) 10))
  (test-error (read-bytevector! (bytevector 1 2) (open-input-bytevector #u8(1)) 0 10))
  (test-error (read-bytevector! (bytevector 1 2) (open-input-bytevector #u8(1)) -1))
  (test-error (read-bytevector! (bytevector 1 2 3) (open-input-bytevector #u8(1)) 2 1)))

;; ── read: various types ─────────────────────────────────────────

(test-group "read various types"
  (test 42 (read (open-input-string "42")))
  (test 'hello (read (open-input-string "hello")))
  (test '(1 2 3) (read (open-input-string "(1 2 3)")))
  (test "abc" (read (open-input-string "\"abc\"")))
  (test #t (read (open-input-string "#t")))
  (test #f (read (open-input-string "#f")))
  (test #(1 2 3) (read (open-input-string "#(1 2 3)"))))

(test-group "read successive"
  (test #t
    (let ((p (open-input-string "1 2")))
      (let ((a (read p)) (b (read p)))
        (and (equal? a 1) (equal? b 2)))))
  (test #t
    (let ((p (open-input-string "42")))
      (read p)
      (eof-object? (read p)))))

(test-group "read errors"
  (test-error (read (open-input-bytevector #u8(1 2 3)))))

;; ── write vs display ────────────────────────────────────────────

(test-group "write quoted output"
  (test "42"
    (let ((p (open-output-string)))
      (write 42 p)
      (get-output-string p)))
  (test "\"hello\""
    (let ((p (open-output-string)))
      (write "hello" p)
      (get-output-string p)))
  (test "(1 2 3)"
    (let ((p (open-output-string)))
      (write '(1 2 3) p)
      (get-output-string p)))
  (test "#t"
    (let ((p (open-output-string)))
      (write #t p)
      (get-output-string p)))
  (test "#\\A"
    (let ((p (open-output-string)))
      (write #\A p)
      (get-output-string p)))
  (test "(1 (2 3) 4)"
    (let ((p (open-output-string)))
      (write '(1 (2 3) 4) p)
      (get-output-string p))))

(test-group "write errors"
  (test-error (write 42 42)))

(test-group "display unquoted output"
  (test "42"
    (let ((p (open-output-string)))
      (display 42 p)
      (get-output-string p)))
  (test "hello"
    (let ((p (open-output-string)))
      (display "hello" p)
      (get-output-string p)))
  (test "A"
    (let ((p (open-output-string)))
      (display #\A p)
      (get-output-string p)))
  (test "(1 2 3)"
    (let ((p (open-output-string)))
      (display '(1 2 3) p)
      (get-output-string p)))
  (test "hello"
    (let ((p (open-output-string)))
      (display 'hello p)
      (get-output-string p))))

(test-group "display errors"
  (test-error (display 42 42)))

;; ── newline ─────────────────────────────────────────────────────

(test-group "newline between writes"
  (test "a\nb"
    (let ((p (open-output-string)))
      (display "a" p)
      (newline p)
      (display "b" p)
      (get-output-string p))))

(test-group "newline errors"
  (test-error (newline 42)))

;; ── peek-char ───────────────────────────────────────────────────

(test-group "peek-char twice returns same"
  (test #t
    (let ((p (open-input-string "abc")))
      (let ((a (peek-char p)) (b (peek-char p)))
        (equal? a b)))))

(test-group "peek-char does not advance"
  (test #\x
    (let ((p (open-input-string "xy")))
      (peek-char p)
      (peek-char p)
      (read-char p))))

(test-group "peek-char then read same"
  (test #t
    (let ((p (open-input-string "hello")))
      (let ((peeked (peek-char p)) (read-val (read-char p)))
        (equal? peeked read-val)))))

(test-group "peek-char errors"
  (test-error (peek-char (open-input-bytevector #u8(1 2 3))))
  (test-error
    (let ((p (open-input-string "hello")))
      (close-port p)
      (peek-char p))))

;; ── peek-u8 ─────────────────────────────────────────────────────

(test-group "peek-u8 first byte"
  (test 65 (peek-u8 (open-input-bytevector #u8(65 66 67)))))

(test-group "peek-u8 twice returns same"
  (test #t
    (let ((p (open-input-bytevector #u8(10 20))))
      (let ((a (peek-u8 p)) (b (peek-u8 p)))
        (equal? a b)))))

(test-group "peek-u8 does not advance"
  (test 99
    (let ((p (open-input-bytevector #u8(99 100))))
      (peek-u8 p)
      (peek-u8 p)
      (read-u8 p))))

(test-group "peek-u8 then read same"
  (test #t
    (let ((p (open-input-bytevector #u8(42))))
      (let ((peeked (peek-u8 p)) (read-val (read-u8 p)))
        (equal? peeked read-val)))))

(test-group "peek-u8 empty returns eof"
  (test #t (eof-object? (peek-u8 (open-input-bytevector #u8())))))

(test-group "peek-u8 errors"
  (test-error (peek-u8 (open-input-string "hello")))
  (test-error
    (let ((p (open-input-bytevector #u8(1 2 3))))
      (close-port p)
      (peek-u8 p))))

;; ── write-simple ────────────────────────────────────────────────

(test-group "write-simple"
  (test "42"
    (let ((p (open-output-string)))
      (write-simple 42 p)
      (get-output-string p)))
  (test "\"hello\""
    (let ((p (open-output-string)))
      (write-simple "hello" p)
      (get-output-string p)))
  (test "(1 2 3)"
    (let ((p (open-output-string)))
      (write-simple '(1 2 3) p)
      (get-output-string p))))

(test-group "write-simple errors"
  (test-error (write-simple 42 42)))

;; ── write-shared ────────────────────────────────────────────────

(test-group "write-shared"
  (test "42"
    (let ((p (open-output-string)))
      (write-shared 42 p)
      (get-output-string p)))
  (test "(1 2 3)"
    (let ((p (open-output-string)))
      (write-shared '(1 2 3) p)
      (get-output-string p)))
  (test "\"abc\""
    (let ((p (open-output-string)))
      (write-shared "abc" p)
      (get-output-string p))))

(test-group "write-shared errors"
  (test-error (write-shared 42 42)))

;; ── read-line edge cases ────────────────────────────────────────

(test-group "read-line CRLF"
  (test "hello"
    (read-line (open-input-string "hello\r\nworld"))))

(test-group "read-line lone CR"
  (test "hello"
    (read-line (open-input-string "hello\rworld"))))

(test-group "read-line empty line"
  (test ""
    (read-line (open-input-string "\nworld"))))

(test-group "read-line successive"
  (test #t
    (let ((p (open-input-string "a\nb\nc")))
      (let ((a (read-line p)) (b (read-line p)) (c (read-line p)))
        (and (equal? a "a") (equal? b "b") (equal? c "c"))))))

(test-group "read-line errors"
  (test-error (read-line (open-input-bytevector #u8(1 2 3))))
  (test-error
    (let ((p (open-input-string "hello")))
      (close-port p)
      (read-line p))))

;; ── read-char edge cases ────────────────────────────────────────

(test-group "read-char unicode"
  (test #\λ (read-char (open-input-string "λ"))))

(test-group "read-char successive"
  (test #t
    (let ((p (open-input-string "ab")))
      (let ((a (read-char p)) (b (read-char p)))
        (and (equal? a #\a) (equal? b #\b))))))

(test-group "read-char errors"
  (test-error (read-char (open-input-bytevector #u8(65))))
  (test-error
    (let ((p (open-input-string "hello")))
      (close-port p)
      (read-char p))))

;; ── write-char edge cases ───────────────────────────────────────

(test-group "write-char unicode"
  (test "λ"
    (let ((p (open-output-string)))
      (write-char #\λ p)
      (get-output-string p))))

(test-group "write-char errors"
  (test-error (write-char 42))
  (test-error (write-char #\A 42)))

;; ── read-string edge cases ──────────────────────────────────────

(test-group "read-string fewer than k chars"
  (test "hi"
    (read-string 10 (open-input-string "hi"))))

(test-group "read-string successive"
  (test #t
    (let ((p (open-input-string "abcd")))
      (let ((a (read-string 2 p)) (b (read-string 2 p)))
        (and (equal? a "ab") (equal? b "cd"))))))

(test-group "read-string errors"
  (test-error (read-string -1 (open-input-string "hello")))
  (test-error (read-string "5" (open-input-string "hello")))
  (test-error (read-string 5 (open-input-bytevector #u8(1 2 3))))
  (test-error
    (let ((p (open-input-string "hello")))
      (close-port p)
      (read-string 3 p))))

;; ── write-string with start/end ─────────────────────────────────

(test-group "write-string with start"
  (test "llo"
    (let ((p (open-output-string)))
      (write-string "hello" p 2)
      (get-output-string p))))

(test-group "write-string with start and end"
  (test "el"
    (let ((p (open-output-string)))
      (write-string "hello" p 1 3)
      (get-output-string p))))

(test-group "write-string empty range"
  (test ""
    (let ((p (open-output-string)))
      (write-string "hello" p 2 2)
      (get-output-string p))))

(test-group "write-string errors"
  (test-error (write-string 42 (open-output-string)))
  (test-error (write-string "hello" 42))
  (test-error (write-string "hello" (open-output-string) 3 1))
  (test-error (write-string "hi" (open-output-string) 10)))

;; ── write-u8 edge cases ─────────────────────────────────────────

(test-group "write-u8 byte 0"
  (test #u8(0)
    (let ((p (open-output-bytevector)))
      (write-u8 0 p)
      (get-output-bytevector p))))

(test-group "write-u8 byte 255"
  (test #u8(255)
    (let ((p (open-output-bytevector)))
      (write-u8 255 p)
      (get-output-bytevector p))))

(test-group "write-u8 errors"
  (test-error (write-u8 256 (open-output-bytevector)))
  (test-error (write-u8 -1 (open-output-bytevector)))
  (test-error (write-u8 "x" (open-output-bytevector)))
  (test-error (write-u8 65 (open-output-string))))

;; ── write-bytevector with start/end ─────────────────────────────

(test-group "write-bytevector with start"
  (test #u8(3 4 5)
    (let ((p (open-output-bytevector)))
      (write-bytevector #u8(1 2 3 4 5) p 2)
      (get-output-bytevector p))))

(test-group "write-bytevector with start and end"
  (test #u8(2 3)
    (let ((p (open-output-bytevector)))
      (write-bytevector #u8(1 2 3 4 5) p 1 3)
      (get-output-bytevector p))))

(test-group "write-bytevector errors"
  (test-error (write-bytevector "abc" (open-output-bytevector)))
  (test-error (write-bytevector #u8(1 2 3) (open-output-bytevector) 2 1))
  (test-error (write-bytevector #u8(1 2 3) (open-output-string))))

;; ── read-u8 edge cases ──────────────────────────────────────────

(test-group "read-u8 successive"
  (test #t
    (let ((p (open-input-bytevector #u8(10 20))))
      (let ((a (read-u8 p)) (b (read-u8 p)))
        (and (equal? a 10) (equal? b 20))))))

(test-group "read-u8 errors"
  (test-error (read-u8 (open-input-string "hello")))
  (test-error
    (let ((p (open-input-bytevector #u8(1 2 3))))
      (close-port p)
      (read-u8 p))))

;; ── flush-output-port ───────────────────────────────────────────

(test-group "flush-output-port bytevector"
  (test #u8(42)
    (let ((p (open-output-bytevector)))
      (write-u8 42 p)
      (flush-output-port p)
      (get-output-bytevector p))))

(test-group "flush-output-port errors"
  (test-error (flush-output-port 42)))

;; ── char-ready? / u8-ready? ────────────────────────────────────

(test-group "char-ready?"
  (test #t (char-ready? (open-input-string "hello")))
  (test #t (char-ready? (open-input-string ""))))

(test-group "u8-ready?"
  (test #t (u8-ready? (open-input-bytevector #u8(1 2))))
  (test #t (u8-ready? (open-input-bytevector #u8()))))

(test-end)
(test-exit)
