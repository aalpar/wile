;;; records-test.scm - R7RS 5.5 Record types: extended coverage
;;;
;;; Test cases extracted from Go test suite:
;;;   - internal/extensions/all/prim_all_test.go
;;;
;;; Complements the canonical R7RS tests in integration/testdata/r7rs-tests.scm.
;;; The r7rs-tests.scm covers only a basic define-record-type (<pare>) example;
;;; this file provides thorough coverage of both the low-level and high-level APIs.

(import (scheme base)
        (chibi test))

(test-begin "records")

;; -- Low-level API: make-record-type ----------------------------------------

(test-group "make-record-type"
  (test #t (record-type? (make-record-type 'point '(x y))))
  (test #f (record? (make-record-type 'point '(x y))))
  (test #t (record-type? (make-record-type 'empty '())))
  (test #t (record-type? (make-record-type 'wrapper '(value)))))

(test-group "make-record-type errors"
  (test-error (make-record-type "point" '(x y)))
  (test-error (make-record-type 'point '(x "y")))
  ;; Wrong arity, deliberately. Written through apply because the compiler
  ;; rejects a statically-visible arity mismatch against a non-rebindable
  ;; callee before the program runs, and this test is asserting the RUNTIME
  ;; error. apply hides the argument count until then. Do not "simplify" this
  ;; back to (make-record-type) — the file stops compiling.
  (test-error (apply make-record-type (list))))

;; -- record-type? -----------------------------------------------------------

(test-group "record-type?"
  (test #t (record-type? (make-record-type 'foo '(a))))
  (test #f (record-type? 42))
  (test #f (record-type? "hello"))
  (test #f (record-type? '(1 2)))
  (test #f (record-type? #t)))

;; -- record? ----------------------------------------------------------------

(test-group "record?"
  (test #t (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '(x y))))
             (record? (ctor 1 2))))
  (test #f (record? (make-record-type 'foo '(a))))
  (test #f (record? 42))
  (test #f (record? "hello"))
  (test #f (record? '(1 . 2))))

;; -- record-type (accessor) -------------------------------------------------

(test-group "record-type accessor"
  (test #t (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '(x y)))
                  (p (ctor 1 2)))
             (record-type? (record-type p))))
  (test #t (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '(x y)))
                  (p (ctor 1 2)))
             (eq? rt (record-type p)))))

(test-group "record-type accessor errors"
  (test-error (record-type 42))
  (test-error (record-type (make-record-type 'foo '()))))

;; -- record-constructor -----------------------------------------------------

(test-group "record-constructor"
  (test #t (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '(x y))))
             (record? (ctor 10 20))))
  ;; partial constructor: supplied field gets value, omitted field defaults to #f
  (test 10 (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '(x)))
                  (p (ctor 10))
                  (get-x (record-accessor rt 'x)))
             (get-x p)))
  (test #f (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '(x)))
                  (p (ctor 10))
                  (get-y (record-accessor rt 'y)))
             (get-y p)))
  ;; empty constructor: all fields default to #f
  (test #f (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '()))
                  (p (ctor))
                  (get-x (record-accessor rt 'x)))
             (get-x p)))
  ;; field ordering: constructor arg order matches field-tag list, not type definition
  (test 30 (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '(y x)))
                  (p (ctor 20 10))
                  (get-x (record-accessor rt 'x))
                  (get-y (record-accessor rt 'y)))
             (+ (get-x p) (get-y p)))))

(test-group "record-constructor errors"
  (test-error (record-constructor 42 '(x)))
  (test-error (let ((rt (make-record-type 'point '(x y))))
                (record-constructor rt '(z))))
  (test-error (let ((rt (make-record-type 'point '(x y))))
                (record-constructor rt '("x")))))

;; -- record-predicate -------------------------------------------------------

(test-group "record-predicate"
  (test #t (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '(x y)))
                  (pred (record-predicate rt)))
             (pred (ctor 1 2))))
  (test #f (let* ((rt1 (make-record-type 'point '(x y)))
                  (rt2 (make-record-type 'color '(r g b)))
                  (ctor2 (record-constructor rt2 '(r g b)))
                  (pred1 (record-predicate rt1)))
             (pred1 (ctor2 255 0 0))))
  (test #f (let* ((rt (make-record-type 'point '(x y)))
                  (pred (record-predicate rt)))
             (pred 42)))
  (test #f (let* ((rt (make-record-type 'point '(x y)))
                  (pred (record-predicate rt)))
             (pred "hello"))))

(test-group "record-predicate errors"
  (test-error (record-predicate 42)))

;; -- record-accessor --------------------------------------------------------

(test-group "record-accessor"
  (test 10 (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '(x y)))
                  (get-x (record-accessor rt 'x)))
             (get-x (ctor 10 20))))
  (test 20 (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '(x y)))
                  (get-y (record-accessor rt 'y)))
             (get-y (ctor 10 20))))
  (test "Alice" (let* ((rt (make-record-type 'person '(name age)))
                       (ctor (record-constructor rt '(name age)))
                       (get-name (record-accessor rt 'name)))
                  (get-name (ctor "Alice" 30))))
  (test #t (let* ((rt (make-record-type 'flag '(value)))
                  (ctor (record-constructor rt '(value)))
                  (get-val (record-accessor rt 'value)))
             (get-val (ctor #t)))))

(test-group "record-accessor errors"
  (test-error (record-accessor 42 'x))
  (test-error (let ((rt (make-record-type 'point '(x y))))
                (record-accessor rt "x")))
  (test-error (let ((rt (make-record-type 'point '(x y))))
                (record-accessor rt 'z)))
  (test-error (let* ((rt1 (make-record-type 'point '(x y)))
                     (rt2 (make-record-type 'color '(r g b)))
                     (ctor2 (record-constructor rt2 '(r g b)))
                     (get-x (record-accessor rt1 'x)))
                (get-x (ctor2 255 0 0))))
  (test-error (let* ((rt (make-record-type 'point '(x y)))
                     (get-x (record-accessor rt 'x)))
                (get-x 42))))

;; -- record-modifier --------------------------------------------------------

(test-group "record-modifier"
  (test 99 (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '(x y)))
                  (get-x (record-accessor rt 'x))
                  (set-x! (record-modifier rt 'x))
                  (p (ctor 10 20)))
             (set-x! p 99)
             (get-x p)))
  (test 99 (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '(x y)))
                  (get-y (record-accessor rt 'y))
                  (set-y! (record-modifier rt 'y))
                  (p (ctor 10 20)))
             (set-y! p 99)
             (get-y p)))
  ;; modify preserves other fields
  (test 10 (let* ((rt (make-record-type 'point '(x y)))
                  (ctor (record-constructor rt '(x y)))
                  (get-x (record-accessor rt 'x))
                  (set-y! (record-modifier rt 'y))
                  (p (ctor 10 20)))
             (set-y! p 99)
             (get-x p)))
  ;; modify can change field type
  (test "hello" (let* ((rt (make-record-type 'box '(value)))
                       (ctor (record-constructor rt '(value)))
                       (get-val (record-accessor rt 'value))
                       (set-val! (record-modifier rt 'value))
                       (b (ctor 42)))
                  (set-val! b "hello")
                  (get-val b))))

(test-group "record-modifier errors"
  (test-error (record-modifier 42 'x))
  (test-error (let ((rt (make-record-type 'point '(x y))))
                (record-modifier rt "x")))
  (test-error (let ((rt (make-record-type 'point '(x y))))
                (record-modifier rt 'z)))
  (test-error (let* ((rt1 (make-record-type 'point '(x y)))
                     (rt2 (make-record-type 'color '(r g b)))
                     (ctor2 (record-constructor rt2 '(r g b)))
                     (set-x! (record-modifier rt1 'x)))
                (set-x! (ctor2 255 0 0) 42)))
  (test-error (let* ((rt (make-record-type 'point '(x y)))
                     (set-x! (record-modifier rt 'x)))
                (set-x! 42 99))))

;; -- define-record-type (high-level macro) ----------------------------------

(test-group "define-record-type construct and access"
  (test 7 (begin
            (define-record-type <point>
              (make-point x y)
              point?
              (x point-x)
              (y point-y))
            (let ((p (make-point 3 4)))
              (+ (point-x p) (point-y p))))))

(test-group "define-record-type predicate"
  (test #t (begin
             (define-record-type <pt>
               (make-pt x y)
               pt?
               (x pt-x)
               (y pt-y))
             (pt? (make-pt 1 2))))
  (test #f (begin
             (define-record-type <pt2>
               (make-pt2 x y)
               pt2?
               (x pt2-x)
               (y pt2-y))
             (pt2? 42)))
  (test #f (begin
             (define-record-type <pt3>
               (make-pt3 x y)
               pt3?
               (x pt3-x)
               (y pt3-y))
             (define-record-type <clr>
               (make-clr r g b)
               clr?
               (r clr-r)
               (g clr-g)
               (b clr-b))
             (pt3? (make-clr 255 0 0)))))

(test-group "define-record-type mutable fields"
  (test 10 (begin
             (define-record-type <mpt>
               (make-mpt x y)
               mpt?
               (x mpt-x mpt-set-x!)
               (y mpt-y mpt-set-y!))
             (let ((p (make-mpt 3 4)))
               (mpt-set-x! p 10)
               (mpt-x p))))
  ;; mutation preserves other fields
  (test 4 (begin
            (define-record-type <mpt2>
              (make-mpt2 x y)
              mpt2?
              (x mpt2-x mpt2-set-x!)
              (y mpt2-y))
            (let ((p (make-mpt2 3 4)))
              (mpt2-set-x! p 10)
              (mpt2-y p)))))

(test-group "define-record-type mixed mutable/immutable"
  (test "Bob" (begin
                (define-record-type <entry>
                  (make-entry key value)
                  entry?
                  (key entry-key)
                  (value entry-value entry-set-value!))
                (let ((e (make-entry 'name "Alice")))
                  (entry-set-value! e "Bob")
                  (entry-value e)))))

(test-group "define-record-type single field"
  (test 42 (begin
             (define-record-type <wrapper>
               (make-wrapper val)
               wrapper?
               (val wrapper-val))
             (wrapper-val (make-wrapper 42)))))

(test-group "define-record-type nested records"
  (test 3 (begin
            (define-record-type <npt>
              (make-npt x y)
              npt?
              (x npt-x)
              (y npt-y))
            (define-record-type <line>
              (make-line start end)
              line?
              (start line-start)
              (end line-end))
            (let ((l (make-line (make-npt 0 0) (make-npt 3 4))))
              (npt-x (line-end l))))))

(test-end)
(test-exit)
