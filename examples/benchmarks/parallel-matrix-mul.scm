;;; parallel-matrix-mul.scm — does SRFI-18 threading speed up matrix multiply?
;;;
;;; FINDING (2026-06-08, 16-core darwin): NO. Wile's VM does not provide CPU
;;; parallelism for compute-bound Scheme. Adding threads makes work *slower*,
;;; monotonically with thread count. Root cause: every closure `Apply`
;;; acquires/releases an environment frame (and non-tail calls a continuation
;;; frame + eval stack) from process-global, mutex-guarded freelists in
;;; machine/pool.go. Concurrent threads serialize on those mutexes; GC and
;;; allocation pressure add further contention. See the pure-compute control
;;; (`bench-threads`) which has nothing to do with matrices and shows the same
;;; inversion.
;;;
;;; Run:  wile --file examples/benchmarks/parallel-matrix-mul.scm
;;;
;;; This benchmark is kept as a regression marker: if a future VM change makes
;;; the parallel rows here BEAT the 1-thread row, threading became worthwhile.

(import (wile algebra matrix) (wile algebra semiring) (wile threads) (scheme time)
        (scheme base) (scheme write))

(define (iota n)
  (let loop ((i (- n 1)) (acc '()))
    (if (< i 0) acc (loop (- i 1) (cons i acc)))))

(define (best-ms thunk reps)
  (let loop ((i 0) (best 1e18))
    (if (= i reps) best
        (let* ((t0 (current-jiffy)) (r (thunk)) (t1 (current-jiffy))
               (ms (/ (- t1 t0) 1000000.0)))
          (loop (+ i 1) (min best ms))))))

;; ── matrix helpers ─────────────────────────────────────────────
(define (mat->rowvec M)
  (list->vector (map list->vector (semiring-matrix->rows M))))

(define (transpose-rowvec rv)
  (let* ((n (vector-length rv)) (m (vector-length (vector-ref rv 0)))
         (out (make-vector m)))
    (let loopj ((j 0))
      (if (= j m) out
          (let ((col (make-vector n)))
            (let loopi ((i 0))
              (when (< i n)
                (vector-set! col i (vector-ref (vector-ref rv i) j))
                (loopi (+ i 1))))
            (vector-set! out j col)
            (loopj (+ j 1)))))))

(define (dot S z a b)
  (let ((k (vector-length a)))
    (let loop ((p 0) (acc z))
      (if (= p k) acc
          (loop (+ p 1)
                (semiring-plus S acc
                  (semiring-times S (vector-ref a p) (vector-ref b p))))))))

(define (mul-block S z Arows Bcols lo hi)
  (let loop ((i (- hi 1)) (acc '()))
    (if (< i lo) acc
        (loop (- i 1)
              (cons (let ((arow (vector-ref Arows i)))
                      (map (lambda (bcol) (dot S z arow bcol)) (vector->list Bcols)))
                    acc)))))

;; Row-partitioned fork-join multiply. nthreads=1 is the serial baseline of the
;; SAME algorithm, so speedup = p1/pN measures parallelism in isolation.
(define (parallel-mul S A B nthreads)
  (let* ((z (semiring-zero S))
         (Arows (mat->rowvec A))
         (Bcols (transpose-rowvec (mat->rowvec B)))
         (n (vector-length Arows))
         (chunk (quotient (+ n nthreads -1) nthreads)))
    (let loop ((t 0) (threads '()))
      (if (>= (* t chunk) n)
          (let jloop ((ths (reverse threads)) (rows '()))
            (if (null? ths)
                (semiring-matrix-from-rows S (apply append (reverse rows)))
                (jloop (cdr ths) (cons (thread-join! (car ths)) rows))))
          (let* ((lo (* t chunk)) (hi (min n (+ lo chunk)))
                 (th (make-thread (lambda () (mul-block S z Arows Bcols lo hi)))))
            (thread-start! th)
            (loop (+ t 1) (cons th threads)))))))

(define (gen S n base)
  (semiring-matrix-from-rows S
    (map (lambda (i)
           (map (lambda (j) (* base (+ 1 (modulo (+ (* i 31) (* j 17)) 97)))) (iota n)))
         (iota n))))

;; ── pure-compute control: parallelism with no matrix code at all ─
(define (work m)
  (let loop ((i 0) (acc 0)) (if (= i m) acc (loop (+ i 1) (+ acc (* i 3))))))
(define (serial-k k) (let loop ((i 0)) (when (< i k) (work 2000000) (loop (+ i 1)))))
(define (parallel-k k)
  (let ((ths (map (lambda (_) (let ((t (make-thread (lambda () (work 2000000)))))
                                (thread-start! t) t))
                  (iota k))))
    (for-each thread-join! ths)))

;; ── run ─────────────────────────────────────────────────────────
(define (bench-matrix n)
  (let* ((S (counting-semiring)) (A (gen S n 1)) (B (gen S n 1))
         (p1 (best-ms (lambda () (parallel-mul S A B 1)) 2))
         (p4 (best-ms (lambda () (parallel-mul S A B 4)) 2))
         (p8 (best-ms (lambda () (parallel-mul S A B 8)) 2))
         (p16 (best-ms (lambda () (parallel-mul S A B 16)) 2)))
    (display (list 'matrix-mul 'n n 'thr1 p1 'thr4 p4 'thr8 p8 'thr16 p16
                   'speedup-16t (/ p1 p16)))
    (newline)))

(display "correct? ")
(let* ((S (counting-semiring)) (A (gen S 23 1)) (B (gen S 23 1)))
  (display (equal? (semiring-matrix->rows (semiring-matrix-mul A B))
                   (semiring-matrix->rows (parallel-mul S A B 4)))))
(newline)
(bench-matrix 160)
(display (list 'pure-compute 'serial-8 (best-ms (lambda () (serial-k 8)) 2)
               'parallel-8 (best-ms (lambda () (parallel-k 8)) 2)))
(newline)
