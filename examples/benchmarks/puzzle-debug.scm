;;; puzzle-debug.scm - Debug version to understand the error

(define (puzzle-iota n)
  (let loop ((i 0) (result '()))
    (if (>= i n)
        (reverse result)
        (loop (+ i 1) (cons i result)))))

(define (puzzle-remove x lst)
  (cond ((null? lst) '())
        ((equal? x (car lst)) (cdr lst))
        (else (cons (car lst) (puzzle-remove x (cdr lst))))))

(define (puzzle-choose lst)
  (if (null? lst)
      '(())
      (apply append
             (map (lambda (x)
                    (map (lambda (y) (cons x y))
                         (puzzle-choose (puzzle-remove x lst))))
                  lst))))

(define (puzzle-pieces)
  '((1 2) (2 3) (3 4)))

(display "Pieces: ")
(display (puzzle-pieces))
(newline)

(display "\nPermutations:\n")
(let ((perms (puzzle-choose (puzzle-pieces))))
  (for-each (lambda (p)
              (display "  ")
              (display p)
              (newline))
            perms))

(display "\nTrying to find: (1 2 3 4)\n")
(display "First permutation: ")
(display (car (puzzle-choose (puzzle-pieces))))
(newline)

(display "\nChecking types:\n")
(let* ((target '(1 2 3 4))
       (perm (car (puzzle-choose (puzzle-pieces)))))
  (display "target car: ")
  (display (car target))
  (display " (should be 1)\n")
  (display "perm car: ")
  (display (car perm))
  (display " (should be a list like (1 2))\n"))
