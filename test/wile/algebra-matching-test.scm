;;; algebra-matching-test.scm — Two-sided matching tests

(import (scheme base)
        (chibi test)
        (wile algebra matching))

(test-begin "matching")

(test-group "preference-profile construction"
  (let ((P (make-preference-profile
             '(a b c)
             (lambda (agent)
               (case agent
                 ((a) '(x y z))
                 ((b) '(y x z))
                 ((c) '(z x y)))))))
    (test #t (preference-profile? P))
    (test '(a b c) (preference-profile-agents P))
    (test '(x y z) ((preference-profile-ranks-of P) 'a))
    (test '(z x y) ((preference-profile-ranks-of P) 'c))))

(test-group "preference-profile-rank-of and prefers-strictly?"
  (let ((P (make-preference-profile
             '(a b c)
             (lambda (agent)
               (case agent
                 ((a) '(x y z))
                 ((b) '(y x z))
                 ((c) '(z x y)))))))
    (test 1 (preference-profile-rank-of P 'a 'x))
    (test 2 (preference-profile-rank-of P 'a 'y))
    (test 3 (preference-profile-rank-of P 'a 'z))
    (test #t (preference-profile-prefers-strictly? P 'a 'x 'y))
    (test #f (preference-profile-prefers-strictly? P 'a 'y 'x))
    (test #f (preference-profile-prefers-strictly? P 'a 'x 'x))
    (test #f (preference-profile-rank-of P 'a 'w))
    (test #f (preference-profile-prefers-strictly? P 'a 'x 'w))
    (test #f (preference-profile-prefers-strictly? P 'a 'w 'x))))

(test-group "validate-preference-profile"
  (let ((good (make-preference-profile
                '(a b)
                (lambda (x) (case x ((a) '(y x)) ((b) '(x y)))))))
    (test #t (validate-preference-profile good '(x y))))
  (let ((bad-out-of-set (make-preference-profile
                          '(a)
                          (lambda (x) '(z)))))
    (test '((preference-out-of-set a z))
          (validate-preference-profile bad-out-of-set '(x y))))
  (let ((bad-tied (make-preference-profile
                    '(a)
                    (lambda (x) '(x x)))))
    (test '((tied-preference a x))
          (validate-preference-profile bad-tied '(x y)))))

(test-group "bipartite-matching construction"
  (let ((M (make-bipartite-matching '((a . x) (b . y)))))
    (test #t (bipartite-matching? M))
    (test '((a . x) (b . y)) (bipartite-matching-pairs M))))

(test-group "bipartite-matching partner and unmatched"
  (let ((M (make-bipartite-matching '((a . x) (b . y)))))
    (test 'x (bipartite-matching-partner M 'a))
    (test 'a (bipartite-matching-partner M 'x))
    (test #f (bipartite-matching-partner M 'c))
    (test '(c) (bipartite-matching-unmatched M 'proposer '(a b c)))
    (test '(z) (bipartite-matching-unmatched M 'receiver '(x y z)))))

(test-group "bipartite-matching equality and validation"
  (let ((M1 (make-bipartite-matching '((a . x) (b . y))))
        (M2 (make-bipartite-matching '((b . y) (a . x))))
        (M3 (make-bipartite-matching '((a . y) (b . x)))))
    (test #t (bipartite-matching-equal? M1 M2))
    (test #f (bipartite-matching-equal? M1 M3)))
  (let ((M (make-bipartite-matching '((a . x) (b . x)))))
    (test '((receiver-matched-twice x))
          (validate-bipartite-matching M '(a b) '(x y)))))

(test-group "blocking-pairs and stable?"
  (let* ((prop-prefs (make-preference-profile
                       '(a b) (lambda (x) (case x ((a) '(y x)) ((b) '(x y))))))
         (recv-prefs (make-preference-profile
                       '(x y) (lambda (x) (case x ((x) '(a b)) ((y) '(a b))))))
         (M-stable (make-bipartite-matching '((a . y) (b . x))))
         (M-unstable (make-bipartite-matching '((a . x) (b . y)))))
    (test '() (blocking-pairs M-stable prop-prefs recv-prefs))
    (test #t (stable? M-stable prop-prefs recv-prefs))
    (test #f (null? (blocking-pairs M-unstable prop-prefs recv-prefs)))
    (test #f (stable? M-unstable prop-prefs recv-prefs))))

(test-group "with-X macros"
  (let ((P (make-preference-profile '(a) (lambda (x) '(y)))))
    (with-preference-profile P (agents ranks-of)
      (test '(a) agents)
      (test '(y) (ranks-of 'a))))
  (let ((M (make-bipartite-matching '((a . x)))))
    (with-bipartite-matching M (pairs)
      (test '((a . x)) pairs))))

(test-group "gale-shapley proposer-optimal — textbook 4x4"
  (let* ((mp (make-preference-profile
               '(1 2 3 4)
               (lambda (m)
                 (case m
                   ((1) '(a b c d))
                   ((2) '(b a c d))
                   ((3) '(a c b d))
                   ((4) '(c a b d))))))
         (wp (make-preference-profile
               '(a b c d)
               (lambda (w)
                 (case w
                   ((a) '(2 4 1 3))
                   ((b) '(3 1 2 4))
                   ((c) '(2 3 4 1))
                   ((d) '(4 1 3 2))))))
         (M (gale-shapley mp wp)))
    (test #t (bipartite-matching? M))
    (test #t (stable? M mp wp))
    (test 4 (length (bipartite-matching-pairs M)))))

(test-group "gale-shapley/receiver-optimal asymmetry"
  (let* ((mp (make-preference-profile
               '(1 2)
               (lambda (m) (case m ((1) '(a b)) ((2) '(b a))))))
         (wp (make-preference-profile
               '(a b)
               (lambda (w) (case w ((a) '(2 1)) ((b) '(1 2))))))
         (M-prop (gale-shapley mp wp))
         (M-recv (gale-shapley/receiver-optimal mp wp)))
    (test #t (stable? M-prop mp wp))
    (test #t (stable? M-recv mp wp))
    (test #f (bipartite-matching-equal? M-prop M-recv))))

(test-group "gale-shapley edge cases"
  ;; Three proposers, two receivers — one proposer ends unmatched
  (let* ((mp (make-preference-profile
               '(1 2 3)
               (lambda (m) '(a b))))
         (wp (make-preference-profile
               '(a b)
               (lambda (w) '(1 2 3))))
         (M (gale-shapley mp wp)))
    (test #t (stable? M mp wp))
    (test 2 (length (bipartite-matching-pairs M)))
    (test '(3) (bipartite-matching-unmatched M 'proposer '(1 2 3))))
  ;; Empty preference list — proposer can never match
  (let* ((mp (make-preference-profile
               '(1 2)
               (lambda (m) (case m ((1) '()) ((2) '(a))))))
         (wp (make-preference-profile
               '(a)
               (lambda (w) '(2 1))))
         (M (gale-shapley mp wp)))
    (test 1 (length (bipartite-matching-pairs M)))
    (test '(1) (bipartite-matching-unmatched M 'proposer '(1 2)))))

(test-group "gale-shapley property: 50 random profiles, all stable"
  (define (random-perm n trial)
    ;; Deterministic pseudo-random shuffle using Park-Miller-style MCG.
    ;; Different `trial` values produce different permutations.
    (let* ((vec (let loop ((i 0) (acc '()))
                  (if (>= i n) (list->vector (reverse acc))
                      (loop (+ i 1) (cons i acc))))))
      (do ((i (- n 1) (- i 1)))
          ((<= i 0))
        (let* ((seed (modulo (+ (* (+ trial 1) 2654435761)
                                (* (+ i 1) 1597334677))
                             2147483647))
               (j (modulo seed (+ i 1)))
               (tmp (vector-ref vec i)))
          (vector-set! vec i (vector-ref vec j))
          (vector-set! vec j tmp)))
      (vector->list vec)))
  ;; Build symbol tables for proposer and receiver agents
  (define p-agents (let loop ((i 0) (acc '()))
                     (if (>= i 5) (reverse acc)
                         (loop (+ i 1) (cons (string->symbol (string-append "p" (number->string i))) acc)))))
  (define r-agents (let loop ((i 0) (acc '()))
                     (if (>= i 5) (reverse acc)
                         (loop (+ i 1) (cons (string->symbol (string-append "r" (number->string i))) acc)))))
  (define (p-agent-index agent)
    (let loop ((i 0) (agents p-agents))
      (cond ((null? agents) #f)
            ((eq? agent (car agents)) i)
            (else (loop (+ i 1) (cdr agents))))))
  (define (r-agent-index agent)
    (let loop ((i 0) (agents r-agents))
      (cond ((null? agents) #f)
            ((eq? agent (car agents)) i)
            (else (loop (+ i 1) (cdr agents))))))
  (let ((n 5))
    (do ((trial 0 (+ trial 1))) ((>= trial 50))
      (let* ((mp (make-preference-profile
                   p-agents
                   (lambda (p)
                     (let ((idx (p-agent-index p)))
                       (map (lambda (i) (list-ref r-agents i))
                            (random-perm n (+ trial (* idx 17))))))))
             (wp (make-preference-profile
                   r-agents
                   (lambda (r)
                     (let ((idx (r-agent-index r)))
                       (map (lambda (i) (list-ref p-agents i))
                            (random-perm n (+ trial (* idx 23) 7)))))))
             (M (gale-shapley mp wp)))
        (test-assert (stable? M mp wp))))))

(test-end "matching")
