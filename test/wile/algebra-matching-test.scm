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
    (test #f (eq? #t (validate-bipartite-matching M '(a b) '(x y))))))

(test-end "matching")
