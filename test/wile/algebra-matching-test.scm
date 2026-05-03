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
    (test #f (eq? #t (validate-preference-profile bad-out-of-set '(x y)))))
  (let ((bad-tied (make-preference-profile
                    '(a)
                    (lambda (x) '(x x)))))
    (test #f (eq? #t (validate-preference-profile bad-tied '(x y))))))

(test-end "matching")
