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

(test-end "matching")
