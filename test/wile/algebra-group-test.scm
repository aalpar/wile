;;; algebra-group-test.scm — Group tests

(import (scheme base)
        (chibi test)
        (wile algebra monoid)
        (wile algebra group))

(test-begin "groups")

(define int-add-group (make-group + 0 -))

(test-group "construction"
  (test #t (group? int-add-group))
  (test #f (group? 42)))

(test-group "operations"
  (test 5  (group-op int-add-group 2 3))
  (test 0  (group-identity int-add-group))
  (test -3 (group-inverse int-add-group 3)))

(test-group "group->monoid"
  (let ((m (group->monoid int-add-group)))
    (test #t (monoid? m))
    (test 0  (monoid-identity m))
    (test 5  (monoid-op m 2 3))
    ;; fold works
    (test 10 (monoid-fold m '(1 2 3 4)))))

(test-group "with-group"
  (test 0 (with-group int-add-group (op identity inverse)
            (op 3 (inverse 3)))))

(test-group "validate-group"
  (test #t (validate-group int-add-group '(-2 -1 0 1 2))))

(test-end)
(test-exit)
