;;; algebra-cfl-test.scm — (wile algebra cfl)

(import (scheme base)
        (chibi test)
        (wile algebra cfl))

(test-begin "cfl")

(test-group "productions — typed kernels"
  (test 'epsilon  (cfl-production-kind (cfl-epsilon 'S)))
  (test 'S        (cfl-production-lhs  (cfl-epsilon 'S)))
  (test 'terminal (cfl-production-kind (cfl-terminal 'O 'open)))
  (test 'open     (cfl-production-rhs1 (cfl-terminal 'O 'open)))
  (test 'unary    (cfl-production-kind (cfl-unary 'A 'B)))
  (test 'binary   (cfl-production-kind (cfl-binary 'S 'A 'B)))
  (test 'B        (cfl-production-rhs2 (cfl-binary 'S 'A 'B)))
  (test #t        (cfl-production? (cfl-epsilon 'S))))

(test-group "grammar + graph records"
  (define g (make-cfl-grammar 'S (list (cfl-epsilon 'S) (cfl-terminal 'S 'a))))
  (test #t  (cfl-grammar? g))
  (test 'S  (cfl-grammar-start g))
  (test 2   (length (cfl-grammar-productions g)))
  (define G (make-cfl-graph '(n0 n1) '((n0 a n1))))
  (test #t  (cfl-graph? G))
  (test '(n0 n1)     (cfl-graph-nodes G))
  (test '((n0 a n1)) (cfl-graph-edges G)))

(test-end)
(test-exit)
