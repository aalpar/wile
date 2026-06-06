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

(test-group "canary — CFL is more precise than Boolean reachability"
  ;; Procedure p reached from two call sites.
  ;;   caller A:  a1 --call1--> p --return1--> a2
  ;;   caller B:  b1 --call2--> p --return2--> b2
  (define G
    (make-cfl-graph
      '(a1 a2 b1 b2 p)
      '((a1 call1 p) (p return1 a2)
        (b1 call2 p) (p return2 b2))))
  ;; Dyck grammar for two bracket pairs, hand-built:
  ;;   S -> eps | S S | O1 T1 | O2 T2
  ;;   T1 -> S C1 ;  T2 -> S C2
  ;;   O1 -> call1 ; C1 -> return1 ; O2 -> call2 ; C2 -> return2
  (define dyck
    (make-cfl-grammar 'S
      (list (cfl-epsilon 'S)
            (cfl-binary 'S 'S 'S)
            (cfl-binary 'S 'O1 'T1) (cfl-binary 'T1 'S 'C1)
            (cfl-binary 'S 'O2 'T2) (cfl-binary 'T2 'S 'C2)
            (cfl-terminal 'O1 'call1)   (cfl-terminal 'C1 'return1)
            (cfl-terminal 'O2 'call2)   (cfl-terminal 'C2 'return2))))
  (define sol (cfl-solve dyck G))
  ;; Matched (interprocedurally feasible) paths ARE start-reachable:
  (test #t (cfl-reachable? sol 'a1 'a2))
  (test #t (cfl-reachable? sol 'b1 'b2))
  ;; THE CANARY: mismatched brackets. A directed path a1->p->b2 EXISTS (so
  ;; Boolean reachability would say #t), but call1/return2 do not balance, so
  ;; CFL reachability is precise and returns #f:
  (test #f (cfl-reachable? sol 'a1 'b2))
  (test #f (cfl-reachable? sol 'b1 'a2))
  ;; Reflexive: epsilon makes every node S-reach itself.
  (test #t (cfl-reachable? sol 'p 'p))
  ;; from / pairs / derives? surfaces:
  (test #t (and (member 'a2 (cfl-reachable-from sol 'a1)) #t))
  (test #f (and (member 'b2 (cfl-reachable-from sol 'a1)) #t))
  (test #t (cfl-derives? sol 'a1 'S 'a2)))

(test-end)
(test-exit)
