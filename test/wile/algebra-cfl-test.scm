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

(test-group "dyck-grammar preset"
  (define G
    (make-cfl-graph '(a1 a2 b2 p)
      '((a1 call1 p) (p return1 a2) (p return2 b2))))
  (define sol (cfl-solve (dyck-grammar '((call1 . return1) (call2 . return2))) G))
  (test #t (cfl-reachable? sol 'a1 'a2))    ; matched
  (test #f (cfl-reachable? sol 'a1 'b2))    ; mismatched
  ;; nested: [[ ]] over a line graph
  (define Gn
    (make-cfl-graph '(x0 x1 x2 x3 x4)
      '((x0 open x1) (x1 open x2) (x2 close x3) (x3 close x4))))
  (define soln (cfl-solve (dyck-grammar '((open . close))) Gn))
  (test #t (cfl-reachable? soln 'x0 'x4))   ; open open close close — balanced
  (test #f (cfl-reachable? soln 'x0 'x3)))  ; open open close — unbalanced

(test-group "validation"
  ;; well-formed grammar/graph -> #t
  (define ok-g (make-cfl-grammar 'S (list (cfl-epsilon 'S) (cfl-terminal 'S 'a))))
  (test #t (validate-cfl-grammar ok-g))
  (test #t (validate-cfl-graph (make-cfl-graph '(n0 n1) '((n0 a n1)))))
  ;; terminal/nonterminal collision: 'a is both a terminal and a nonterminal LHS
  (define bad-g
    (make-cfl-grammar 'S (list (cfl-terminal 'S 'a) (cfl-epsilon 'a))))
  ;; assert the SPECIFIC violation, not merely that some list came back
  (test #t (and (member '(terminal-nonterminal-collision a)
                        (validate-cfl-grammar bad-g)) #t))
  ;; start with no production
  (test #t (and (member '(start-undefined Q)
                        (validate-cfl-grammar (make-cfl-grammar 'Q (list (cfl-epsilon 'S))))) #t))
  ;; binary RHS that is not a nonterminal (undefined symbol Z)
  (test #t (and (member '(rhs-not-nonterminal Z)
                        (validate-cfl-grammar
                          (make-cfl-grammar 'S (list (cfl-binary 'S 'Z 'S) (cfl-epsilon 'S))))) #t))
  ;; edge to undeclared node
  (test #t (and (member '(edge-to-undeclared n1)
                        (validate-cfl-graph (make-cfl-graph '(n0) '((n0 a n1))))) #t)))

(test-group "general (non-Dyck) grammar — even-length a-paths"
  ;; S -> eps | P S ;  P -> A A ;  A -> a
  ;; S derives exactly the paths whose a-edge count is even.
  (define even-a
    (make-cfl-grammar 'S
      (list (cfl-epsilon 'S) (cfl-binary 'S 'P 'S) (cfl-binary 'P 'A 'A) (cfl-terminal 'A 'a))))
  (define G (make-cfl-graph '(n0 n1 n2 n3) '((n0 a n1) (n1 a n2) (n2 a n3))))
  (define sol (cfl-solve even-a G))
  (test #t (cfl-reachable? sol 'n0 'n2))   ; two a-edges: even
  (test #f (cfl-reachable? sol 'n0 'n3))   ; three a-edges: odd
  (test #t (cfl-reachable? sol 'n0 'n0)))  ; zero: even (epsilon)

(test-group "error handling — fail-fast on malformed input and unknown nodes"
  ;; cfl-solve raises on an edge naming an undeclared node (n1 not declared)
  (test #t (guard (e (#t #t))
             (cfl-solve (make-cfl-grammar 'S (list (cfl-terminal 'S 'a)))
                        (make-cfl-graph '(n0) '((n0 a n1))))
             #f))
  ;; cfl-solve raises on a production RHS that is not a defined nonterminal (Z)
  (test #t (guard (e (#t #t))
             (cfl-solve (make-cfl-grammar 'S (list (cfl-binary 'S 'Z 'S) (cfl-epsilon 'S)))
                        (make-cfl-graph '(n0) '()))
             #f))
  ;; queries raise on an unknown node — a typo must not silently read as "not reachable"
  (define sol (cfl-solve (dyck-grammar '((call1 . return1)))
                         (make-cfl-graph '(a1 a2 p) '((a1 call1 p) (p return1 a2)))))
  (test #t (guard (e (#t #t)) (cfl-reachable? sol 'nope 'a2) #f))
  (test #t (guard (e (#t #t)) (cfl-reachable-from sol 'nope) #f))
  (test #t (guard (e (#t #t)) (cfl-derives? sol 'nope 'S 'a2) #f))
  ;; cfl-derives? keeps the tolerant #f for an unknown nonterminal (NOT a raise)
  (test #f (cfl-derives? sol 'a1 'no-such-nonterminal 'a2)))

(test-end)
(test-exit)
