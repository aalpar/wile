;; microKanren — Hemann & Friedman (2013)
;; Adapted for R7RS (assp → assoc with custom comparator)

;; Logic variables: represented as single-element vectors
(define (var c)
  "Create a logic variable identified by the integer C.\nLogic variables are represented as single-element vectors.\nTwo variables are the same if they have the same identifier.\n\nExamples:\n  (var 0)  => #(0)\n  (var 3)  => #(3)\n\nParameters:\n  c : integer\nReturns: vector\nCategory: logic"
  (vector c))
(define (var? x)
  "Return #t if X is a logic variable (a vector), #f otherwise.\n\nExamples:\n  (var? (var 0))  => #t\n  (var? 'x)       => #f\n\nParameters:\n  x : any\nReturns: boolean\nCategory: logic"
  (vector? x))
(define (var=? x1 x2)
  "Return #t if logic variables X1 and X2 have the same identifier.\n\nExamples:\n  (var=? (var 0) (var 0))  => #t\n  (var=? (var 0) (var 1))  => #f\n\nParameters:\n  x1 : vector\n  x2 : vector\nReturns: boolean\nCategory: logic"
  (= (vector-ref x1 0) (vector-ref x2 0)))

;; Substitution: association list of (var . value) pairs
(define (walk u s)
  "Look up value U in substitution S, following chains of bindings.\nIf U is a logic variable bound in S, recursively walks its value\nuntil a non-variable or unbound variable is reached. Non-variables\nare returned unchanged.\n\nExamples:\n  (walk (var 0) '((#(0) . 5)))  => 5\n  (walk (var 0) '((#(0) . #(1)) (#(1) . 7)))  => 7\n  (walk 'a '())  => a\n\nParameters:\n  u : any\n  s : list\nReturns: any\nCategory: logic\n\nSee also: `ext-s', `unify'."
  (let ((pr (and (var? u) (assoc u s (lambda (a b) (var=? a b))))))
    (if pr (walk (cdr pr) s) u)))

(define (ext-s x v s)
  "Extend substitution S by associating logic variable X with value V.\nReturns a new substitution (association list) with the binding prepended.\n\nExamples:\n  (ext-s (var 0) 5 '())  => ((#(0) . 5))\n\nParameters:\n  x : vector\n  v : any\n  s : list\nReturns: list\nCategory: logic\n\nSee also: `walk', `unify'."
  (cons (cons x v) s))

;; Unification
(define (unify u v s)
  "Attempt to make U and V equal under substitution S.\nWalks both values to their current bindings, then extends S\nwith new associations as needed. Returns the extended substitution\non success, or #f if U and V cannot be made equal.\nHandles pairs recursively and uses eqv? for atoms.\n\nExamples:\n  (unify (var 0) 5 '())  => ((#(0) . 5))\n  (unify (var 0) (var 1) '())  => ((#(0) . #(1)))\n  (unify 3 4 '())  => #f\n\nParameters:\n  u : any\n  v : any\n  s : list\nReturns: any\nCategory: logic\n\nSee also: `walk', `ext-s', `=='."
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? u v)) s)
      ((var? u) (ext-s u v s))
      ((var? v) (ext-s v u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

;; Goals: state/counter → stream
(define (== u v)
  "Return a goal that unifies U and V.\nThe goal takes a state/counter pair and produces a singleton\nstream on success or the empty stream on failure.\n\nExamples:\n  ((== 5 5) empty-state)     => ((() . 0))\n  ((== 5 6) empty-state)     => ()\n\nParameters:\n  u : any\n  v : any\nReturns: procedure\nCategory: logic\n\nSee also: `unify', `unit', `mzero'."
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit (cons s (cdr s/c))) mzero))))

(define (unit s/c)
  "Wrap state/counter S/C in a singleton answer stream.\n\nExamples:\n  (unit empty-state)  => ((() . 0))\n\nParameters:\n  s/c : pair\nReturns: list\nCategory: logic"
  (cons s/c '()))
(define mzero '())

(define (call/fresh f)
  "Return a goal that allocates a fresh logic variable and passes\nit to F. F must be a one-argument procedure returning a goal.\nThe variable counter in the state is incremented.\n\nThe result is an answer stream (a list of states); each state is a\n(substitution . counter) pair, and a substitution is itself a list\nof (var . value) bindings -- hence the extra level of parentheses\naround the binding.\n\nExamples:\n  ((call/fresh (lambda (q) (== q 5))) empty-state)\n    => ((((#(0) . 5)) . 1))\n\nParameters:\n  f : procedure\nReturns: procedure\nCategory: logic\n\nSee also: `var', `=='."
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) (cons (car s/c) (+ c 1))))))

;; Goal combinators
(define (disj g1 g2)
  "Return a goal that succeeds if either G1 or G2 succeeds.\nInterleaves the answer streams from both goals to ensure\nfair enumeration of results.\n\nExamples:\n  ((disj (== 'x 1) (== 'x 2)) empty-state)  ; two answers\n\nParameters:\n  g1 : procedure\n  g2 : procedure\nReturns: procedure\nCategory: logic\n\nSee also: `conj', `mplus'."
  (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2)
  "Return a goal that succeeds when both G1 and G2 succeed.\nRuns G1 first, then threads each of its answers through G2\nvia bind.\n\nExamples:\n  ((call/fresh\n     (lambda (q) (conj (== q 5) (== q 5))))\n   empty-state)  => ((((#(0) . 5)) . 1))\n\nParameters:\n  g1 : procedure\n  g2 : procedure\nReturns: procedure\nCategory: logic\n\nSee also: `disj', `bind'."
  (lambda (s/c) (bind (g1 s/c) g2)))

;; Stream operations (interleaving search)
(define (mplus $1 $2)
  "Interleave two answer streams $1 and $2.\nIf $1 is empty, return $2. If $1 is a suspension (procedure),\nreturn a suspension that swaps the arguments, ensuring fair\nenumeration of both branches. Otherwise cons the first answer\nof $1 and interleave the rest with $2.\n\nExamples:\n  (mplus '(a b) '(1 2))  => (a b 1 2)\n  (mplus '() '(x y))  => (x y)\n\nParameters:\n  $1 : any\n  $2 : any\nReturns: any\nCategory: logic\n\nSee also: `bind', `disj'."
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  "Apply goal G to every answer in stream $ and merge the results.\nIf $ is empty, return the empty stream. If $ is a suspension,\nreturn a suspension that continues binding after forcing.\nOtherwise apply G to the first answer and interleave with\nthe rest via mplus.\n\nExamples:\n  (bind '() (== 'x 1))  => ()\n\nParameters:\n  $ : any\n  g : procedure\nReturns: any\nCategory: logic\n\nSee also: `mplus', `conj'."
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))

;; Initial state: empty substitution, counter at 0
(define empty-state (cons '() 0))
