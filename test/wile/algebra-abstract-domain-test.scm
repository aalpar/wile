;;; algebra-abstract-domain-test.scm — (wile algebra abstract-domain)

(import (scheme base)
        (chibi test)
        (wile algebra lattice)
        (wile algebra abstract-domain))

(test-begin "abstract-domain")

;;; --- sign-lattice --------------------------------------------------------

(test-group "sign-lattice — construction"
  (test #t (lattice? (sign-lattice)))
  (test 'flat-bottom (lattice-bottom (sign-lattice)))
  (test 'flat-top    (lattice-top (sign-lattice))))

(test-group "sign-lattice — ordering"
  (define L (sign-lattice))
  (test #t (lattice-leq? L 'flat-bottom 'neg))
  (test #t (lattice-leq? L 'flat-bottom 'zero))
  (test #t (lattice-leq? L 'flat-bottom 'pos))
  (test #t (lattice-leq? L 'neg 'flat-top))
  (test #t (lattice-leq? L 'zero 'flat-top))
  (test #t (lattice-leq? L 'pos 'flat-top))
  (test #f (lattice-leq? L 'neg 'zero))
  (test #f (lattice-leq? L 'zero 'pos))
  (test #f (lattice-leq? L 'pos 'neg)))

(test-group "sign-lattice — join/meet"
  (define L (sign-lattice))
  (test 'flat-top    (lattice-join L 'neg 'pos))
  (test 'flat-bottom (lattice-meet L 'neg 'pos))
  (test 'neg         (lattice-join L 'neg 'neg))
  (test 'pos         (lattice-join L 'flat-bottom 'pos)))

;;; --- abstract-sign -------------------------------------------------------

(test-group "abstract-sign"
  (test 'neg  (abstract-sign -5))
  (test 'zero (abstract-sign 0))
  (test 'pos  (abstract-sign 7))
  (test 'pos  (abstract-sign 1))
  (test 'neg  (abstract-sign -1))
  (test 'pos  (abstract-sign 1000000)))

;;; --- sign-binop ----------------------------------------------------------

(test-group "sign-binop — strictness + annihilation"
  (test 'flat-bottom (sign-binop 'add 'flat-bottom 'pos))
  (test 'flat-bottom (sign-binop 'add 'neg 'flat-bottom))
  (test 'zero        (sign-binop 'mul 'zero 'flat-top))
  (test 'zero        (sign-binop 'mul 'flat-top 'zero))
  (test 'flat-top    (sign-binop 'add 'flat-top 'pos))
  (test 'flat-top    (sign-binop 'div 'pos 'pos)))

(test-group "sign-binop — add table"
  (test 'neg      (sign-binop 'add 'neg 'neg))
  (test 'neg      (sign-binop 'add 'neg 'zero))
  (test 'flat-top (sign-binop 'add 'neg 'pos))
  (test 'pos      (sign-binop 'add 'pos 'pos))
  (test 'zero     (sign-binop 'add 'zero 'zero))
  (test 'pos      (sign-binop 'add 'zero 'pos)))

(test-group "sign-binop — sub table"
  (test 'flat-top (sign-binop 'sub 'neg 'neg))
  (test 'neg      (sign-binop 'sub 'zero 'pos))
  (test 'pos      (sign-binop 'sub 'pos 'neg))
  (test 'flat-top (sign-binop 'sub 'pos 'pos)))

(test-group "sign-binop — mul table"
  (test 'pos      (sign-binop 'mul 'neg 'neg))
  (test 'neg      (sign-binop 'mul 'neg 'pos))
  (test 'pos      (sign-binop 'mul 'pos 'pos))
  (test 'zero     (sign-binop 'mul 'neg 'zero))
  (test 'zero     (sign-binop 'mul 'zero 'pos)))

(test-end)
(test-exit)
