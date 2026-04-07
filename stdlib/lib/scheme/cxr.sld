(define-library (scheme cxr)
  (description "Compositions of car and cdr up to four deep: caar, cadr, ..., cddddr.")
  (export
    caar cadr cdar cddr
    caaar caadr cadar caddr
    cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr
    cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr
    cddaar cddadr cdddar cddddr)

  (begin
    (define (caar x)
      "Return (car (car X)).\nExtract the car of the car of a pair.\n\nExamples:\n  (caar '((1 2) 3))  => 1\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (car x)))
    (define (cadr x)
      "Return (car (cdr X)).\nExtract the second element of a list.\n\nExamples:\n  (cadr '(1 2 3))  => 2\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (cdr x)))
    (define (cdar x)
      "Return (cdr (car X)).\nExtract the cdr of the car of a pair.\n\nExamples:\n  (cdar '((1 2 3) 4))  => (2 3)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (car x)))
    (define (cddr x)
      "Return (cdr (cdr X)).\nDrop the first two elements of a list.\n\nExamples:\n  (cddr '(1 2 3 4))  => (3 4)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (cdr x)))

    (define (caaar x)
      "Return (car (car (car X))).\nThree levels deep: car of car of car.\n\nExamples:\n  (caaar '(((1 2) 3) 4))  => 1\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (car (car x))))
    (define (caadr x)
      "Return (car (car (cdr X))).\nCar of car of the tail of X.\n\nExamples:\n  (caadr '(1 (2 3) 4))  => 2\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (car (cdr x))))
    (define (cadar x)
      "Return (car (cdr (car X))).\nSecond element of the car of X.\n\nExamples:\n  (cadar '((1 2 3) 4))  => 2\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (cdr (car x))))
    (define (caddr x)
      "Return (car (cdr (cdr X))).\nExtract the third element of a list.\n\nExamples:\n  (caddr '(1 2 3 4))  => 3\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (cdr (cdr x))))
    (define (cdaar x)
      "Return (cdr (car (car X))).\nCdr of car of car of X.\n\nExamples:\n  (cdaar '(((1 2 3) 4) 5))  => (2 3)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (car (car x))))
    (define (cdadr x)
      "Return (cdr (car (cdr X))).\nCdr of the second element of X.\n\nExamples:\n  (cdadr '(1 (2 3 4) 5))  => (3 4)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (car (cdr x))))
    (define (cddar x)
      "Return (cdr (cdr (car X))).\nDrop two elements from the car of X.\n\nExamples:\n  (cddar '((1 2 3 4) 5))  => (3 4)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (cdr (car x))))
    (define (cdddr x)
      "Return (cdr (cdr (cdr X))).\nDrop the first three elements of a list.\n\nExamples:\n  (cdddr '(1 2 3 4 5))  => (4 5)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (cdr (cdr x))))

    (define (caaaar x)
      "Return (car (car (car (car X)))).\nFour levels deep: car composed four times.\n\nExamples:\n  (caaaar '((((1)))))  => 1\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (car (car (car x)))))
    (define (caaadr x)
      "Return (car (car (car (cdr X)))).\nCar three times into the tail of X.\n\nExamples:\n  (caaadr '(0 ((1))))  => 1\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (car (car (cdr x)))))
    (define (caadar x)
      "Return (car (car (cdr (car X)))).\nCar of car of second element of car of X.\n\nExamples:\n  (caadar '((0 (1)) 2))  => 1\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (car (cdr (car x)))))
    (define (caaddr x)
      "Return (car (car (cdr (cdr X)))).\nCar of car of the third tail element of X.\n\nExamples:\n  (caaddr '(0 1 (2 3)))  => 2\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (car (cdr (cdr x)))))
    (define (cadaar x)
      "Return (car (cdr (car (car X)))).\nSecond element of car of car of X.\n\nExamples:\n  (cadaar '(((1 2) 3) 4))  => 2\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (cdr (car (car x)))))
    (define (cadadr x)
      "Return (car (cdr (car (cdr X)))).\nSecond element of the second element of X.\n\nExamples:\n  (cadadr '(0 (1 2 3)))  => 2\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (cdr (car (cdr x)))))
    (define (caddar x)
      "Return (car (cdr (cdr (car X)))).\nThird element of the car of X.\n\nExamples:\n  (caddar '((1 2 3 4) 5))  => 3\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (cdr (cdr (car x)))))
    (define (cadddr x)
      "Return (car (cdr (cdr (cdr X)))).\nExtract the fourth element of a list.\n\nExamples:\n  (cadddr '(1 2 3 4 5))  => 4\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (car (cdr (cdr (cdr x)))))
    (define (cdaaar x)
      "Return (cdr (car (car (car X)))).\nCdr of car of car of car of X.\n\nExamples:\n  (cdaaar '((((1 2 3)))))  => (2 3)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (car (car (car x)))))
    (define (cdaadr x)
      "Return (cdr (car (car (cdr X)))).\nCdr of car of car of the tail of X.\n\nExamples:\n  (cdaadr '(0 ((1 2 3))))  => (2 3)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (car (car (cdr x)))))
    (define (cdadar x)
      "Return (cdr (car (cdr (car X)))).\nCdr of second element of the car of X.\n\nExamples:\n  (cdadar '((0 (1 2 3)) 4))  => (2 3)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (car (cdr (car x)))))
    (define (cdaddr x)
      "Return (cdr (car (cdr (cdr X)))).\nCdr of the third element of a list.\n\nExamples:\n  (cdaddr '(0 1 (2 3 4)))  => (3 4)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (car (cdr (cdr x)))))
    (define (cddaar x)
      "Return (cdr (cdr (car (car X)))).\nDrop two from car of car of X.\n\nExamples:\n  (cddaar '(((1 2 3 4)) 5))  => (3 4)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (cdr (car (car x)))))
    (define (cddadr x)
      "Return (cdr (cdr (car (cdr X)))).\nDrop two from the second element of X.\n\nExamples:\n  (cddadr '(0 (1 2 3 4)))  => (3 4)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (cdr (car (cdr x)))))
    (define (cdddar x)
      "Return (cdr (cdr (cdr (car X)))).\nDrop three elements from the car of X.\n\nExamples:\n  (cdddar '((1 2 3 4 5) 6))  => (4 5)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (cdr (cdr (car x)))))
    (define (cddddr x)
      "Return (cdr (cdr (cdr (cdr X)))).\nDrop the first four elements of a list.\n\nExamples:\n  (cddddr '(1 2 3 4 5 6))  => (5 6)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
      (cdr (cdr (cdr (cdr x)))))))
