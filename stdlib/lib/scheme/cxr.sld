(define-library (scheme cxr)
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
      "Return (car (car X)).\nExtract the car of the car of a pair."
      (car (car x)))
    (define (cadr x)
      "Return (car (cdr X)).\nExtract the second element of a list."
      (car (cdr x)))
    (define (cdar x)
      "Return (cdr (car X)).\nExtract the cdr of the car of a pair."
      (cdr (car x)))
    (define (cddr x)
      "Return (cdr (cdr X)).\nDrop the first two elements of a list."
      (cdr (cdr x)))

    (define (caaar x)
      "Return (car (car (car X))).\nThree levels deep: car of car of car."
      (car (car (car x))))
    (define (caadr x)
      "Return (car (car (cdr X))).\nCar of car of the tail of X."
      (car (car (cdr x))))
    (define (cadar x)
      "Return (car (cdr (car X))).\nSecond element of the car of X."
      (car (cdr (car x))))
    (define (caddr x)
      "Return (car (cdr (cdr X))).\nExtract the third element of a list."
      (car (cdr (cdr x))))
    (define (cdaar x)
      "Return (cdr (car (car X))).\nCdr of car of car of X."
      (cdr (car (car x))))
    (define (cdadr x)
      "Return (cdr (car (cdr X))).\nCdr of the second element of X."
      (cdr (car (cdr x))))
    (define (cddar x)
      "Return (cdr (cdr (car X))).\nDrop two elements from the car of X."
      (cdr (cdr (car x))))
    (define (cdddr x)
      "Return (cdr (cdr (cdr X))).\nDrop the first three elements of a list."
      (cdr (cdr (cdr x))))

    (define (caaaar x)
      "Return (car (car (car (car X)))).\nFour levels deep: car composed four times."
      (car (car (car (car x)))))
    (define (caaadr x)
      "Return (car (car (car (cdr X)))).\nCar three times into the tail of X."
      (car (car (car (cdr x)))))
    (define (caadar x)
      "Return (car (car (cdr (car X)))).\nCar of car of second element of car of X."
      (car (car (cdr (car x)))))
    (define (caaddr x)
      "Return (car (car (cdr (cdr X)))).\nCar of car of the third tail element of X."
      (car (car (cdr (cdr x)))))
    (define (cadaar x)
      "Return (car (cdr (car (car X)))).\nSecond element of car of car of X."
      (car (cdr (car (car x)))))
    (define (cadadr x)
      "Return (car (cdr (car (cdr X)))).\nSecond element of the second element of X."
      (car (cdr (car (cdr x)))))
    (define (caddar x)
      "Return (car (cdr (cdr (car X)))).\nThird element of the car of X."
      (car (cdr (cdr (car x)))))
    (define (cadddr x)
      "Return (car (cdr (cdr (cdr X)))).\nExtract the fourth element of a list."
      (car (cdr (cdr (cdr x)))))
    (define (cdaaar x)
      "Return (cdr (car (car (car X)))).\nCdr of car of car of car of X."
      (cdr (car (car (car x)))))
    (define (cdaadr x)
      "Return (cdr (car (car (cdr X)))).\nCdr of car of car of the tail of X."
      (cdr (car (car (cdr x)))))
    (define (cdadar x)
      "Return (cdr (car (cdr (car X)))).\nCdr of second element of the car of X."
      (cdr (car (cdr (car x)))))
    (define (cdaddr x)
      "Return (cdr (car (cdr (cdr X)))).\nCdr of the third element of a list."
      (cdr (car (cdr (cdr x)))))
    (define (cddaar x)
      "Return (cdr (cdr (car (car X)))).\nDrop two from car of car of X."
      (cdr (cdr (car (car x)))))
    (define (cddadr x)
      "Return (cdr (cdr (car (cdr X)))).\nDrop two from the second element of X."
      (cdr (cdr (car (cdr x)))))
    (define (cdddar x)
      "Return (cdr (cdr (cdr (car X)))).\nDrop three elements from the car of X."
      (cdr (cdr (cdr (car x)))))
    (define (cddddr x)
      "Return (cdr (cdr (cdr (cdr X)))).\nDrop the first four elements of a list."
      (cdr (cdr (cdr (cdr x)))))))
