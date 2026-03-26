(define-library (scheme r5rs)
  (import (scheme base)
          (scheme complex)
          (scheme cxr)
          (scheme read)
          (scheme write))

  (export
    eq?
    eqv?
    equal?
    boolean?
    not
    pair?
    cons
    car
    cdr
    caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
    null?
    list?
    list
    length
    append
    reverse
    list-tail
    list-ref
    memq
    memv
    member
    assq
    assv
    assoc
    symbol?
    symbol->string
    string->symbol
    number?
    complex?
    real?
    rational?
    integer?
    exact?
    inexact?
    = < > <= >=
    zero?
    positive?
    negative?
    odd?
    even?
    max
    min
    + - * /
    abs
    quotient
    remainder
    modulo
    gcd
    lcm
    numerator
    denominator
    floor
    ceiling
    truncate
    round
    rationalize
    expt
    number->string
    string->number
    make-rectangular
    make-polar
    real-part
    imag-part
    magnitude
    angle
    char?
    char=?
    char<?
    char>?
    char<=?
    char>=?
    char->integer
    integer->char
    string?
    string-length
    string-ref
    string=?
    string<?
    string>?
    string<=?
    string>=?
    substring
    string-append
    string->list
    list->string
    vector?
    make-vector
    vector
    vector-length
    vector-ref
    vector-set!
    vector->list
    list->vector
    procedure?
    apply
    map
    for-each
    call-with-current-continuation
    call/cc
    values
    call-with-values
    dynamic-wind
    current-input-port
    current-output-port
    read
    newline
    write
    display))
