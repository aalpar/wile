(define-library (scheme r5rs)
  (description "R5RS compatibility library: re-exports the bindings required by the Revised^5 Report on Scheme.")
  (import (scheme base)
          (scheme inexact)
          (scheme complex)
          (scheme char)
          (scheme cxr)
          (scheme read)
          (scheme write)
          (scheme lazy)
          (scheme eval)
          (scheme repl)
          (scheme load)
          (scheme file))

  ;; R5RS spells the exact/inexact converters exact->inexact and inexact->exact.
  ;; R7RS renamed them to inexact and exact; the long names live only in (scheme r5rs).
  ;; Defined here as aliases over the R7RS core converters imported from (scheme base).
  (begin
    (define exact->inexact inexact)
    (define inexact->exact exact))

  (export
    ;; Syntactic keywords (R5RS §7.1.3, §4.1-4.3)
    quote
    lambda
    if
    set!
    begin
    define
    define-syntax
    let-syntax
    letrec-syntax
    syntax-rules
    quasiquote
    unquote
    unquote-splicing
    cond
    case
    and
    or
    let
    let*
    letrec
    do
    delay
    else
    =>
    ...
    _
    ;; Equivalence predicates
    eq?
    eqv?
    equal?
    ;; Booleans
    boolean?
    not
    ;; Pairs and lists
    pair?
    cons
    car
    cdr
    set-car!
    set-cdr!
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
    ;; Symbols
    symbol?
    symbol->string
    string->symbol
    ;; Numbers
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
    exp
    log
    sin
    cos
    tan
    asin
    acos
    atan
    sqrt
    expt
    make-rectangular
    make-polar
    real-part
    imag-part
    magnitude
    angle
    exact->inexact
    inexact->exact
    number->string
    string->number
    ;; Characters
    char?
    char=?
    char<?
    char>?
    char<=?
    char>=?
    char-ci=?
    char-ci<?
    char-ci>?
    char-ci<=?
    char-ci>=?
    char-alphabetic?
    char-numeric?
    char-whitespace?
    char-upper-case?
    char-lower-case?
    char->integer
    integer->char
    char-upcase
    char-downcase
    ;; Strings
    string?
    make-string
    string
    string-length
    string-ref
    string-set!
    string=?
    string-ci=?
    string<?
    string>?
    string<=?
    string>=?
    string-ci<?
    string-ci>?
    string-ci<=?
    string-ci>=?
    substring
    string-append
    string->list
    list->string
    string-copy
    string-fill!
    ;; Vectors
    vector?
    make-vector
    vector
    vector-length
    vector-ref
    vector-set!
    vector->list
    list->vector
    vector-fill!
    ;; Control features
    procedure?
    apply
    map
    for-each
    force
    call-with-current-continuation
    call/cc
    values
    call-with-values
    dynamic-wind
    ;; Eval (R5RS §6.5)
    eval
    scheme-report-environment
    null-environment
    interaction-environment
    ;; Input and output (R5RS §6.6)
    call-with-input-file
    call-with-output-file
    input-port?
    output-port?
    current-input-port
    current-output-port
    with-input-from-file
    with-output-to-file
    open-input-file
    open-output-file
    close-input-port
    close-output-port
    read
    read-char
    peek-char
    eof-object?
    char-ready?
    write
    display
    newline
    write-char
    load))
