(define-library (scheme base)
  (export
    ;; Special forms
    quote
    lambda
    if
    set!
    begin
    define
    define-syntax
    quasiquote
    unquote
    unquote-splicing
    cond-expand
    case-lambda
    include
    include-ci
    ;; Auxiliary syntax (R7RS §5.1)
    else
    =>
    ;; Derived syntax (bootstrap macros)
    cond
    case
    and
    or
    when
    unless
    let
    let*
    letrec
    letrec*
    let-values
    let*-values
    do
    parameterize
    guard
    delay
    delay-force
    define-record-type
    ;; Equivalence predicates
    eq?
    eqv?
    equal?
    boolean?
    boolean=?
    not
    ;; Pairs and lists
    pair?
    cons
    car
    cdr
    set-car!
    set-cdr!
    caar cadr cdar cddr
    null?
    list?
    list
    make-list
    length
    append
    reverse
    list-tail
    list-ref
    list-set!
    list-copy
    memq
    memv
    member
    assq
    assv
    assoc
    ;; Symbols
    symbol?
    symbol=?
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
    exact-integer?
    finite?
    infinite?
    nan?
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
    floor/
    floor-quotient
    floor-remainder
    truncate/
    truncate-quotient
    truncate-remainder
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
    square
    exact-integer-sqrt
    expt
    exact
    inexact
    number->string
    string->number
    ;; Characters
    char?
    char=?
    char<?
    char>?
    char<=?
    char>=?
    char->integer
    integer->char
    ;; Strings
    string?
    make-string
    string
    string-length
    string-ref
    string-set!
    string=?
    string<?
    string>?
    string<=?
    string>=?
    substring
    string-append
    string-copy
    string-copy!
    string-fill!
    string->list
    list->string
    string-map
    string-for-each
    ;; Vectors
    vector?
    make-vector
    vector
    vector-length
    vector-ref
    vector-set!
    vector->list
    list->vector
    vector->string
    string->vector
    vector-copy
    vector-copy!
    vector-append
    vector-fill!
    vector-map
    vector-for-each
    ;; Bytevectors
    bytevector?
    make-bytevector
    bytevector
    bytevector-length
    bytevector-u8-ref
    bytevector-u8-set!
    bytevector-copy
    bytevector-copy!
    bytevector-append
    utf8->string
    string->utf8
    ;; Control features
    procedure?
    apply
    map
    for-each
    call-with-current-continuation
    call/cc
    values
    call-with-values
    dynamic-wind
    ;; Ports
    port?
    input-port?
    output-port?
    input-port-open?
    output-port-open?
    current-input-port
    current-output-port
    current-error-port
    close-port
    close-input-port
    close-output-port
    open-input-string
    open-output-string
    get-output-string
    ;; Input
    eof-object
    eof-object?
    read-char
    peek-char
    read-line
    read-string
    read
    ;; Binary Input (R7RS §6.13.3)
    read-u8
    peek-u8
    u8-ready?
    ;; Output
    newline
    write-char
    write-string
    write
    display
    flush-output-port
    ;; Binary Output (R7RS §6.13.3)
    write-u8
    ;; System interface
    features
    ;; Parameters
    make-parameter
    ;; Exception handling (R7RS 6.11)
    error-object?
    error-object-message
    error-object-irritants
    error
    raise
    raise-continuable
    with-exception-handler))
