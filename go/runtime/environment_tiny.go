// Copyright 2025 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Package runtime provides the runtime environment initialization and built-in primitives
// for the Scheme interpreter.
//
// This package is responsible for:
//   - Creating and initializing the top-level environment with all R7RS primitives
//   - Implementing foreign functions (Go implementations of Scheme primitives)
//   - Loading bootstrap macros (and, or, let, let*, letrec, cond, when, unless)
//   - Managing I/O ports for read/write operations
//
// # Architecture
//
// The runtime creates a three-phase environment hierarchy:
//
//	TopLevel (Runtime) -> Expand -> Compile
//
// Primitives are registered as foreign closures in the TopLevel environment.
// Each primitive is implemented as a ForeignFunction that receives a MachineContext
// and returns an error (nil on success).
//
// # Foreign Function Convention
//
// All primitive functions follow this signature:
//
//	func PrimXxx(ctx context.Context, mc *machine.MachineContext) error
//
// Arguments are accessed via mc.EnvironmentFrame().GetLocalBindingByIndex(i).Value()
// where i is the 0-based parameter index. For variadic functions, the last parameter
// contains a list of remaining arguments.
//
// Results are returned via mc.SetValue(result).
package runtime

import (
	"context"
	"io"
	"strings"

	"wile/environment"
	"wile/machine"
	"wile/parser"
	"wile/runtime/primitives"
	"wile/values"
)

// bootstrapMacros contains the R7RS derived expression forms as syntax-rules macros.
// These are loaded during environment initialization.
const bootstrapMacros = `
;; Boolean operators
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

;; Binding forms
(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body ...)
     ((lambda (name ...) (begin body ...)) val ...))
    ((let tag ((name val) ...) body ...)
     (letrec ((tag (lambda (name ...) body ...)))
       (tag val ...)))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body ...)
     (let () body ...))
    ((let* ((name1 val1) (name2 val2) ...) body ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...) body ...)))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var init) ...) body ...)
     (let ((var #f) ...)
       (set! var init) ...
       body ...))))

;; Conditional forms
(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test))
     test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp temp (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test (begin result1 result2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test) (begin result1 result2 ...)))))

;; Lazy evaluation (promises)
(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (%make-lazy-promise (lambda () expression)))))

(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expression)
     (%make-lazy-promise (lambda () expression)))))

;; Parameters (dynamic binding)
(define-syntax parameterize
  (syntax-rules ()
    ((parameterize () body ...)
     (begin body ...))
    ((parameterize ((param val) rest ...) body ...)
     (let ((p param)
           (new val)
           (old (param)))
       (dynamic-wind
         (lambda () (p new))
         (lambda () (parameterize (rest ...) body ...))
         (lambda () (p old)))))))

;; Exception handling (R7RS guard macro)
(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) body ...)
     (call/cc
       (lambda (guard-continuation)
         (with-exception-handler
           (lambda (condition)
             (guard-continuation
               (let ((var condition))
                 (guard-aux var clause ...))))
           (lambda () body ...)))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux var (else result ...))
     (begin result ...))
    ((guard-aux var (test => proc) clause ...)
     (if test
         (proc var)
         (guard-aux var clause ...)))
    ((guard-aux var (test result ...) clause ...)
     (if test
         (begin result ...)
         (guard-aux var clause ...)))
    ((guard-aux var)
     (raise var))))

;; Records (SRFI-9 / R7RS define-record-type)
(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type (constructor constructor-tag ...) predicate field-spec ...)
     (define-record-type-impl type (constructor constructor-tag ...) predicate () () field-spec ...))))

(define-syntax define-record-type-impl
  (syntax-rules ()
    ((define-record-type-impl type (constructor constructor-tag ...) predicate (field-name ...) (defn ...))
     (begin
       (define type (make-record-type 'type '(field-name ...)))
       (define constructor (record-constructor type '(constructor-tag ...)))
       (define predicate (record-predicate type))
       defn ...))
    ((define-record-type-impl type (constructor constructor-tag ...) predicate (field-name ...) (defn ...) (field-tag accessor) rest ...)
     (define-record-type-impl type (constructor constructor-tag ...) predicate
       (field-name ... field-tag)
       (defn ... (define accessor (record-accessor type 'field-tag)))
       rest ...))
    ((define-record-type-impl type (constructor constructor-tag ...) predicate (field-name ...) (defn ...) (field-tag accessor modifier) rest ...)
     (define-record-type-impl type (constructor constructor-tag ...) predicate
       (field-name ... field-tag)
       (defn ... (begin (define accessor (record-accessor type 'field-tag)) (define modifier (record-modifier type 'field-tag))))
       rest ...))))

;; Multiple values binding forms
(define-syntax let-values
  (syntax-rules ()
    ((let-values () body ...)
     (begin body ...))
    ((let-values ((formals expr)) body ...)
     (call-with-values
       (lambda () expr)
       (lambda formals body ...)))
    ((let-values ((formals expr) more ...) body ...)
     (call-with-values
       (lambda () expr)
       (lambda formals
         (let-values (more ...) body ...))))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body ...)
     (begin body ...))
    ((let*-values ((formals expr) more ...) body ...)
     (call-with-values
       (lambda () expr)
       (lambda formals
         (let*-values (more ...) body ...))))))

;; Iteration
(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test result ...)
         command ...)
     (letrec ((loop (lambda (var ...)
                      (if test
                          (begin result ...)
                          (begin
                            command ...
                            (loop (do "step" var step ...) ...))))))
       (loop init ...)))
    ((do "step" var)
     var)
    ((do "step" var step)
     step)))
`

// PrimitiveSpec defines a runtime primitive procedure.
type PrimitiveSpec struct {
	Name       string                  // Scheme name (e.g., "+", "car")
	ParamCount int                     // Number of fixed parameters
	IsVariadic bool                    // Whether it accepts variable args
	Impl       machine.ForeignFunction // Implementation function
}

// BindingSpec defines a compile-time binding (no runtime value).
type BindingSpec struct {
	Name        string
	BindingType environment.BindingType
}

// runtimePrimitives lists all primitives with runtime implementations.
var runtimePrimitives = []PrimitiveSpec{
	// Arithmetic
	{"+", 1, true, primitives.PrimAdd},
	{"-", 2, true, primitives.PrimSub},
	{"*", 1, true, primitives.PrimMul},
	{"/", 2, true, primitives.PrimDiv},

	// Numeric comparisons (variadic: compare all adjacent pairs)
	{"=", 2, true, primitives.PrimNumEq},
	{"<", 2, true, primitives.PrimNumLt},
	{">", 2, true, primitives.PrimNumGt},
	{"<=", 2, true, primitives.PrimNumLe},
	{">=", 2, true, primitives.PrimNumGe},

	// Type predicates
	{"null?", 1, false, primitives.PrimNullQ},
	{"pair?", 1, false, primitives.PrimPairQ},
	{"number?", 1, false, primitives.PrimNumberQ},
	{"boolean?", 1, false, primitives.PrimBooleanQ},
	{"string?", 1, false, primitives.PrimStringQ},
	{"symbol?", 1, false, primitives.PrimSymbolQ},
	{"procedure?", 1, false, primitives.PrimProcedureQ},
	{"vector?", 1, false, primitives.PrimVectorQ},
	{"char?", 1, false, primitives.PrimCharQ},
	{"port?", 1, false, primitives.PrimPortQ},
	{"list?", 1, false, primitives.PrimListQ},

	// Numeric type predicates
	{"integer?", 1, false, primitives.PrimIntegerQ},
	{"real?", 1, false, primitives.PrimRealQ},
	{"rational?", 1, false, primitives.PrimRationalQ},
	{"complex?", 1, false, primitives.PrimComplexQ},
	{"exact?", 1, false, primitives.PrimExactQ},
	{"inexact?", 1, false, primitives.PrimInexactQ},
	{"exact-integer?", 1, false, primitives.PrimExactIntegerQ},
	{"zero?", 1, false, primitives.PrimZeroQ},
	{"positive?", 1, false, primitives.PrimPositiveQ},
	{"negative?", 1, false, primitives.PrimNegativeQ},
	{"odd?", 1, false, primitives.PrimOddQ},
	{"even?", 1, false, primitives.PrimEvenQ},

	// Boolean operations
	{"not", 1, false, primitives.PrimNot},

	// Equality predicates
	{"eq?", 2, false, primitives.PrimEqQ},
	{"eqv?", 2, false, primitives.PrimEqvQ},
	{"equal?", 2, false, primitives.PrimEqualQ},

	// Identifier comparison (R7RS syntax)
	{"bound-identifier=?", 2, false, primitives.PrimBoundIdentifierEqualQ},
	{"free-identifier=?", 2, false, primitives.PrimFreeIdentifierEqualQ},

	// Syntax objects (R6RS syntax-case support)
	{"identifier?", 1, false, primitives.PrimIdentifierQ},
	{"syntax->datum", 1, false, primitives.PrimSyntaxToDatum},
	{"datum->syntax", 2, false, primitives.PrimDatumToSyntax},
	{"generate-temporaries", 1, false, primitives.PrimGenerateTemporaries},

	// Pairs and lists
	{"car", 1, false, primitives.PrimCar},
	{"cdr", 1, false, primitives.PrimCdr},
	{"set-car!", 2, false, primitives.PrimSetCar},
	{"set-cdr!", 2, false, primitives.PrimSetCdr},
	// CxR accessors (scheme/cxr) - 2-level
	{"caar", 1, false, primitives.PrimCaar},
	{"cadr", 1, false, primitives.PrimCadr},
	{"cdar", 1, false, primitives.PrimCdar},
	{"cddr", 1, false, primitives.PrimCddr},
	// CxR accessors (scheme/cxr) - 3-level
	{"caaar", 1, false, primitives.PrimCaaar},
	{"caadr", 1, false, primitives.PrimCaadr},
	{"cadar", 1, false, primitives.PrimCadar},
	{"caddr", 1, false, primitives.PrimCaddr},
	{"cdaar", 1, false, primitives.PrimCdaar},
	{"cdadr", 1, false, primitives.PrimCdadr},
	{"cddar", 1, false, primitives.PrimCddar},
	{"cdddr", 1, false, primitives.PrimCdddr},
	// CxR accessors (scheme/cxr) - 4-level
	{"caaaar", 1, false, primitives.PrimCaaaar},
	{"caaadr", 1, false, primitives.PrimCaaadr},
	{"caadar", 1, false, primitives.PrimCaadar},
	{"caaddr", 1, false, primitives.PrimCaaddr},
	{"cadaar", 1, false, primitives.PrimCadaar},
	{"cadadr", 1, false, primitives.PrimCadadr},
	{"caddar", 1, false, primitives.PrimCaddar},
	{"cadddr", 1, false, primitives.PrimCadddr},
	{"cdaaar", 1, false, primitives.PrimCdaaar},
	{"cdaadr", 1, false, primitives.PrimCdaadr},
	{"cdadar", 1, false, primitives.PrimCdadar},
	{"cdaddr", 1, false, primitives.PrimCdaddr},
	{"cddaar", 1, false, primitives.PrimCddaar},
	{"cddadr", 1, false, primitives.PrimCddadr},
	{"cdddar", 1, false, primitives.PrimCdddar},
	{"cddddr", 1, false, primitives.PrimCddddr},
	{"cons", 2, false, primitives.PrimCons},
	{"list", 1, true, primitives.PrimList},
	{"make-list", 2, true, primitives.PrimMakeList},
	{"append", 1, true, primitives.PrimAppend},
	{"length", 1, false, primitives.PrimLength},
	{"reverse", 1, false, primitives.PrimReverse},
	{"list-ref", 2, false, primitives.PrimListRef},
	{"list-set!", 3, false, primitives.PrimListSet},
	{"list-tail", 2, false, primitives.PrimListTail},
	{"memq", 2, false, primitives.PrimMemq},
	{"memv", 2, false, primitives.PrimMemv},
	{"member", 2, false, primitives.PrimMember},
	{"assq", 2, false, primitives.PrimAssq},
	{"assv", 2, false, primitives.PrimAssv},
	{"assoc", 2, false, primitives.PrimAssoc},

	// Numeric operations
	{"abs", 1, false, primitives.PrimAbs},
	{"max", 2, true, primitives.PrimMax},
	{"min", 2, true, primitives.PrimMin},
	{"floor", 1, false, primitives.PrimFloor},
	{"ceiling", 1, false, primitives.PrimCeiling},
	{"truncate", 1, false, primitives.PrimTruncate},
	{"round", 1, false, primitives.PrimRound},
	{"quotient", 2, false, primitives.PrimQuotient},
	{"remainder", 2, false, primitives.PrimRemainder},
	{"modulo", 2, false, primitives.PrimModulo},
	{"floor/", 2, false, primitives.PrimFloorDiv},
	{"floor-quotient", 2, false, primitives.PrimFloorQuotient},
	{"floor-remainder", 2, false, primitives.PrimFloorRemainder},
	{"truncate/", 2, false, primitives.PrimTruncateDiv},
	{"truncate-quotient", 2, false, primitives.PrimTruncateQuotient},
	{"truncate-remainder", 2, false, primitives.PrimTruncateRemainder},
	{"gcd", 1, true, primitives.PrimGcd},
	{"lcm", 1, true, primitives.PrimLcm},
	{"expt", 2, false, primitives.PrimExpt},
	{"square", 1, false, primitives.PrimSquare},
	{"sqrt", 1, false, primitives.PrimSqrt},
	{"exact-integer-sqrt", 1, false, primitives.PrimExactIntegerSqrt},
	{"exact", 1, false, primitives.PrimExact},
	{"inexact", 1, false, primitives.PrimInexact},
	{"numerator", 1, false, primitives.PrimNumerator},
	{"denominator", 1, false, primitives.PrimDenominator},
	{"rationalize", 2, false, primitives.PrimRationalize},

	// Inexact number operations (scheme inexact)
	{"exp", 1, false, primitives.PrimExp},
	{"log", 2, true, primitives.PrimLog},
	{"sin", 1, false, primitives.PrimSin},
	{"cos", 1, false, primitives.PrimCos},
	{"tan", 1, false, primitives.PrimTan},
	{"asin", 1, false, primitives.PrimAsin},
	{"acos", 1, false, primitives.PrimAcos},
	{"atan", 2, true, primitives.PrimAtan},
	{"finite?", 1, false, primitives.PrimFiniteQ},
	{"infinite?", 1, false, primitives.PrimInfiniteQ},
	{"nan?", 1, false, primitives.PrimNanQ},

	// Complex number operations
	{"make-rectangular", 2, false, primitives.PrimMakeRectangular},
	{"make-polar", 2, false, primitives.PrimMakePolar},
	{"real-part", 1, false, primitives.PrimRealPart},
	{"imag-part", 1, false, primitives.PrimImagPart},
	{"magnitude", 1, false, primitives.PrimMagnitude},
	{"angle", 1, false, primitives.PrimAngle},

	// String operations
	{"string-length", 1, false, primitives.PrimStringLength},
	{"string-ref", 2, false, primitives.PrimStringRef},
	{"string-append", 1, true, primitives.PrimStringAppend},
	{"string=?", 2, false, primitives.PrimStringEq},
	{"string<?", 2, false, primitives.PrimStringLt},
	{"string>?", 2, false, primitives.PrimStringGt},
	{"string<=?", 2, false, primitives.PrimStringLe},
	{"string>=?", 2, false, primitives.PrimStringSe},
	{"string-ci=?", 2, false, primitives.PrimStringCiEq},
	{"string-ci<?", 2, false, primitives.PrimStringCiLt},
	{"string-ci>?", 2, false, primitives.PrimStringCiGt},
	{"string-ci<=?", 2, false, primitives.PrimStringCiLe},
	{"string-ci>=?", 2, false, primitives.PrimStringCiGe},
	{"string-upcase", 1, false, primitives.PrimStringUpcase},
	{"string-downcase", 1, false, primitives.PrimStringDowncase},
	{"substring", 3, false, primitives.PrimSubstring},
	{"number->string", 2, true, primitives.PrimNumberToString},
	{"string->number", 2, true, primitives.PrimStringToNumber},
	{"symbol->string", 1, false, primitives.PrimSymbolToString},
	{"string->symbol", 1, false, primitives.PrimStringToSymbol},
	{"string->list", 1, false, primitives.PrimStringToList},
	{"list->string", 1, false, primitives.PrimListToString},

	// Character operations
	{"char=?", 2, false, primitives.PrimCharEq},
	{"char<?", 2, false, primitives.PrimCharLt},
	{"char>?", 2, false, primitives.PrimCharGt},
	{"char<=?", 2, false, primitives.PrimCharLe},
	{"char>=?", 2, false, primitives.PrimCharGe},
	{"char->integer", 1, false, primitives.PrimCharToInteger},
	{"integer->char", 1, false, primitives.PrimIntegerToChar},
	{"char-alphabetic?", 1, false, primitives.PrimCharAlphabeticQ},
	{"char-numeric?", 1, false, primitives.PrimCharNumericQ},
	{"char-whitespace?", 1, false, primitives.PrimCharWhitespaceQ},
	{"char-upper-case?", 1, false, primitives.PrimCharUpperCaseQ},
	{"char-lower-case?", 1, false, primitives.PrimCharLowerCaseQ},
	{"char-upcase", 1, false, primitives.PrimCharUpcase},
	{"char-downcase", 1, false, primitives.PrimCharDowncase},
	{"char-foldcase", 1, false, primitives.PrimCharFoldcase},
	{"digit-value", 1, false, primitives.PrimDigitValue},

	// Vector operations
	{"make-vector", 2, true, primitives.PrimMakeVector},
	{"vector", 1, true, primitives.PrimVector},
	{"vector-length", 1, false, primitives.PrimVectorLength},
	{"vector-ref", 2, false, primitives.PrimVectorRef},
	{"vector-set!", 3, false, primitives.PrimVectorSet},
	{"vector->list", 1, false, primitives.PrimVectorToList},
	{"list->vector", 1, false, primitives.PrimListToVector},

	// Bytevector operations
	{"bytevector?", 1, false, primitives.PrimBytevectorQ},
	{"make-bytevector", 2, true, primitives.PrimMakeBytevector},
	{"bytevector", 1, true, primitives.PrimBytevector},
	{"bytevector-length", 1, false, primitives.PrimBytevectorLength},
	{"bytevector-u8-ref", 2, false, primitives.PrimBytevectorU8Ref},
	{"bytevector-u8-set!", 3, false, primitives.PrimBytevectorU8Set},
	{"bytevector-copy", 2, true, primitives.PrimBytevectorCopy},
	{"bytevector-copy!", 4, true, primitives.PrimBytevectorCopyBang},
	{"bytevector-append", 1, true, primitives.PrimBytevectorAppend},
	{"utf8->string", 2, true, primitives.PrimUtf8ToString},
	{"string->utf8", 2, true, primitives.PrimStringToUtf8},

	// I/O
	// Note: current-input-port, current-output-port, current-error-port are
	// bound as parameter objects in registerPortParameters(), not as primitives.
	{"read-token", 1, true, primitives.PrimReadToken},
	{"read-syntax", 1, true, primitives.PrimReadSyntax},
	{"read", 1, true, primitives.PrimRead},
	{"newline", 1, true, primitives.PrimNewline},
	{"write", 2, true, primitives.PrimWrite},
	{"write-char", 2, true, primitives.PrimWriteChar},
	{"display", 2, true, primitives.PrimDisplay},

	// File I/O (scheme file)
	{"open-input-file", 1, false, primitives.PrimOpenInputFile},
	{"open-output-file", 1, false, primitives.PrimOpenOutputFile},
	{"close-port", 1, false, primitives.PrimClosePort},
	{"close-input-port", 1, false, primitives.PrimClosePort},
	{"close-output-port", 1, false, primitives.PrimClosePort},
	{"input-port?", 1, false, primitives.PrimInputPortQ},
	{"output-port?", 1, false, primitives.PrimOutputPortQ},
	{"input-port-open?", 1, false, primitives.PrimInputPortOpenQ},
	{"output-port-open?", 1, false, primitives.PrimOutputPortOpenQ},
	{"file-exists?", 1, false, primitives.PrimFileExistsQ},
	{"delete-file", 1, false, primitives.PrimDeleteFile},
	{"eof-object", 0, false, primitives.PrimEofObject},
	{"eof-object?", 1, false, primitives.PrimEofObjectQ},
	{"call-with-input-file", 2, false, primitives.PrimCallWithInputFile},
	{"call-with-output-file", 2, false, primitives.PrimCallWithOutputFile},
	{"open-binary-input-file", 1, false, primitives.PrimOpenBinaryInputFile},
	{"open-binary-output-file", 1, false, primitives.PrimOpenBinaryOutputFile},
	{"with-input-from-file", 2, false, primitives.PrimWithInputFromFile},
	{"with-output-to-file", 2, false, primitives.PrimWithOutputToFile},
	{"write-simple", 2, true, primitives.PrimWriteSimple},
	{"write-shared", 2, true, primitives.PrimWriteShared},

	// String and bytevector ports (scheme base)
	{"open-input-string", 1, false, primitives.PrimOpenInputString},
	{"open-output-string", 0, false, primitives.PrimOpenOutputString},
	{"get-output-string", 1, false, primitives.PrimGetOutputString},
	{"open-input-bytevector", 1, false, primitives.PrimOpenInputBytevector},
	{"open-output-bytevector", 0, false, primitives.PrimOpenOutputBytevector},
	{"get-output-bytevector", 1, false, primitives.PrimGetOutputBytevector},

	// Process context (scheme process-context)
	{"command-line", 0, false, primitives.PrimCommandLine},
	{"exit", 1, true, primitives.PrimExit},
	{"emergency-exit", 1, true, primitives.PrimEmergencyExit},
	{"get-environment-variable", 1, false, primitives.PrimGetEnvironmentVariable},
	{"get-environment-variables", 0, false, primitives.PrimGetEnvironmentVariables},

	// Time (scheme time)
	{"current-second", 0, false, primitives.PrimCurrentSecond},
	{"current-jiffy", 0, false, primitives.PrimCurrentJiffy},
	{"jiffies-per-second", 0, false, primitives.PrimJiffiesPerSecond},

	// Higher-order functions
	{"apply", 2, true, primitives.PrimApply},
	{"map", 2, true, primitives.PrimMap},
	{"for-each", 2, true, primitives.PrimForEach},

	// Continuations
	{"call-with-current-continuation", 1, false, primitives.PrimCallCC},
	{"call/cc", 1, false, primitives.PrimCallCC},
	{"dynamic-wind", 3, false, primitives.PrimDynamicWind},

	// Multiple values
	{"values", 1, true, primitives.PrimValues}, // 1 param = rest list (0 required + rest)
	{"call-with-values", 2, false, primitives.PrimCallWithValues},

	// Exception handling (R7RS 6.11)
	{"error-object?", 1, false, primitives.PrimErrorObjectQ},
	{"error-object-message", 1, false, primitives.PrimErrorObjectMessage},
	{"error-object-irritants", 1, false, primitives.PrimErrorObjectIrritants},
	{"with-exception-handler", 2, false, primitives.PrimWithExceptionHandler},
	{"raise", 1, false, primitives.PrimRaise},
	{"raise-continuable", 1, false, primitives.PrimRaiseContinuable},
	{"error", 2, true, primitives.PrimError}, // (error message irritant ...)

	// Feature detection (R7RS)
	{"features", 0, false, primitives.PrimFeatures},

	// Promises (scheme lazy)
	{"promise?", 1, false, primitives.PrimPromiseQ},
	{"make-promise", 1, false, primitives.PrimMakePromise},
	{"force", 1, false, primitives.PrimForce},
	{"%make-lazy-promise", 1, false, primitives.PrimMakeLazyPromise},

	// Eval (scheme eval)
	{"eval", 2, false, primitives.PrimEval},
	{"interaction-environment", 0, false, primitives.PrimInteractionEnvironment},
	{"scheme-report-environment", 1, false, primitives.PrimSchemeReportEnvironment},
	{"null-environment", 1, false, primitives.PrimNullEnvironment},
	{"environment", 1, true, primitives.PrimEnvironment},

	// Load (scheme load)
	{"load", 1, false, primitives.PrimLoad},

	// Phase hooks (expansion and compilation)
	{"expand", 1, false, primitives.PrimExpand},
	{"expand-once", 1, false, primitives.PrimExpandOnce},
	{"syntax-local-value", 1, false, primitives.PrimSyntaxLocalValue},
	{"make-compile-time-value", 1, false, primitives.PrimMakeCompileTimeValue},
	{"syntax-local-introduce", 1, false, primitives.PrimSyntaxLocalIntroduce},
	{"syntax-local-identifier-as-binding", 1, false, primitives.PrimSyntaxLocalIdentifierAsBinding},
	{"compile", 1, false, primitives.PrimCompile},

	// Parameters (scheme base)
	{"make-parameter", 2, true, primitives.PrimMakeParameter},
	{"parameter?", 1, false, primitives.PrimParameterQ},

	// Records (SRFI-9 / R7RS)
	{"make-record-type", 2, false, primitives.PrimMakeRecordType},
	{"record-type?", 1, false, primitives.PrimIsRecordType},
	{"record?", 1, false, primitives.PrimIsRecord},
	{"record-type", 1, false, primitives.PrimRecordType},
	{"record-constructor", 2, false, primitives.PrimRecordConstructor},
	{"record-predicate", 1, false, primitives.PrimRecordPredicate},
	{"record-accessor", 2, false, primitives.PrimRecordAccessor},
	{"record-modifier", 2, false, primitives.PrimRecordModifier},

	// SRFI-18 Threads
	{"current-thread", 0, false, primitives.PrimCurrentThread},
	{"thread?", 1, false, primitives.PrimThreadQ},
	{"make-thread", 2, true, primitives.PrimMakeThread},
	{"thread-name", 1, false, primitives.PrimThreadName},
	{"thread-specific", 1, false, primitives.PrimThreadSpecific},
	{"thread-specific-set!", 2, false, primitives.PrimThreadSpecificSet},
	{"thread-start!", 1, false, primitives.PrimThreadStart},
	{"thread-yield!", 0, false, primitives.PrimThreadYield},
	{"thread-sleep!", 1, false, primitives.PrimThreadSleep},
	{"thread-terminate!", 1, false, primitives.PrimThreadTerminate},
	{"thread-join!", 2, true, primitives.PrimThreadJoin},

	// SRFI-18 Mutexes
	{"mutex?", 1, false, primitives.PrimMutexQ},
	{"make-mutex", 1, true, primitives.PrimMakeMutex},
	{"mutex-name", 1, false, primitives.PrimMutexName},
	{"mutex-specific", 1, false, primitives.PrimMutexSpecific},
	{"mutex-specific-set!", 2, false, primitives.PrimMutexSpecificSet},
	{"mutex-state", 1, false, primitives.PrimMutexState},
	{"mutex-lock!", 2, true, primitives.PrimMutexLock},
	{"mutex-unlock!", 2, true, primitives.PrimMutexUnlock},

	// SRFI-18 Condition Variables
	{"condition-variable?", 1, false, primitives.PrimConditionVariableQ},
	{"make-condition-variable", 1, true, primitives.PrimMakeConditionVariable},
	{"condition-variable-name", 1, false, primitives.PrimConditionVariableName},
	{"condition-variable-specific", 1, false, primitives.PrimConditionVariableSpecific},
	{"condition-variable-specific-set!", 2, false, primitives.PrimConditionVariableSpecificSet},
	{"condition-variable-signal!", 1, false, primitives.PrimConditionVariableSignal},
	{"condition-variable-broadcast!", 1, false, primitives.PrimConditionVariableBroadcast},

	// SRFI-18 Time
	{"current-time", 0, false, primitives.PrimCurrentTime},
	{"time?", 1, false, primitives.PrimTimeQ},
	{"time->seconds", 1, false, primitives.PrimTimeToSeconds},
	{"seconds->time", 1, false, primitives.PrimSecondsToTime},

	// Go Channels
	{"make-channel", 1, true, primitives.PrimMakeChannel},
	{"channel?", 1, false, primitives.PrimChannelQ},
	{"channel-send!", 2, false, primitives.PrimChannelSend},
	{"channel-receive", 1, false, primitives.PrimChannelReceive},
	{"channel-try-send!", 2, false, primitives.PrimChannelTrySend},
	{"channel-try-receive", 1, false, primitives.PrimChannelTryReceive},
	{"channel-close!", 1, false, primitives.PrimChannelClose},
	{"channel-closed?", 1, false, primitives.PrimChannelClosedQ},
	{"channel-length", 1, false, primitives.PrimChannelLength},
	{"channel-capacity", 1, false, primitives.PrimChannelCapacity},

	// Go WaitGroup
	{"make-wait-group", 0, false, primitives.PrimMakeWaitGroup},
	{"wait-group?", 1, false, primitives.PrimWaitGroupQ},
	{"wait-group-add!", 2, false, primitives.PrimWaitGroupAdd},
	{"wait-group-done!", 1, false, primitives.PrimWaitGroupDone},
	{"wait-group-wait!", 1, false, primitives.PrimWaitGroupWait},

	// Go RWMutex
	{"make-rw-mutex", 1, true, primitives.PrimMakeRWMutex},
	{"rw-mutex?", 1, false, primitives.PrimRWMutexQ},
	{"rw-mutex-read-lock!", 1, false, primitives.PrimRWMutexReadLock},
	{"rw-mutex-read-unlock!", 1, false, primitives.PrimRWMutexReadUnlock},
	{"rw-mutex-write-lock!", 1, false, primitives.PrimRWMutexWriteLock},
	{"rw-mutex-write-unlock!", 1, false, primitives.PrimRWMutexWriteUnlock},
	{"rw-mutex-try-read-lock!", 1, false, primitives.PrimRWMutexTryReadLock},
	{"rw-mutex-try-write-lock!", 1, false, primitives.PrimRWMutexTryWriteLock},

	// Go Once
	{"make-once", 0, false, primitives.PrimMakeOnce},
	{"once?", 1, false, primitives.PrimOnceQ},
	{"once-do!", 2, false, primitives.PrimOnceDo},
	{"once-done?", 1, false, primitives.PrimOnceDoneQ},

	// Go Atomic
	{"make-atomic", 1, false, primitives.PrimMakeAtomic},
	{"atomic?", 1, false, primitives.PrimAtomicQ},
	{"atomic-load", 1, false, primitives.PrimAtomicLoad},
	{"atomic-store!", 2, false, primitives.PrimAtomicStore},
	{"atomic-swap!", 2, false, primitives.PrimAtomicSwap},
	{"atomic-compare-and-swap!", 3, false, primitives.PrimAtomicCompareAndSwap},
}

// compileTimePrimitives lists primitives handled by the compiler (bindings only, no runtime values).
// These forms should NOT have their arguments expanded by the macro expander.
var compileTimePrimitives = []BindingSpec{
	{"if", environment.BindingTypePrimitive},
	{"lambda", environment.BindingTypePrimitive},
	{"case-lambda", environment.BindingTypePrimitive},
	{"quote", environment.BindingTypePrimitive},
	{"define", environment.BindingTypePrimitive},
	{"define-syntax", environment.BindingTypePrimitive},
	{"set!", environment.BindingTypePrimitive},
	{"begin", environment.BindingTypePrimitive},
	{"include", environment.BindingTypePrimitive},
	{"include-ci", environment.BindingTypePrimitive},
	{"quasiquote", environment.BindingTypePrimitive},
	{"unquote", environment.BindingTypePrimitive},
	{"unquote-splicing", environment.BindingTypePrimitive},
	{"cond-expand", environment.BindingTypePrimitive},
	{"define-for-syntax", environment.BindingTypePrimitive},
	{"begin-for-syntax", environment.BindingTypePrimitive},
	{"eval-when", environment.BindingTypePrimitive},
}

// expandTimePrimitives lists primitives available at macro expansion time.
// These are needed for syntax-case fenders (guard expressions) which run during expansion.
// This is a subset of runtimePrimitives that are safe and useful for macro transformers.
var expandTimePrimitives = []PrimitiveSpec{
	// Syntax objects (R6RS syntax-case support) - essential for fenders
	{"identifier?", 1, false, primitives.PrimIdentifierQ},
	{"syntax->datum", 1, false, primitives.PrimSyntaxToDatum},
	{"datum->syntax", 2, false, primitives.PrimDatumToSyntax},
	{"bound-identifier=?", 2, false, primitives.PrimBoundIdentifierEqualQ},
	{"free-identifier=?", 2, false, primitives.PrimFreeIdentifierEqualQ},
	{"generate-temporaries", 1, false, primitives.PrimGenerateTemporaries},

	// Type predicates - commonly used in fenders
	{"null?", 1, false, primitives.PrimNullQ},
	{"pair?", 1, false, primitives.PrimPairQ},
	{"list?", 1, false, primitives.PrimListQ},
	{"symbol?", 1, false, primitives.PrimSymbolQ},
	{"number?", 1, false, primitives.PrimNumberQ},
	{"string?", 1, false, primitives.PrimStringQ},
	{"boolean?", 1, false, primitives.PrimBooleanQ},
	{"vector?", 1, false, primitives.PrimVectorQ},
	{"char?", 1, false, primitives.PrimCharQ},
	{"procedure?", 1, false, primitives.PrimProcedureQ},
	{"integer?", 1, false, primitives.PrimIntegerQ},

	// Equality predicates
	{"eq?", 2, false, primitives.PrimEqQ},
	{"eqv?", 2, false, primitives.PrimEqvQ},
	{"equal?", 2, false, primitives.PrimEqualQ},

	// Boolean operations
	{"not", 1, false, primitives.PrimNot},

	// Basic list operations - useful for examining syntax structure
	{"car", 1, false, primitives.PrimCar},
	{"cdr", 1, false, primitives.PrimCdr},
	{"cons", 2, false, primitives.PrimCons},
	{"list", 1, true, primitives.PrimList},
	{"append", 1, true, primitives.PrimAppend},
	{"length", 1, false, primitives.PrimLength},
	{"reverse", 1, false, primitives.PrimReverse},
	{"list-ref", 2, false, primitives.PrimListRef},
	{"list-tail", 2, false, primitives.PrimListTail},
	{"memq", 2, false, primitives.PrimMemq},
	{"memv", 2, false, primitives.PrimMemv},
	{"member", 2, false, primitives.PrimMember},

	// Numeric comparisons - useful for fenders checking counts/lengths
	{"=", 2, true, primitives.PrimNumEq},
	{"<", 2, true, primitives.PrimNumLt},
	{">", 2, true, primitives.PrimNumGt},
	{"<=", 2, true, primitives.PrimNumLe},
	{">=", 2, true, primitives.PrimNumGe},
	{"zero?", 1, false, primitives.PrimZeroQ},
	{"positive?", 1, false, primitives.PrimPositiveQ},
	{"negative?", 1, false, primitives.PrimNegativeQ},

	// Basic arithmetic - occasionally needed
	{"+", 1, true, primitives.PrimAdd},
	{"-", 2, true, primitives.PrimSub},
}

// registerCompileTimePrimitives creates bindings for compile-time primitives in the environment.
// These primitives (if, lambda, quote, etc.) are handled specially by the compiler and
// should not have their arguments expanded by the macro expander.
func registerCompileTimePrimitives(env *environment.EnvironmentFrame, specs []BindingSpec) error {
	compileEnv := env.Compile()
	for _, spec := range specs {
		sym := compileEnv.InternSymbol(values.NewSymbol(spec.Name))
		compileEnv.MaybeCreateOwnGlobalBinding(sym, spec.BindingType) // Fixed
	}
	return nil
}

// registerRuntimePrimitives creates bindings and foreign closures for runtime primitives.
// Each primitive gets:
//  1. A global binding in the environment (BindingTypeVariable)
//  2. A foreign closure wrapping the Go implementation function
//
// The closure is created with the specified parameter count and variadic flag.
func registerRuntimePrimitives(env *environment.EnvironmentFrame, specs []PrimitiveSpec) error {
	// Register in TopLevel (which IS the runtime phase) - not env.Runtime() which is a separate meta phase
	for _, spec := range specs {
		sym := env.InternSymbol(values.NewSymbol(spec.Name))
		env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

		closure := machine.NewForeignClosure(env, spec.ParamCount, spec.IsVariadic, spec.Impl)
		if err := env.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure); err != nil {
			return values.WrapForeignErrorf(err, "error registering %s", spec.Name)
		}
	}
	return nil
}

// registerExpandTimePrimitives creates bindings and foreign closures for expand-time primitives.
// These are registered in the expand phase environment (env.Expand()) and are available
// during macro expansion, specifically for syntax-case fender (guard) expressions.
//
// This enables fenders like:
//
//	(syntax-case stx ()
//	  ((my-macro x) (identifier? #'x) body)  ; fender can use identifier?
//	  ...)
func registerExpandTimePrimitives(env *environment.EnvironmentFrame, specs []PrimitiveSpec) error {
	expandEnv := env.Expand()
	for _, spec := range specs {
		sym := expandEnv.InternSymbol(values.NewSymbol(spec.Name))
		expandEnv.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)

		closure := machine.NewForeignClosure(expandEnv, spec.ParamCount, spec.IsVariadic, spec.Impl)
		if err := expandEnv.SetOwnGlobalValue(environment.NewGlobalIndex(sym), closure); err != nil {
			return values.WrapForeignErrorf(err, "error registering expand-time primitive %s", spec.Name)
		}
	}
	return nil
}

// loadBootstrapMacros parses and executes the bootstrap macro definitions.
// This loads the derived expression forms (and, or, let, let*, letrec, cond, when, unless)
// which are defined as syntax-rules macros in the bootstrapMacros constant.
//
// Each macro definition is:
//  1. Parsed from the bootstrapMacros string
//  2. Macro-expanded (which is a no-op for define-syntax at top level)
//  3. Compiled to bytecode
//  4. Executed to register the syntax transformer
func loadBootstrapMacros(env *environment.EnvironmentFrame) error {
	ctx := context.Background()
	rdr := strings.NewReader(bootstrapMacros)
	p := parser.NewParser(env, rdr)

	for {
		stx, err := p.ReadSyntax(nil)
		if err == io.EOF {
			break
		}
		if err != nil {
			return values.WrapForeignErrorf(err, "error parsing bootstrap macros")
		}

		// Expand the syntax
		ectx := machine.NewExpandTimeCallContext()
		expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
		if err != nil {
			return values.WrapForeignErrorf(err, "error expanding bootstrap macro")
		}

		// Compile and run
		tpl := machine.NewNativeTemplate(0, 0, false)
		// Use inTail=false for top-level expressions
		cctx := machine.NewCompileTimeCallContext(false, true, env)
		err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
		if err != nil {
			return values.WrapForeignErrorf(err, "error compiling bootstrap macro")
		}

		cont := machine.NewMachineContinuation(nil, tpl, env)
		mc := machine.NewMachineContext(cont)
		err = mc.Run(ctx)
		if err != nil {
			return values.WrapForeignErrorf(err, "error running bootstrap macro")
		}
	}
	return nil
}

// NewTopLevelEnvironmentFrameTiny creates and initializes a complete Scheme runtime environment.
//
// This function:
//  1. Creates a new top-level environment frame
//  2. Registers compile-time primitives (if, lambda, quote, define, etc.)
//  3. Registers runtime primitives (arithmetic, list ops, I/O, etc.)
//  4. Registers expand-time primitives for syntax-case fenders (identifier?, pair?, etc.)
//  5. Registers port parameters (current-input-port, current-output-port, current-error-port)
//  6. Registers primitive compilers in the compile environment
//  7. Loads bootstrap macros (and, or, let, let*, letrec, cond, when, unless, parameterize)
//
// The resulting environment is ready for parsing, expanding, compiling, and executing
// Scheme programs.
func NewTopLevelEnvironmentFrameTiny() (*environment.EnvironmentFrame, error) {
	// Initialize primitives package state
	primitives.InitState()

	env := environment.NewTopLevelEnvironmentFrame()

	// Register compile-time primitives (bindings only, no values)
	registerCompileTimePrimitives(env, compileTimePrimitives)

	// Register runtime primitives (bindings + foreign closures)
	err := registerRuntimePrimitives(env, runtimePrimitives)
	if err != nil {
		return nil, err
	}

	// Register expand-time primitives for syntax-case fenders
	err = registerExpandTimePrimitives(env, expandTimePrimitives)
	if err != nil {
		return nil, err
	}

	// Register port parameters as global variables
	registerPortParameters(env)

	// Register syntax compilers in the compile environment
	err = machine.RegisterSyntaxCompilers(env)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error registering syntax compilers")
	}

	// Register primitive expanders in the expand environment
	err = machine.RegisterPrimitiveExpanders(env)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error registering primitive expanders")
	}

	// Load bootstrap macros (and, or, let, let*, letrec, cond, when, unless, parameterize)
	err = loadBootstrapMacros(env)
	if err != nil {
		return nil, values.WrapForeignErrorf(err, "error loading bootstrap macros")
	}

	return env, nil
}

// registerPortParameters binds the port parameter objects in the global environment.
// These are R7RS parameters that can be used with parameterize.
func registerPortParameters(env *environment.EnvironmentFrame) {
	portParams := []struct {
		name  string
		param *values.Parameter
	}{
		{"current-input-port", primitives.GetCurrentInputPortParam()},
		{"current-output-port", primitives.GetCurrentOutputPortParam()},
		{"current-error-port", primitives.GetCurrentErrorPortParam()},
	}

	for _, pp := range portParams {
		sym := env.InternSymbol(values.NewSymbol(pp.name))
		idx, _ := env.MaybeCreateOwnGlobalBinding(sym, environment.BindingTypeVariable)
		env.SetOwnGlobalValue(idx, pp.param) //nolint:errcheck
	}
}
