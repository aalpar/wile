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
     ((lambda (name ...) (begin body ...)) val ...))))

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

	// Pairs and lists
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
	{"current-input-port", 0, false, primitives.PrimCurrentInputPort},
	{"current-output-port", 0, false, primitives.PrimCurrentOutputPort},
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
}

// registerCompileTimePrimitives creates bindings for compile-time primitives in the environment.
// These primitives (if, lambda, quote, etc.) are handled specially by the compiler and
// should not have their arguments expanded by the macro expander.
func registerCompileTimePrimitives(env *environment.EnvironmentFrame, specs []BindingSpec) {
	for _, spec := range specs {
		sym := env.InternSymbol(values.NewSymbol(spec.Name))
		env.MaybeCreateGlobalBinding(sym, spec.BindingType)
	}
}

// registerRuntimePrimitives creates bindings and foreign closures for runtime primitives.
// Each primitive gets:
//  1. A global binding in the environment (BindingTypeVariable)
//  2. A foreign closure wrapping the Go implementation function
//
// The closure is created with the specified parameter count and variadic flag.
func registerRuntimePrimitives(env *environment.EnvironmentFrame, specs []PrimitiveSpec) error {
	for _, spec := range specs {
		sym := env.InternSymbol(values.NewSymbol(spec.Name))
		env.MaybeCreateGlobalBinding(sym, environment.BindingTypeVariable)

		closure := machine.NewForeignClosure(env, spec.ParamCount, spec.IsVariadic, spec.Impl)
		if err := env.SetGlobalValue(environment.NewGlobalIndex(sym), closure); err != nil {
			return values.WrapForeignErrorf(err, "error registering %s", spec.Name)
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
		stx, err := p.ReadSyntax()
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
//  4. Registers primitive compilers in the compile environment
//  5. Loads bootstrap macros (and, or, let, let*, letrec, cond, when, unless)
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
	if err := registerRuntimePrimitives(env, runtimePrimitives); err != nil {
		return nil, err
	}

	// Register primitive compilers in the compile environment
	if err := machine.RegisterPrimitiveCompilers(env); err != nil {
		return nil, values.WrapForeignErrorf(err, "error registering primitive compilers")
	}

	// Load bootstrap macros (and, or, let, let*, letrec, cond, when, unless)
	if err := loadBootstrapMacros(env); err != nil {
		return nil, values.WrapForeignErrorf(err, "error loading bootstrap macros")
	}

	return env, nil
}
