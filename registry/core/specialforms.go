// Copyright 2026 Aaron Alpar
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

package core

import (
	"github.com/aalpar/wile/registry"
)

// compileTimeBindingSpecs are names that exist only at compile time.
// The expander recognizes these as primitive forms and dispatches to
// registered primitive expanders rather than treating them as applications.
//
//nolint:govet
var compileTimeBindingSpecs = []registry.BindingSpec{
	{"if",
		"Conditional expression. Evaluates TEST; if it yields a true value,\n" +
			"CONSEQUENT is evaluated and its value returned. Otherwise ALTERNATE\n" +
			"is evaluated and returned, or void if omitted. R7RS §4.1.5.\n" +
			"Syntax: (if TEST CONSEQUENT ALTERNATE)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (if #t 1 2)       => 1\n" +
			"  (if #f 1 2)       => 2\n" +
			"  (if (> 3 2) 'yes 'no)  => yes"},
	{"lambda",
		"Creates an anonymous procedure. FORMALS is a list of parameters,\n" +
			"a single identifier for a rest arg, or a dotted pair for fixed+rest.\n" +
			"BODY is one or more expressions evaluated in order. R7RS §4.1.4.\n" +
			"Syntax: (lambda FORMALS BODY)\n" +
			"Category: procedures\n\n" +
			"Examples:\n" +
			"  ((lambda (x) (* x x)) 5)  => 25\n" +
			"  ((lambda xs xs) 1 2 3)     => (1 2 3)"},
	{"case-lambda",
		"Creates a procedure that dispatches on argument count. Each clause\n" +
			"has its own formals and body; the first clause matching the call's\n" +
			"arity is selected. R7RS §4.2.9.\n" +
			"Syntax: (case-lambda (FORMALS BODY) ...)\n" +
			"Category: procedures\n\n" +
			"Examples:\n" +
			"  (define f (case-lambda ((x) x) ((x y) (+ x y))))\n" +
			"  (f 1)      => 1\n" +
			"  (f 1 2)    => 3"},
	{"quote",
		"Returns DATUM as a literal value without evaluating it.\n" +
			"The quoted datum is immutable. R7RS §4.1.2.\n" +
			"Syntax: (quote DATUM) or 'DATUM\n" +
			"Category: quotation\n\n" +
			"Examples:\n" +
			"  'a             => a\n" +
			"  '(1 2 3)       => (1 2 3)\n" +
			"  (quote (+ 1 2))  => (+ 1 2)"},
	{"define",
		"Defines a variable binding. The first form binds the result of EXPRESSION.\n" +
			"The second form is shorthand for binding a lambda. R7RS §5.3.\n" +
			"Syntax: (define VARIABLE EXPRESSION) or (define (VARIABLE FORMALS) BODY)\n" +
			"Category: definitions\n\n" +
			"Examples:\n" +
			"  (define x 42)\n" +
			"  (define (square x) (* x x))\n" +
			"  (square 5)  => 25"},
	{"define-syntax",
		"Defines a macro binding. TRANSFORMER is typically a syntax-rules\n" +
			"expression that specifies the macro's rewrite patterns. R7RS §5.4.\n" +
			"Syntax: (define-syntax KEYWORD TRANSFORMER)\n" +
			"Category: definitions\n\n" +
			"Examples:\n" +
			"  (define-syntax swap! (syntax-rules () ((swap! a b) (let ((t a)) (set! a b) (set! b t)))))\n" +
			"  ;; (swap! x y) expands to a let-based swap"},
	{"set!",
		"Assignment. Evaluates EXPRESSION and stores the result in the\n" +
			"location bound to VARIABLE. The variable must already be defined.\n" +
			"R7RS §4.1.6.\n" +
			"Syntax: (set! VARIABLE EXPRESSION)\n" +
			"Category: binding\n\n" +
			"Examples:\n" +
			"  (let ((x 1)) (set! x 2) x)  => 2"},
	{"begin",
		"Sequences expressions, returning the value of the last one.\n" +
			"At top level or in a body, splices its contents into the\n" +
			"enclosing context. R7RS §4.2.3.\n" +
			"Syntax: (begin EXPRESSION1 EXPRESSION2 ...)\n" +
			"Category: sequencing\n\n" +
			"Examples:\n" +
			"  (begin 1 2 3)  => 3"},
	{"include",
		"Textual inclusion at expand time. Each FILENAME is read and its\n" +
			"contents are spliced into the enclosing context as if written\n" +
			"directly. R7RS §4.1.7.\n" +
			"Syntax: (include FILENAME1 FILENAME2 ...)\n" +
			"Category: libraries"},
	{"include-ci",
		"Like include, but reads the file contents case-insensitively.\n" +
			"Identifiers in the included file are folded to lower case.\n" +
			"R7RS §4.1.7.\n" +
			"Syntax: (include-ci FILENAME1 FILENAME2 ...)\n" +
			"Category: libraries"},
	{"quasiquote",
		"Template construction. Like quote, but allows unquoted\n" +
			"sub-expressions via unquote (,) and unquote-splicing (,@).\n" +
			"R7RS §4.2.8.\n" +
			"Syntax: (quasiquote TEMPLATE) or `TEMPLATE\n" +
			"Category: quotation\n\n" +
			"Examples:\n" +
			"  `(1 ,(+ 1 1) 3)       => (1 2 3)\n" +
			"  `(a ,@'(b c) d)       => (a b c d)"},
	{"unquote",
		"Inside a quasiquote template, evaluates EXPRESSION and inserts\n" +
			"its value into the surrounding template. Only valid within\n" +
			"quasiquote. R7RS §4.2.8.\n" +
			"Syntax: (unquote EXPRESSION) or ,EXPRESSION\n" +
			"Category: quotation"},
	{"unquote-splicing",
		"Inside a quasiquote template, evaluates EXPRESSION (which must\n" +
			"produce a list) and splices its elements into the surrounding\n" +
			"template. Only valid within quasiquote. R7RS §4.2.8.\n" +
			"Syntax: (unquote-splicing EXPRESSION) or ,@EXPRESSION\n" +
			"Category: quotation"},
	{"cond-expand",
		"Feature-based conditional expansion. Each clause has a feature\n" +
			"requirement and a body; the first clause whose requirement is\n" +
			"satisfied has its body expanded. R7RS §4.2.1.\n" +
			"Syntax: (cond-expand CLAUSE1 CLAUSE2 ...)\n" +
			"Category: conditionals"},
	{"define-for-syntax",
		"Defines a variable in the expand-time (phase-1) environment.\n" +
			"The expression is evaluated at expansion time. Used for\n" +
			"helper bindings available to macro transformers.\n" +
			"Syntax: (define-for-syntax VARIABLE EXPRESSION)\n" +
			"Category: macros"},
	{"begin-for-syntax",
		"Evaluates expressions at expand time (phase 1). Used to run\n" +
			"side effects or define helpers during macro expansion.\n" +
			"Syntax: (begin-for-syntax EXPRESSION1 ...)\n" +
			"Category: macros"},
	{"eval-when",
		"Evaluates expressions only during the specified phases.\n" +
			"Phases include expand and compile. Used for phase-specific\n" +
			"side effects and definitions.\n" +
			"Syntax: (eval-when (PHASE ...) EXPRESSION ...)\n" +
			"Category: macros"},
	// R7RS §4.3.1: syntax-error for compile-time errors in macros
	{"syntax-error",
		"Signals a compile-time error during macro expansion.\n" +
			"MESSAGE is a string; IRRITANTs provide additional context.\n" +
			"R7RS §4.3.1.\n" +
			"Syntax: (syntax-error MESSAGE IRRITANT ...)\n" +
			"Category: macros"},
	// R7RS §6.10: dynamic-wind for control flow with cleanup handlers
	{"dynamic-wind",
		"Calls THUNK with BEFORE and AFTER as entry/exit guards.\n" +
			"BEFORE is called on every entry, AFTER on every exit,\n" +
			"including non-local exits via continuations. R7RS §6.10.\n" +
			"Syntax: (dynamic-wind BEFORE THUNK AFTER)\n" +
			"Category: control"},
	// R7RS §6.10: apply for procedure application with argument list
	{"apply",
		"Calls PROC with the given arguments. The last argument must\n" +
			"be a list, whose elements become the tail of the argument list.\n" +
			"R7RS §6.10.\n" +
			"Syntax: (apply PROC ARG1 ... ARGS)\n" +
			"Category: control"},
	// Racket-style continuation marks
	{"with-continuation-mark",
		"Evaluates EXPRESSION with KEY mapped to VALUE in the\n" +
			"current continuation frame. Used for continuation marks\n" +
			"(stack inspection) and parameterize.\n" +
			"Syntax: (with-continuation-mark KEY VALUE EXPRESSION)\n" +
			"Category: control"},
	// R7RS §4.2.2: binding forms (core compiled, not macros)
	{"let",
		"Binding form. Evaluates all INIT expressions, then binds them\n" +
			"to VARs in a new scope for BODY. Named let creates a local\n" +
			"recursive procedure. R7RS §4.2.2.\n" +
			"Syntax: (let ((VAR INIT) ...) BODY) or (let NAME ((VAR INIT) ...) BODY)\n" +
			"Category: binding\n\n" +
			"Examples:\n" +
			"  (let ((x 1) (y 2)) (+ x y))  => 3\n" +
			"  (let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))  => 120"},
	{"let*",
		"Sequential binding form. Like let, but each INIT is evaluated\n" +
			"in a scope that includes the preceding bindings. R7RS §4.2.2.\n" +
			"Syntax: (let* ((VAR INIT) ...) BODY)\n" +
			"Category: binding\n\n" +
			"Examples:\n" +
			"  (let* ((x 1) (y (+ x 1))) y)  => 2"},
	{"letrec",
		"Recursive binding form. All VARs are bound before any INIT\n" +
			"is evaluated, allowing mutually recursive definitions.\n" +
			"R7RS §4.2.2.\n" +
			"Syntax: (letrec ((VAR INIT) ...) BODY)\n" +
			"Category: binding"},
	{"letrec*",
		"Like letrec, but INIT expressions are evaluated left-to-right,\n" +
			"each in a scope that includes all VARs. R7RS §4.2.2.\n" +
			"Syntax: (letrec* ((VAR INIT) ...) BODY)\n" +
			"Category: binding"},
	// Auxiliary syntax (R7RS §4.2.1, §4.2.5)
	// These are literals used in syntax-rules patterns for cond and case
	{"else",
		"Auxiliary syntax used as a catch-all clause in cond, case, and\n" +
			"cond-expand forms. Not a procedure; cannot be called directly.\n" +
			"R7RS §4.2.1.\n" +
			"Syntax: else\n" +
			"Category: auxiliary syntax"},
	{"=>",
		"Auxiliary syntax used in cond clauses: (test => PROC).\n" +
			"When test yields a true value, PROC is called with that value.\n" +
			"Not a procedure; cannot be called directly. R7RS §4.2.1.\n" +
			"Syntax: =>\n" +
			"Category: auxiliary syntax"},
	// Auxiliary syntax (R7RS §4.3.2)
	// syntax-rules is handled by define-syntax at compile time, but needs
	// a binding for library export resolution (like else, =>, ..., _)
	{"syntax-rules",
		"Defines a pattern-based macro transformer. Each clause has a\n" +
			"pattern and template; the first matching pattern determines\n" +
			"the expansion. R7RS §4.3.2.\n" +
			"Syntax: (syntax-rules (LITERAL ...) CLAUSE ...)\n" +
			"Category: macros\n\n" +
			"Examples:\n" +
			"  (define-syntax my-if\n" +
			"    (syntax-rules (then else)\n" +
			"      ((my-if test then c else a) (if test c a))))"},
	// These are special identifiers in syntax-rules patterns
	{"...",
		"Auxiliary syntax for repetition in syntax-rules patterns and\n" +
			"templates. Indicates zero or more repetitions of the preceding\n" +
			"element. Not a procedure; cannot be called directly. R7RS §4.3.2.\n" +
			"Syntax: ...\n" +
			"Category: auxiliary syntax"},
	{"_",
		"Auxiliary syntax for a wildcard in syntax-rules patterns. Matches\n" +
			"any form without binding it. Not a procedure; cannot be called\n" +
			"directly. R7RS §4.3.2.\n" +
			"Syntax: _\n" +
			"Category: auxiliary syntax"},
	// Library system forms (R7RS §5.6 and R6RS aliases)
	{"define-library",
		"Defines a Scheme library. LIBRARY-NAME is a list of symbols.\n" +
			"DECLARATIONs include (export ...), (import ...), (begin ...),\n" +
			"(include ...), (description ...), and (cond-expand ...). R7RS §5.6.\n" +
			"Syntax: (define-library LIBRARY-NAME DECLARATION ...)\n" +
			"Category: libraries"},
	{"library",
		"R6RS alias for define-library. Defines a Scheme library with the\n" +
			"given name and declarations. See define-library.\n" +
			"Syntax: (library LIBRARY-NAME DECLARATION ...)\n" +
			"Category: libraries"},
	{"import",
		"Imports bindings from one or more libraries. Each IMPORT-SET\n" +
			"may be modified with only, except, prefix, or rename.\n" +
			"R7RS §5.6.\n" +
			"Syntax: (import IMPORT-SET ...)\n" +
			"Category: libraries"},
	{"export",
		"Inside define-library: specifies the exported bindings.\n" +
			"Each EXPORT-SPEC is a name or (rename INTERNAL EXTERNAL).\n" +
			"R7RS §5.6.\n" +
			"Syntax: (export EXPORT-SPEC ...)\n" +
			"Category: libraries"},
	// R6RS syntax-case macro system
	{"syntax-case",
		"Pattern-matching macro transformer (R6RS). Each clause is\n" +
			"(PATTERN BODY) or (PATTERN FENDER BODY). Pattern\n" +
			"variables are bound in BODY. Use (syntax TEMPLATE) to\n" +
			"construct syntax output.\n" +
			"Syntax: (syntax-case EXPRESSION (LITERAL ...) CLAUSE ...)\n" +
			"Category: macros"},
	{"syntax",
		"Inside syntax-case, constructs a syntax object from TEMPLATE\n" +
			"with pattern variables substituted. Analogous to quasiquote\n" +
			"for syntax objects (R6RS).\n" +
			"Syntax: (syntax TEMPLATE)\n" +
			"Category: macros"},
	// Quasisyntax forms (R6RS / Wile extension)
	{"quasisyntax",
		"Like quasiquote but for syntax objects. unsyntax and\n" +
			"unsyntax-splicing escapes are evaluated and inserted.\n" +
			"Syntax: (quasisyntax TEMPLATE) or #`TEMPLATE\n" +
			"Category: macros"},
	{"unsyntax",
		"Inside quasisyntax, evaluates EXPRESSION (which must return\n" +
			"a syntax object) and inserts it. Only valid inside quasisyntax.\n" +
			"Syntax: (unsyntax EXPRESSION) or #,EXPRESSION\n" +
			"Category: macros"},
	{"unsyntax-splicing",
		"Inside quasisyntax, evaluates EXPRESSION (which must return a\n" +
			"list of syntax objects) and splices them. Only valid inside quasisyntax.\n" +
			"Syntax: (unsyntax-splicing EXPRESSION) or #,@EXPRESSION\n" +
			"Category: macros"},
	// with-syntax (R6RS pattern binding)
	{"with-syntax",
		"Binds syntax-case pattern variables from EXPRESSIONs.\n" +
			"Like let, but for syntax patterns. Each PATTERN is matched\n" +
			"against its EXPRESSION and bound in BODY.\n" +
			"Syntax: (with-syntax ((PATTERN EXPRESSION) ...) BODY)\n" +
			"Category: macros"},
	// meta (phase-1 compilation)
	{"meta",
		"Compiles EXPRESSIONs in the expand-time (phase-1) environment.\n" +
			"Useful for calling expand-time helpers from compile-time code.\n" +
			"Syntax: (meta EXPRESSION ...)\n" +
			"Category: macros"},
}

// macroDocs provides documentation for bootstrap macros defined in Scheme.
// These macros are loaded from bootstrap_macros.scm; their documentation
// is registered here so it's accessible via the REPL's ,doc command.
//
//nolint:govet
var macroDocs = []registry.DocEntry{
	{"and",
		"Short-circuit conjunction. Evaluates tests left-to-right;\n" +
			"returns #f as soon as one yields false, otherwise returns\n" +
			"the value of the last test. R7RS §4.2.1.\n" +
			"Syntax: (and TEST1 ...)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (and 1 2 3)      => 3\n" +
			"  (and 1 #f 3)     => #f\n" +
			"  (and)            => #t"},
	{"or",
		"Short-circuit disjunction. Evaluates tests left-to-right;\n" +
			"returns the first true value, or #f if all yield false.\n" +
			"R7RS §4.2.1.\n" +
			"Syntax: (or TEST1 ...)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (or #f 2 3)      => 2\n" +
			"  (or #f #f)       => #f\n" +
			"  (or)             => #f"},
	{"cond",
		"Multi-way conditional. Each clause is (TEST EXPR ...) or\n" +
			"(TEST => PROC). Evaluates tests in order; the first true\n" +
			"test's expressions are evaluated. R7RS §4.2.1.\n" +
			"Syntax: (cond CLAUSE1 CLAUSE2 ...)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (cond ((> 3 2) 'greater) ((< 3 2) 'less))  => greater\n" +
			"  (cond (#f 1) (else 2))  => 2"},
	{"case",
		"Datum dispatch. Evaluates KEY, then matches it via eqv? against\n" +
			"datum lists in each clause. The matching clause's expressions\n" +
			"are evaluated. R7RS §4.2.1.\n" +
			"Syntax: (case KEY CLAUSE1 CLAUSE2 ...)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (case (+ 1 1) ((1) 'one) ((2) 'two) (else 'other))  => two"},
	{"when",
		"One-armed conditional. If TEST is true, evaluates the\n" +
			"expressions in order and returns the last value. Returns\n" +
			"void if TEST is false. R7RS §4.2.1.\n" +
			"Syntax: (when TEST EXPRESSION1 EXPRESSION2 ...)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (when #t 42)     => 42\n" +
			"  (when #f 42)     => ; void"},
	{"unless",
		"One-armed conditional. If TEST is false, evaluates the\n" +
			"expressions in order and returns the last value. Returns\n" +
			"void if TEST is true. R7RS §4.2.1.\n" +
			"Syntax: (unless TEST EXPRESSION1 EXPRESSION2 ...)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (unless #f 42)   => 42\n" +
			"  (unless #t 42)   => ; void"},
	{"do",
		"Iteration construct. Initializes variables, then repeatedly\n" +
			"evaluates COMMANDs and advances STEPs until TEST is true.\n" +
			"Returns the value of the last EXPR. R7RS §4.2.4.\n" +
			"Syntax: (do ((VAR INIT STEP) ...) (TEST EXPR ...) COMMAND ...)\n" +
			"Category: iteration\n\n" +
			"Examples:\n" +
			"  (do ((i 0 (+ i 1))) ((= i 3) i))  => 3"},
	{"guard",
		"Exception handling. Evaluates BODY; if an exception is raised,\n" +
			"binds it to VAR and tests cond-style clauses. If no clause\n" +
			"matches, the exception is re-raised. R7RS §4.2.7.\n" +
			"Syntax: (guard (VAR CLAUSE1 ...) BODY)\n" +
			"Category: exceptions\n\n" +
			"Examples:\n" +
			"  (guard (e (#t (error-object-message e))) (error \"oops\"))  => \"oops\""},
	{"parameterize",
		"Dynamic binding. Temporarily binds parameter objects to new\n" +
			"values for the dynamic extent of BODY. Restored on exit,\n" +
			"including non-local exits. R7RS §4.2.6.\n" +
			"Syntax: (parameterize ((PARAM VALUE) ...) BODY)\n" +
			"Category: parameters\n\n" +
			"Examples:\n" +
			"  (let ((p (make-parameter 10))) (parameterize ((p 20)) (p)))  => 20"},
	{"delay",
		"Creates a promise that will evaluate EXPRESSION when forced.\n" +
			"The result is memoized; subsequent forces return the cached\n" +
			"value. R7RS §4.2.5.\n" +
			"Syntax: (delay EXPRESSION)\n" +
			"Category: promises\n\n" +
			"Examples:\n" +
			"  (force (delay (+ 1 2)))  => 3"},
	{"delay-force",
		"Creates an iterative lazy promise. Like delay, but EXPRESSION\n" +
			"must return a promise; forcing trampolines through the chain\n" +
			"without stack growth. R7RS §4.2.5.\n" +
			"Syntax: (delay-force EXPRESSION)\n" +
			"Category: promises"},
	{"define-record-type",
		"Defines a new record type with a constructor, predicate, and\n" +
			"field accessors/mutators. Each FIELD-SPEC is (NAME ACCESSOR)\n" +
			"or (NAME ACCESSOR MUTATOR). R7RS §5.5.\n" +
			"Syntax: (define-record-type NAME CONSTRUCTOR PRED FIELD-SPEC ...)\n" +
			"Category: records\n\n" +
			"Examples:\n" +
			"  (define-record-type <point> (make-point x y) point? (x point-x) (y point-y))\n" +
			"  (point-x (make-point 1 2))  => 1"},
	{"let-values",
		"Multiple-value binding. Each INIT may return multiple values\n" +
			"which are bound to the corresponding VARs. All inits are\n" +
			"evaluated before any bindings are created. R7RS §4.2.2.\n" +
			"Syntax: (let-values (((VAR ...) INIT) ...) BODY)\n" +
			"Category: binding"},
	{"let*-values",
		"Sequential multiple-value binding. Like let-values, but each\n" +
			"INIT is evaluated in a scope that includes the preceding\n" +
			"bindings. R7RS §4.2.2.\n" +
			"Syntax: (let*-values (((VAR ...) INIT) ...) BODY)\n" +
			"Category: binding"},
	{"define-values",
		"Multiple-value definition. EXPRESSION must return as many\n" +
			"values as there are variables in FORMALS. Supports proper\n" +
			"lists, dotted pairs, and rest patterns. R7RS §5.3.3.\n" +
			"Syntax: (define-values FORMALS EXPRESSION)\n" +
			"Category: definitions"},
}

func addSpecialForms(r *registry.Registry) error {
	r.AddBindingSpecs(compileTimeBindingSpecs)
	for _, doc := range macroDocs {
		r.AddDocumentation(doc.Name, doc.Doc)
	}
	return nil
}
