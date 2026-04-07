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
		"Conditional expression. Evaluates <test>; if it yields a true value,\n" +
			"<consequent> is evaluated and its value returned. Otherwise <alternate>\n" +
			"is evaluated and returned, or void if omitted. R7RS §4.1.5.\n" +
			"Syntax: (if <test> <consequent> <alternate>)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (if #t 1 2)       => 1\n" +
			"  (if #f 1 2)       => 2\n" +
			"  (if (> 3 2) 'yes 'no)  => yes"},
	{"lambda",
		"Creates an anonymous procedure. <formals> is a list of parameters,\n" +
			"a single identifier for a rest arg, or a dotted pair for fixed+rest.\n" +
			"<body> is one or more expressions evaluated in order. R7RS §4.1.4.\n" +
			"Syntax: (lambda <formals> <body>)\n" +
			"Category: procedures\n\n" +
			"Examples:\n" +
			"  ((lambda (x) (* x x)) 5)  => 25\n" +
			"  ((lambda xs xs) 1 2 3)     => (1 2 3)"},
	{"case-lambda",
		"Creates a procedure that dispatches on argument count. Each clause\n" +
			"has its own formals and body; the first clause matching the call's\n" +
			"arity is selected. R7RS §4.2.9.\n" +
			"Syntax: (case-lambda (<formals> <body>) ...)\n" +
			"Category: procedures\n\n" +
			"Examples:\n" +
			"  (define f (case-lambda ((x) x) ((x y) (+ x y))))\n" +
			"  (f 1)      => 1\n" +
			"  (f 1 2)    => 3"},
	{"quote",
		"Returns <datum> as a literal value without evaluating it.\n" +
			"The quoted datum is immutable. R7RS §4.1.2.\n" +
			"Syntax: (quote <datum>) or '<datum>\n" +
			"Category: quotation\n\n" +
			"Examples:\n" +
			"  'a             => a\n" +
			"  '(1 2 3)       => (1 2 3)\n" +
			"  (quote (+ 1 2))  => (+ 1 2)"},
	{"define",
		"Defines a variable binding. The first form binds the result of <expression>.\n" +
			"The second form is shorthand for binding a lambda. R7RS §5.3.\n" +
			"Syntax: (define <variable> <expression>) or (define (<variable> <formals>) <body>)\n" +
			"Category: definitions\n\n" +
			"Examples:\n" +
			"  (define x 42)\n" +
			"  (define (square x) (* x x))\n" +
			"  (square 5)  => 25"},
	{"define-syntax",
		"Defines a macro binding. <transformer> is typically a syntax-rules\n" +
			"expression that specifies the macro's rewrite patterns. R7RS §5.4.\n" +
			"Syntax: (define-syntax <keyword> <transformer>)\n" +
			"Category: definitions\n\n" +
			"Examples:\n" +
			"  (define-syntax swap! (syntax-rules () ((swap! a b) (let ((t a)) (set! a b) (set! b t)))))\n" +
			"  ;; (swap! x y) expands to a let-based swap"},
	{"set!",
		"Assignment. Evaluates <expression> and stores the result in the\n" +
			"location bound to <variable>. The variable must already be defined.\n" +
			"R7RS §4.1.6.\n" +
			"Syntax: (set! <variable> <expression>)\n" +
			"Category: binding\n\n" +
			"Examples:\n" +
			"  (let ((x 1)) (set! x 2) x)  => 2"},
	{"begin",
		"Sequences expressions, returning the value of the last one.\n" +
			"At top level or in a body, splices its contents into the\n" +
			"enclosing context. R7RS §4.2.3.\n" +
			"Syntax: (begin <expression1> <expression2> ...)\n" +
			"Category: sequencing\n\n" +
			"Examples:\n" +
			"  (begin 1 2 3)  => 3"},
	{"include",
		"Textual inclusion at expand time. Each <filename> is read and its\n" +
			"contents are spliced into the enclosing context as if written\n" +
			"directly. R7RS §4.1.7.\n" +
			"Syntax: (include <filename1> <filename2> ...)\n" +
			"Category: libraries"},
	{"include-ci",
		"Like include, but reads the file contents case-insensitively.\n" +
			"Identifiers in the included file are folded to lower case.\n" +
			"R7RS §4.1.7.\n" +
			"Syntax: (include-ci <filename1> <filename2> ...)\n" +
			"Category: libraries"},
	{"quasiquote",
		"Template construction. Like quote, but allows unquoted\n" +
			"sub-expressions via unquote (,) and unquote-splicing (,@).\n" +
			"R7RS §4.2.8.\n" +
			"Syntax: (quasiquote <template>) or `<template>\n" +
			"Category: quotation\n\n" +
			"Examples:\n" +
			"  `(1 ,(+ 1 1) 3)       => (1 2 3)\n" +
			"  `(a ,@'(b c) d)       => (a b c d)"},
	{"unquote",
		"Inside a quasiquote template, evaluates <expression> and inserts\n" +
			"its value into the surrounding template. Only valid within\n" +
			"quasiquote. R7RS §4.2.8.\n" +
			"Syntax: (unquote <expression>) or ,<expression>\n" +
			"Category: quotation"},
	{"unquote-splicing",
		"Inside a quasiquote template, evaluates <expression> (which must\n" +
			"produce a list) and splices its elements into the surrounding\n" +
			"template. Only valid within quasiquote. R7RS §4.2.8.\n" +
			"Syntax: (unquote-splicing <expression>) or ,@<expression>\n" +
			"Category: quotation"},
	{"cond-expand",
		"Feature-based conditional expansion. Each clause has a feature\n" +
			"requirement and a body; the first clause whose requirement is\n" +
			"satisfied has its body expanded. R7RS §4.2.1.\n" +
			"Syntax: (cond-expand <clause1> <clause2> ...)\n" +
			"Category: conditionals"},
	{"define-for-syntax",
		"Defines a variable in the expand-time (phase-1) environment.\n" +
			"The expression is evaluated at expansion time. Used for\n" +
			"helper bindings available to macro transformers.\n" +
			"Syntax: (define-for-syntax <variable> <expression>)\n" +
			"Category: macros"},
	{"begin-for-syntax",
		"Evaluates expressions at expand time (phase 1). Used to run\n" +
			"side effects or define helpers during macro expansion.\n" +
			"Syntax: (begin-for-syntax <expression1> ...)\n" +
			"Category: macros"},
	{"eval-when",
		"Evaluates expressions only during the specified phases.\n" +
			"Phases include expand and compile. Used for phase-specific\n" +
			"side effects and definitions.\n" +
			"Syntax: (eval-when (<phase> ...) <expression> ...)\n" +
			"Category: macros"},
	// R7RS §4.3.1: syntax-error for compile-time errors in macros
	{"syntax-error",
		"Signals a compile-time error during macro expansion.\n" +
			"<message> is a string; <irritant>s provide additional context.\n" +
			"R7RS §4.3.1.\n" +
			"Syntax: (syntax-error <message> <irritant> ...)\n" +
			"Category: macros"},
	// R7RS §6.10: dynamic-wind for control flow with cleanup handlers
	{"dynamic-wind",
		"Calls <thunk> with <before> and <after> as entry/exit guards.\n" +
			"<before> is called on every entry, <after> on every exit,\n" +
			"including non-local exits via continuations. R7RS §6.10.\n" +
			"Syntax: (dynamic-wind <before> <thunk> <after>)\n" +
			"Category: control"},
	// R7RS §6.10: apply for procedure application with argument list
	{"apply",
		"Calls <proc> with the given arguments. The last argument must\n" +
			"be a list, whose elements become the tail of the argument list.\n" +
			"R7RS §6.10.\n" +
			"Syntax: (apply <proc> <arg1> ... <args>)\n" +
			"Category: control"},
	// Racket-style continuation marks
	{"with-continuation-mark",
		"Evaluates <expression> with <key> mapped to <value> in the\n" +
			"current continuation frame. Used for continuation marks\n" +
			"(stack inspection) and parameterize.\n" +
			"Syntax: (with-continuation-mark <key> <value> <expression>)\n" +
			"Category: control"},
	// R7RS §4.2.2: binding forms (core compiled, not macros)
	{"let",
		"Binding form. Evaluates all <init> expressions, then binds them\n" +
			"to <var>s in a new scope for <body>. Named let creates a local\n" +
			"recursive procedure. R7RS §4.2.2.\n" +
			"Syntax: (let ((<var> <init>) ...) <body>) or (let <name> ((<var> <init>) ...) <body>)\n" +
			"Category: binding\n\n" +
			"Examples:\n" +
			"  (let ((x 1) (y 2)) (+ x y))  => 3\n" +
			"  (let loop ((n 5) (acc 1)) (if (= n 0) acc (loop (- n 1) (* acc n))))  => 120"},
	{"let*",
		"Sequential binding form. Like let, but each <init> is evaluated\n" +
			"in a scope that includes the preceding bindings. R7RS §4.2.2.\n" +
			"Syntax: (let* ((<var> <init>) ...) <body>)\n" +
			"Category: binding\n\n" +
			"Examples:\n" +
			"  (let* ((x 1) (y (+ x 1))) y)  => 2"},
	{"letrec",
		"Recursive binding form. All <var>s are bound before any <init>\n" +
			"is evaluated, allowing mutually recursive definitions.\n" +
			"R7RS §4.2.2.\n" +
			"Syntax: (letrec ((<var> <init>) ...) <body>)\n" +
			"Category: binding"},
	{"letrec*",
		"Like letrec, but <init> expressions are evaluated left-to-right,\n" +
			"each in a scope that includes all <var>s. R7RS §4.2.2.\n" +
			"Syntax: (letrec* ((<var> <init>) ...) <body>)\n" +
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
		"Auxiliary syntax used in cond clauses: (test => <proc>).\n" +
			"When test yields a true value, <proc> is called with that value.\n" +
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
			"Syntax: (syntax-rules (<literal> ...) <clause> ...)\n" +
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
		"Defines a Scheme library. <library-name> is a list of symbols.\n" +
			"<declaration>s include (export ...), (import ...), (begin ...),\n" +
			"(include ...), (description ...), and (cond-expand ...). R7RS §5.6.\n" +
			"Syntax: (define-library <library-name> <declaration> ...)\n" +
			"Category: libraries"},
	{"library",
		"R6RS alias for define-library. Defines a Scheme library with the\n" +
			"given name and declarations. See define-library.\n" +
			"Syntax: (library <library-name> <declaration> ...)\n" +
			"Category: libraries"},
	{"import",
		"Imports bindings from one or more libraries. Each <import-set>\n" +
			"may be modified with only, except, prefix, or rename.\n" +
			"R7RS §5.6.\n" +
			"Syntax: (import <import-set> ...)\n" +
			"Category: libraries"},
	{"export",
		"Inside define-library: specifies the exported bindings.\n" +
			"Each <export-spec> is a name or (rename <internal> <external>).\n" +
			"R7RS §5.6.\n" +
			"Syntax: (export <export-spec> ...)\n" +
			"Category: libraries"},
	// R6RS syntax-case macro system
	{"syntax-case",
		"Pattern-matching macro transformer (R6RS). Each clause is\n" +
			"(<pattern> <body>) or (<pattern> <fender> <body>). Pattern\n" +
			"variables are bound in <body>. Use (syntax template) to\n" +
			"construct syntax output.\n" +
			"Syntax: (syntax-case <expression> (<literal> ...) <clause> ...)\n" +
			"Category: macros"},
	{"syntax",
		"Inside syntax-case, constructs a syntax object from <template>\n" +
			"with pattern variables substituted. Analogous to quasiquote\n" +
			"for syntax objects (R6RS).\n" +
			"Syntax: (syntax <template>)\n" +
			"Category: macros"},
	// Quasisyntax forms (R6RS / Wile extension)
	{"quasisyntax",
		"Like quasiquote but for syntax objects. unsyntax and\n" +
			"unsyntax-splicing escapes are evaluated and inserted.\n" +
			"Syntax: (quasisyntax <template>) or #`<template>\n" +
			"Category: macros"},
	{"unsyntax",
		"Inside quasisyntax, evaluates <expression> (which must return\n" +
			"a syntax object) and inserts it. Only valid inside quasisyntax.\n" +
			"Syntax: (unsyntax <expression>) or #,<expression>\n" +
			"Category: macros"},
	{"unsyntax-splicing",
		"Inside quasisyntax, evaluates <expression> (which must return a\n" +
			"list of syntax objects) and splices them. Only valid inside quasisyntax.\n" +
			"Syntax: (unsyntax-splicing <expression>) or #,@<expression>\n" +
			"Category: macros"},
	// with-syntax (R6RS pattern binding)
	{"with-syntax",
		"Binds syntax-case pattern variables from <expression>s.\n" +
			"Like let, but for syntax patterns. Each <pattern> is matched\n" +
			"against its <expression> and bound in <body>.\n" +
			"Syntax: (with-syntax ((<pattern> <expression>) ...) <body>)\n" +
			"Category: macros"},
	// meta (phase-1 compilation)
	{"meta",
		"Compiles <expression>s in the expand-time (phase-1) environment.\n" +
			"Useful for calling expand-time helpers from compile-time code.\n" +
			"Syntax: (meta <expression> ...)\n" +
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
			"Syntax: (and <test1> ...)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (and 1 2 3)      => 3\n" +
			"  (and 1 #f 3)     => #f\n" +
			"  (and)            => #t"},
	{"or",
		"Short-circuit disjunction. Evaluates tests left-to-right;\n" +
			"returns the first true value, or #f if all yield false.\n" +
			"R7RS §4.2.1.\n" +
			"Syntax: (or <test1> ...)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (or #f 2 3)      => 2\n" +
			"  (or #f #f)       => #f\n" +
			"  (or)             => #f"},
	{"cond",
		"Multi-way conditional. Each clause is (<test> <expr> ...) or\n" +
			"(<test> => <proc>). Evaluates tests in order; the first true\n" +
			"test's expressions are evaluated. R7RS §4.2.1.\n" +
			"Syntax: (cond <clause1> <clause2> ...)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (cond ((> 3 2) 'greater) ((< 3 2) 'less))  => greater\n" +
			"  (cond (#f 1) (else 2))  => 2"},
	{"case",
		"Datum dispatch. Evaluates <key>, then matches it via eqv? against\n" +
			"datum lists in each clause. The matching clause's expressions\n" +
			"are evaluated. R7RS §4.2.1.\n" +
			"Syntax: (case <key> <clause1> <clause2> ...)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (case (+ 1 1) ((1) 'one) ((2) 'two) (else 'other))  => two"},
	{"when",
		"One-armed conditional. If <test> is true, evaluates the\n" +
			"expressions in order and returns the last value. Returns\n" +
			"void if <test> is false. R7RS §4.2.1.\n" +
			"Syntax: (when <test> <expression1> <expression2> ...)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (when #t 42)     => 42\n" +
			"  (when #f 42)     => ; void"},
	{"unless",
		"One-armed conditional. If <test> is false, evaluates the\n" +
			"expressions in order and returns the last value. Returns\n" +
			"void if <test> is true. R7RS §4.2.1.\n" +
			"Syntax: (unless <test> <expression1> <expression2> ...)\n" +
			"Category: conditionals\n\n" +
			"Examples:\n" +
			"  (unless #f 42)   => 42\n" +
			"  (unless #t 42)   => ; void"},
	{"do",
		"Iteration construct. Initializes variables, then repeatedly\n" +
			"evaluates <command>s and advances <step>s until <test> is true.\n" +
			"Returns the value of the last <expr>. R7RS §4.2.4.\n" +
			"Syntax: (do ((<var> <init> <step>) ...) (<test> <expr> ...) <command> ...)\n" +
			"Category: iteration\n\n" +
			"Examples:\n" +
			"  (do ((i 0 (+ i 1))) ((= i 3) i))  => 3"},
	{"guard",
		"Exception handling. Evaluates <body>; if an exception is raised,\n" +
			"binds it to <var> and tests cond-style clauses. If no clause\n" +
			"matches, the exception is re-raised. R7RS §4.2.7.\n" +
			"Syntax: (guard (<var> <clause1> ...) <body>)\n" +
			"Category: exceptions\n\n" +
			"Examples:\n" +
			"  (guard (e (#t (error-object-message e))) (error \"oops\"))  => \"oops\""},
	{"parameterize",
		"Dynamic binding. Temporarily binds parameter objects to new\n" +
			"values for the dynamic extent of <body>. Restored on exit,\n" +
			"including non-local exits. R7RS §4.2.6.\n" +
			"Syntax: (parameterize ((<param> <value>) ...) <body>)\n" +
			"Category: parameters\n\n" +
			"Examples:\n" +
			"  (let ((p (make-parameter 10))) (parameterize ((p 20)) (p)))  => 20"},
	{"delay",
		"Creates a promise that will evaluate <expression> when forced.\n" +
			"The result is memoized; subsequent forces return the cached\n" +
			"value. R7RS §4.2.5.\n" +
			"Syntax: (delay <expression>)\n" +
			"Category: promises\n\n" +
			"Examples:\n" +
			"  (force (delay (+ 1 2)))  => 3"},
	{"delay-force",
		"Creates an iterative lazy promise. Like delay, but <expression>\n" +
			"must return a promise; forcing trampolines through the chain\n" +
			"without stack growth. R7RS §4.2.5.\n" +
			"Syntax: (delay-force <expression>)\n" +
			"Category: promises"},
	{"define-record-type",
		"Defines a new record type with a constructor, predicate, and\n" +
			"field accessors/mutators. Each <field> is (<name> <accessor>)\n" +
			"or (<name> <accessor> <mutator>). R7RS §5.5.\n" +
			"Syntax: (define-record-type <name> <constructor> <pred> <field> ...)\n" +
			"Category: records\n\n" +
			"Examples:\n" +
			"  (define-record-type <point> (make-point x y) point? (x point-x) (y point-y))\n" +
			"  (point-x (make-point 1 2))  => 1"},
	{"let-values",
		"Multiple-value binding. Each <init> may return multiple values\n" +
			"which are bound to the corresponding <var>s. All inits are\n" +
			"evaluated before any bindings are created. R7RS §4.2.2.\n" +
			"Syntax: (let-values (((<var> ...) <init>) ...) <body>)\n" +
			"Category: binding"},
	{"let*-values",
		"Sequential multiple-value binding. Like let-values, but each\n" +
			"<init> is evaluated in a scope that includes the preceding\n" +
			"bindings. R7RS §4.2.2.\n" +
			"Syntax: (let*-values (((<var> ...) <init>) ...) <body>)\n" +
			"Category: binding"},
	{"define-values",
		"Multiple-value definition. <expression> must return as many\n" +
			"values as there are variables in <formals>. Supports proper\n" +
			"lists, dotted pairs, and rest patterns. R7RS §5.3.3.\n" +
			"Syntax: (define-values <formals> <expression>)\n" +
			"Category: definitions"},
}

func addSpecialForms(r *registry.Registry) error {
	r.AddBindingSpecs(compileTimeBindingSpecs)
	for _, doc := range macroDocs {
		r.AddDocumentation(doc.Name, doc.Doc)
	}
	return nil
}
