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
		"Syntax: (if <test> <consequent> <alternate>)\n" +
			"Conditional expression. Evaluates <test>; if it yields a true value,\n" +
			"<consequent> is evaluated and its value returned. Otherwise <alternate>\n" +
			"is evaluated and returned, or void if omitted. R7RS §4.1.5."},
	{"lambda",
		"Syntax: (lambda <formals> <body>)\n" +
			"Creates an anonymous procedure. <formals> is a list of parameters,\n" +
			"a single identifier for a rest arg, or a dotted pair for fixed+rest.\n" +
			"<body> is one or more expressions evaluated in order. R7RS §4.1.4."},
	{"case-lambda",
		"Syntax: (case-lambda (<formals> <body>) ...)\n" +
			"Creates a procedure that dispatches on argument count. Each clause\n" +
			"has its own formals and body; the first clause matching the call's\n" +
			"arity is selected. R7RS §4.2.9."},
	{"quote",
		"Syntax: (quote <datum>) or '<datum>\n" +
			"Returns <datum> as a literal value without evaluating it.\n" +
			"The quoted datum is immutable. R7RS §4.1.2."},
	{"define",
		"Syntax: (define <variable> <expression>) or (define (<variable> <formals>) <body>)\n" +
			"Defines a variable binding. The first form binds the result of <expression>.\n" +
			"The second form is shorthand for binding a lambda. R7RS §5.3."},
	{"define-syntax",
		"Syntax: (define-syntax <keyword> <transformer>)\n" +
			"Defines a macro binding. <transformer> is typically a syntax-rules\n" +
			"expression that specifies the macro's rewrite patterns. R7RS §5.4."},
	{"set!",
		"Syntax: (set! <variable> <expression>)\n" +
			"Assignment. Evaluates <expression> and stores the result in the\n" +
			"location bound to <variable>. The variable must already be defined.\n" +
			"R7RS §4.1.6."},
	{"begin",
		"Syntax: (begin <expression1> <expression2> ...)\n" +
			"Sequences expressions, returning the value of the last one.\n" +
			"At top level or in a body, splices its contents into the\n" +
			"enclosing context. R7RS §4.2.3."},
	{"include",
		"Syntax: (include <filename1> <filename2> ...)\n" +
			"Textual inclusion at expand time. Each <filename> is read and its\n" +
			"contents are spliced into the enclosing context as if written\n" +
			"directly. R7RS §4.1.7."},
	{"include-ci",
		"Syntax: (include-ci <filename1> <filename2> ...)\n" +
			"Like include, but reads the file contents case-insensitively.\n" +
			"Identifiers in the included file are folded to lower case.\n" +
			"R7RS §4.1.7."},
	{"quasiquote",
		"Syntax: (quasiquote <template>) or `<template>\n" +
			"Template construction. Like quote, but allows unquoted\n" +
			"sub-expressions via unquote (,) and unquote-splicing (,@).\n" +
			"R7RS §4.2.8."},
	{"unquote",
		"Syntax: (unquote <expression>) or ,<expression>\n" +
			"Inside a quasiquote template, evaluates <expression> and inserts\n" +
			"its value into the surrounding template. Only valid within\n" +
			"quasiquote. R7RS §4.2.8."},
	{"unquote-splicing",
		"Syntax: (unquote-splicing <expression>) or ,@<expression>\n" +
			"Inside a quasiquote template, evaluates <expression> (which must\n" +
			"produce a list) and splices its elements into the surrounding\n" +
			"template. Only valid within quasiquote. R7RS §4.2.8."},
	{"cond-expand",
		"Syntax: (cond-expand <clause1> <clause2> ...)\n" +
			"Feature-based conditional expansion. Each clause has a feature\n" +
			"requirement and a body; the first clause whose requirement is\n" +
			"satisfied has its body expanded. R7RS §4.2.1."},
	{"define-for-syntax",
		"Syntax: (define-for-syntax <variable> <expression>)\n" +
			"Defines a variable in the expand-time (phase-1) environment.\n" +
			"The expression is evaluated at expansion time. Used for\n" +
			"helper bindings available to macro transformers."},
	{"begin-for-syntax",
		"Syntax: (begin-for-syntax <expression1> ...)\n" +
			"Evaluates expressions at expand time (phase 1). Used to run\n" +
			"side effects or define helpers during macro expansion."},
	{"eval-when",
		"Syntax: (eval-when (<phase> ...) <expression> ...)\n" +
			"Evaluates expressions only during the specified phases.\n" +
			"Phases include expand and compile. Used for phase-specific\n" +
			"side effects and definitions."},
	// R7RS §4.3.1: syntax-error for compile-time errors in macros
	{"syntax-error",
		"Syntax: (syntax-error <message> <irritant> ...)\n" +
			"Signals a compile-time error during macro expansion.\n" +
			"<message> is a string; <irritant>s provide additional context.\n" +
			"R7RS §4.3.1."},
	// R7RS §6.10: dynamic-wind for control flow with cleanup handlers
	{"dynamic-wind",
		"Syntax: (dynamic-wind <before> <thunk> <after>)\n" +
			"Calls <thunk> with <before> and <after> as entry/exit guards.\n" +
			"<before> is called on every entry, <after> on every exit,\n" +
			"including non-local exits via continuations. R7RS §6.10."},
	// R7RS §6.10: apply for procedure application with argument list
	{"apply",
		"Syntax: (apply <proc> <arg1> ... <args>)\n" +
			"Calls <proc> with the given arguments. The last argument must\n" +
			"be a list, whose elements become the tail of the argument list.\n" +
			"R7RS §6.10."},
	// Racket-style continuation marks
	{"with-continuation-mark",
		"Syntax: (with-continuation-mark <key> <value> <expression>)\n" +
			"Evaluates <expression> with <key> mapped to <value> in the\n" +
			"current continuation frame. Used for continuation marks\n" +
			"(stack inspection) and parameterize."},
	// R7RS §4.2.2: binding forms (core compiled, not macros)
	{"let",
		"Syntax: (let ((<var> <init>) ...) <body>) or (let <name> ((<var> <init>) ...) <body>)\n" +
			"Binding form. Evaluates all <init> expressions, then binds them\n" +
			"to <var>s in a new scope for <body>. Named let creates a local\n" +
			"recursive procedure. R7RS §4.2.2."},
	{"let*",
		"Syntax: (let* ((<var> <init>) ...) <body>)\n" +
			"Sequential binding form. Like let, but each <init> is evaluated\n" +
			"in a scope that includes the preceding bindings. R7RS §4.2.2."},
	{"letrec",
		"Syntax: (letrec ((<var> <init>) ...) <body>)\n" +
			"Recursive binding form. All <var>s are bound before any <init>\n" +
			"is evaluated, allowing mutually recursive definitions.\n" +
			"R7RS §4.2.2."},
	{"letrec*",
		"Syntax: (letrec* ((<var> <init>) ...) <body>)\n" +
			"Like letrec, but <init> expressions are evaluated left-to-right,\n" +
			"each in a scope that includes all <var>s. R7RS §4.2.2."},
	// Auxiliary syntax (R7RS §4.2.1, §4.2.5)
	// These are literals used in syntax-rules patterns for cond and case
	{"else",
		"Auxiliary syntax used as a catch-all clause in cond, case, and\n" +
			"cond-expand forms. Not a procedure; cannot be called directly.\n" +
			"R7RS §4.2.1."},
	{"=>",
		"Auxiliary syntax used in cond clauses: (test => <proc>).\n" +
			"When test yields a true value, <proc> is called with that value.\n" +
			"Not a procedure; cannot be called directly. R7RS §4.2.1."},
	// Auxiliary syntax (R7RS §4.3.2)
	// syntax-rules is handled by define-syntax at compile time, but needs
	// a binding for library export resolution (like else, =>, ..., _)
	{"syntax-rules",
		"Syntax: (syntax-rules (<literal> ...) <clause> ...)\n" +
			"Defines a pattern-based macro transformer. Each clause has a\n" +
			"pattern and template; the first matching pattern determines\n" +
			"the expansion. R7RS §4.3.2."},
	// These are special identifiers in syntax-rules patterns
	{"...",
		"Auxiliary syntax for repetition in syntax-rules patterns and\n" +
			"templates. Indicates zero or more repetitions of the preceding\n" +
			"element. Not a procedure; cannot be called directly. R7RS §4.3.2."},
	{"_",
		"Auxiliary syntax for a wildcard in syntax-rules patterns. Matches\n" +
			"any form without binding it. Not a procedure; cannot be called\n" +
			"directly. R7RS §4.3.2."},
}

// macroDocs provides documentation for bootstrap macros defined in Scheme.
// These macros are loaded from bootstrap_macros.scm; their documentation
// is registered here so it's accessible via the REPL's ,doc command.
//
//nolint:govet
var macroDocs = []registry.DocEntry{
	{"and",
		"Syntax: (and <test1> ...)\n" +
			"Short-circuit conjunction. Evaluates tests left-to-right;\n" +
			"returns #f as soon as one yields false, otherwise returns\n" +
			"the value of the last test. R7RS §4.2.1."},
	{"or",
		"Syntax: (or <test1> ...)\n" +
			"Short-circuit disjunction. Evaluates tests left-to-right;\n" +
			"returns the first true value, or #f if all yield false.\n" +
			"R7RS §4.2.1."},
	{"cond",
		"Syntax: (cond <clause1> <clause2> ...)\n" +
			"Multi-way conditional. Each clause is (<test> <expr> ...) or\n" +
			"(<test> => <proc>). Evaluates tests in order; the first true\n" +
			"test's expressions are evaluated. R7RS §4.2.1."},
	{"case",
		"Syntax: (case <key> <clause1> <clause2> ...)\n" +
			"Datum dispatch. Evaluates <key>, then matches it via eqv? against\n" +
			"datum lists in each clause. The matching clause's expressions\n" +
			"are evaluated. R7RS §4.2.1."},
	{"when",
		"Syntax: (when <test> <expression1> <expression2> ...)\n" +
			"One-armed conditional. If <test> is true, evaluates the\n" +
			"expressions in order and returns the last value. Returns\n" +
			"void if <test> is false. R7RS §4.2.1."},
	{"unless",
		"Syntax: (unless <test> <expression1> <expression2> ...)\n" +
			"One-armed conditional. If <test> is false, evaluates the\n" +
			"expressions in order and returns the last value. Returns\n" +
			"void if <test> is true. R7RS §4.2.1."},
	{"do",
		"Syntax: (do ((<var> <init> <step>) ...) (<test> <expr> ...) <command> ...)\n" +
			"Iteration construct. Initializes variables, then repeatedly\n" +
			"evaluates <command>s and advances <step>s until <test> is true.\n" +
			"Returns the value of the last <expr>. R7RS §4.2.4."},
	{"guard",
		"Syntax: (guard (<var> <clause1> ...) <body>)\n" +
			"Exception handling. Evaluates <body>; if an exception is raised,\n" +
			"binds it to <var> and tests cond-style clauses. If no clause\n" +
			"matches, the exception is re-raised. R7RS §4.2.7."},
	{"parameterize",
		"Syntax: (parameterize ((<param> <value>) ...) <body>)\n" +
			"Dynamic binding. Temporarily binds parameter objects to new\n" +
			"values for the dynamic extent of <body>. Restored on exit,\n" +
			"including non-local exits. R7RS §4.2.6."},
	{"delay",
		"Syntax: (delay <expression>)\n" +
			"Creates a promise that will evaluate <expression> when forced.\n" +
			"The result is memoized; subsequent forces return the cached\n" +
			"value. R7RS §4.2.5."},
	{"delay-force",
		"Syntax: (delay-force <expression>)\n" +
			"Creates an iterative lazy promise. Like delay, but <expression>\n" +
			"must return a promise; forcing trampolines through the chain\n" +
			"without stack growth. R7RS §4.2.5."},
	{"define-record-type",
		"Syntax: (define-record-type <name> <constructor> <pred> <field> ...)\n" +
			"Defines a new record type with a constructor, predicate, and\n" +
			"field accessors/mutators. Each <field> is (<name> <accessor>)\n" +
			"or (<name> <accessor> <mutator>). R7RS §5.5."},
	{"let-values",
		"Syntax: (let-values (((<var> ...) <init>) ...) <body>)\n" +
			"Multiple-value binding. Each <init> may return multiple values\n" +
			"which are bound to the corresponding <var>s. All inits are\n" +
			"evaluated before any bindings are created. R7RS §4.2.2."},
	{"let*-values",
		"Syntax: (let*-values (((<var> ...) <init>) ...) <body>)\n" +
			"Sequential multiple-value binding. Like let-values, but each\n" +
			"<init> is evaluated in a scope that includes the preceding\n" +
			"bindings. R7RS §4.2.2."},
	{"define-values",
		"Syntax: (define-values <formals> <expression>)\n" +
			"Multiple-value definition. <expression> must return as many\n" +
			"values as there are variables in <formals>. Supports proper\n" +
			"lists, dotted pairs, and rest patterns. R7RS §5.3.3."},
}

func addSpecialForms(r *registry.Registry) error {
	r.AddBindingSpecs(compileTimeBindingSpecs)
	for _, doc := range macroDocs {
		r.AddDocumentation(doc.Name, doc.Doc)
	}
	return nil
}
