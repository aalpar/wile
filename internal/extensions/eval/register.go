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

// Package eval provides eval and environment primitives.
package eval

import (
	"github.com/aalpar/wile/registry"
)

// Extension is the eval extension.
var Extension = registry.NewDescribedExtension("eval",
	"Code evaluation: eval, load, include, macroexpand.",
	AddToRegistry)

// Builder aggregates all eval registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all eval primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "eval", ParamCount: 1, IsVariadic: true, Impl: PrimEval,
			Doc: "Evaluates expression in the current or given environment. With one argument, uses the current namespace.\n\nExamples:\n  (eval '(+ 1 2))                                   => 3\n  (eval '(* 6 7) (scheme-report-environment 7))      => 42", ParamNames: []string{"expr"}, Category: "eval"},
		{Name: "load", ParamCount: 1, Impl: PrimLoad,
			Doc: "Reads and evaluates all expressions from a Scheme source file in the current environment.\n\nExamples:\n  (load \"lib.scm\")  ; evaluates all forms in lib.scm", ParamNames: []string{"filename"}, Category: "eval"},
		{Name: "current-load-path", Impl: PrimCurrentLoadPath,
			Doc: "Returns the full path of the file currently being loaded, or #f if not inside load.\n\nExamples:\n  (current-load-path)  ; inside load => \"/path/to/file.scm\"\n  (current-load-path)  ; at REPL => #f", Category: "eval"},
		{Name: "current-load-directory", Impl: PrimCurrentLoadDirectory,
			Doc: "Returns the directory containing the file currently being loaded, or #f if not inside load.\n\nExamples:\n  (current-load-directory)  ; inside load => \"/path/to\"", Category: "eval"},
		{Name: "current-load-depth", Impl: PrimCurrentLoadDepth,
			Doc: "Returns the current nesting depth of load calls as an exact integer.\n\nExamples:\n  (current-load-depth)  ; at top level => 0", Category: "eval"},
		{Name: "scheme-report-environment", ParamCount: 1, Impl: PrimSchemeReportEnvironment,
			Doc: "Returns the environment specified by the given Scheme version (5 or 7).\n\nExamples:\n  (eval '(+ 1 2) (scheme-report-environment 7))  => 3", ParamNames: []string{"version"}, Category: "eval"},
		{Name: "null-environment", ParamCount: 1, Impl: PrimNullEnvironment,
			Doc: "Returns a fresh environment with no bindings for the given Scheme version (5 or 7).\n\nExamples:\n  (null-environment 7)  => #<environment>", ParamNames: []string{"version"}, Category: "eval"},
		{Name: "environment", ParamCount: 1, IsVariadic: true, Impl: PrimEnvironment,
			Doc: "Creates an environment populated by the given import specs. Each spec names a library to import.\n\nExamples:\n  (eval '(+ 1 2) (environment '(scheme base)))  => 3", ParamNames: []string{"import-spec"}, Category: "eval"},
		{Name: "expand", ParamCount: 1, Impl: PrimExpand,
			Doc: "Fully macro-expands a syntax object or datum. Returns the expanded syntax.\n\nExamples:\n  (expand '(and a b))  ; => expanded form using if", ParamNames: []string{"stx"}, Category: "eval"},
		{Name: "expand-once", ParamCount: 1, Impl: PrimExpandOnce,
			Doc: "Expands one level of macros. Returns two values: the expanded form and a boolean indicating whether expansion occurred.\n\nExamples:\n  (expand-once '(and a b))  ; => (values <one-step expansion> #t)", ParamNames: []string{"stx"}, Category: "eval"},
		{Name: "compile", ParamCount: 1, Impl: PrimCompile,
			Doc: "Compiles an expression to a callable thunk. The thunk can be called with no arguments to evaluate the expression.\n\nExamples:\n  (let ((f (compile '(+ 1 2)))) (f))  => 3", ParamNames: []string{"expr"}, Category: "eval"},
		{Name: "syntax-local-value", ParamCount: 1, Impl: PrimSyntaxLocalValue,
			Doc: "Returns the compile-time value bound to identifier. Only valid during macro expansion.\n\nExamples:\n  ; inside a macro transformer:\n  ; (syntax-local-value #'my-binding)  => <compile-time value>", ParamNames: []string{"id"}, Category: "eval"},
		{Name: "syntax-local-value/immediate", ParamCount: 1, Impl: PrimSyntaxLocalValueImmediate,
			Doc: "Like syntax-local-value but does not chase rename-transformers.\n\nExamples:\n  ; (syntax-local-value/immediate #'id)  => <immediate binding>", ParamNames: []string{"id"}, Category: "eval"},
		{Name: "make-compile-time-value", ParamCount: 1, Impl: PrimMakeCompileTimeValue,
			Doc: "Wraps value for storage as a compile-time binding via define-for-syntax.\n\nExamples:\n  (make-compile-time-value 42)  => #<compile-time-value 42>", ParamNames: []string{"value"}, Category: "eval"},
		{Name: "syntax-local-introduce", ParamCount: 1, Impl: PrimSyntaxLocalIntroduce,
			Doc: "Flips the introduction scope on a syntax object. Used to break hygiene in macro transformers.\n\nExamples:\n  ; inside a macro transformer:\n  ; (syntax-local-introduce #'x)  => x with flipped intro scope", ParamNames: []string{"stx"}, Category: "eval"},
		{Name: "syntax-local-identifier-as-binding", ParamCount: 1, Impl: PrimSyntaxLocalIdentifierAsBinding,
			Doc: "Adds the current use-site scope to an identifier, making it suitable for creating bindings.\n\nExamples:\n  ; inside a macro transformer:\n  ; (syntax-local-identifier-as-binding #'x)  => x with use-site scope", ParamNames: []string{"id"}, Category: "eval"},
	}, registry.PhaseRuntime)
	return nil
}
