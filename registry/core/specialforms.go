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

// compileTimeBindings are names that exist only at compile time.
// The expander recognizes these as primitive forms and dispatches to
// registered primitive expanders rather than treating them as applications.
var compileTimeBindings = []string{
	"if",
	"lambda",
	"case-lambda",
	"quote",
	"define",
	"define-syntax",
	"set!",
	"begin",
	"include",
	"include-ci",
	"quasiquote",
	"unquote",
	"unquote-splicing",
	"cond-expand",
	"define-for-syntax",
	"begin-for-syntax",
	"eval-when",
	// R7RS §4.3.1: syntax-error for compile-time errors in macros
	"syntax-error",
	// R7RS §6.10: dynamic-wind for control flow with cleanup handlers
	"dynamic-wind",
	// R7RS §6.10: apply for procedure application with argument list
	"apply",
	// Racket-style continuation marks
	"with-continuation-mark",
	// R7RS §4.2.2: binding forms (core compiled, not macros)
	"let",
	"let*",
	"letrec",
	"letrec*",
	// Auxiliary syntax (R7RS §4.2.1, §4.2.5)
	// These are literals used in syntax-rules patterns for cond and case
	"else",
	"=>",
	// Auxiliary syntax (R7RS §4.3.2)
	// syntax-rules is handled by define-syntax at compile time, but needs
	// a binding for library export resolution (like else, =>, ..., _)
	"syntax-rules",
	// These are special identifiers in syntax-rules patterns
	"...",
	"_",
}

func addSpecialForms(r *registry.Registry) error {
	r.AddBindings(compileTimeBindings)
	return nil
}
