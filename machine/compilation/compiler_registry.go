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

package compilation

import (
	"slices"
	"strings"

	"github.com/aalpar/wile/internal/forms"
	"github.com/aalpar/wile/internal/validate"
	"github.com/aalpar/wile/werr"
)

// CompilerFunc is the signature for Tier 2 (syntax passthrough) compiler
// functions. These receive ValidatedLiteral because Tier 2 forms pass through
// validation as literals with a FormName. Tier 1 forms (if, define, lambda,
// etc.) are dispatched by type switch in compileValidated and never reach
// the registry.
type CompilerFunc func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, expr *validate.ValidatedLiteral) error

// compilerRegistry maps form names to their Tier 2 compiler functions.
var compilerRegistry = make(map[string]CompilerFunc)

// registerCompiler adds a compiler function to the registry.
func registerCompiler(name string, fn CompilerFunc) {
	compilerRegistry[name] = fn
}

// LookupCompiler returns the compiler function for a form name, or nil.
func LookupCompiler(name string) CompilerFunc {
	return compilerRegistry[name]
}

// typeSwitchForms are Tier 1 forms dispatched by the type switch in
// compileValidated. They have no registry entry because the concrete
// ValidatedExpr type carries the dispatch information directly.
var typeSwitchForms = map[string]bool{
	"if":                     true,
	"define":                 true,
	"lambda":                 true,
	"case-lambda":            true,
	"set!":                   true,
	"quote":                  true,
	"begin":                  true,
	"quasiquote":             true,
	"dynamic-wind":           true,
	"apply":                  true,
	"with-continuation-mark": true,
	"let":                    true,
	"let*":                   true,
	"letrec":                 true,
	"letrec*":                true,
}

// expandTimeOnlyForms are forms handled entirely during expansion that
// legitimately have no compiler. They never reach the compilation phase.
var expandTimeOnlyForms = map[string]bool{
	"let-syntax":    true,
	"letrec-syntax": true,
	"syntax-rules":  true,
}

// VerifyCompilers checks that every form registered in the forms package
// has a corresponding compiler — either in the registry (Tier 2) or in
// the type switch (Tier 1). Returns an error listing any gaps.
func VerifyCompilers() error {
	var missing []string
	for _, name := range forms.Names() {
		if expandTimeOnlyForms[name] || typeSwitchForms[name] {
			continue
		}
		if compilerRegistry[name] == nil {
			missing = append(missing, name+": missing compiler")
		}
	}
	return formatMissing("compiler", missing)
}

// VerifyExpanders checks that every Tier 2 syntax compiler entry has a
// corresponding primitive expander entry. A Tier 2 form without an expander
// is silently treated as a procedure call during expansion — the most
// dangerous form of registration drift.
func VerifyExpanders() error {
	expanderNames := make(map[string]bool, len(primitiveExpanderEntries))
	for _, e := range primitiveExpanderEntries {
		expanderNames[e.Name] = true
	}

	var missing []string
	for _, e := range syntaxCompilerEntries {
		if !expanderNames[e.Name] {
			missing = append(missing, e.Name+": syntax compiler has no expander")
		}
	}
	return formatMissing("expander", missing)
}

func formatMissing(kind string, missing []string) error {
	if len(missing) == 0 {
		return nil
	}
	slices.Sort(missing)
	var b strings.Builder
	for _, m := range missing {
		b.WriteString("\n  ")
		b.WriteString(m)
	}
	return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
		"%s registration inconsistencies:%s", kind, b.String())
}
