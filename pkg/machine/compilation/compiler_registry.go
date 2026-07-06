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

	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/werr"
)

// CompilerFunc is the codegen signature for registry-dispatched forms. Its
// parameter is the ValidatedExpr interface (not the concrete *ValidatedLiteral)
// so one uniform type can hold any form's compiler keyed by FormName; each
// handler asserts its concrete type at its head. Today only Tier-2 (syntax
// passthrough) forms dispatch through the registry — Tier-1 forms still route
// through the concrete-type switch in compileValidated. The interface parameter
// is the enabler for folding Tier-1 into the same registry.
type CompilerFunc func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error

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

// dispatchKind classifies how a special form that has no Tier-2 compiler entry
// is handled. Tier-2 (registry) forms are not classified here — they are exactly
// the keys of compilerRegistry (derived from syntaxCompilerEntries).
type dispatchKind int

const (
	// dispatchTypeSwitch marks a Tier-1 form dispatched by the concrete-type
	// switch in compileValidated (compile_validated.go). Its validator produces
	// a dedicated Validated* type the switch matches directly — no registry
	// entry, no string lookup. Note the name→case relationship is name-level,
	// not 1:1: let/let*/letrec/letrec* all validate to *ValidatedLet.
	dispatchTypeSwitch dispatchKind = iota
	// dispatchExpandOnly marks a form handled entirely during expansion that
	// legitimately has no compiler; it never reaches the compilation phase.
	dispatchExpandOnly
)

// formDispatch is the single classification table for forms that have no Tier-2
// compiler entry: the Tier-1 (type-switch) forms and the expand-only forms. It
// replaces the former parallel typeSwitchForms/expandTimeOnlyForms maps.
// VerifyCompilers derives its skip decision from this table; the TestFormDispatch
// guards keep it honest against the form registry and the compiler registry.
//
// ADDING A NEW FORM: a Tier-1 form (dedicated Validated* type + a case in
// compileValidated) gets a dispatchTypeSwitch entry here; an expand-only form
// gets a dispatchExpandOnly entry. A Tier-2 form needs NO entry here — add it to
// syntaxCompilerEntries instead.
var formDispatch = map[string]dispatchKind{
	// Tier 1 — dispatched by concrete type in compileValidated.
	"if":                     dispatchTypeSwitch,
	"define":                 dispatchTypeSwitch,
	"lambda":                 dispatchTypeSwitch,
	"case-lambda":            dispatchTypeSwitch,
	"set!":                   dispatchTypeSwitch,
	"quote":                  dispatchTypeSwitch,
	"begin":                  dispatchTypeSwitch,
	"quasiquote":             dispatchTypeSwitch,
	"dynamic-wind":           dispatchTypeSwitch,
	"apply":                  dispatchTypeSwitch,
	"with-continuation-mark": dispatchTypeSwitch,
	"let":                    dispatchTypeSwitch,
	"let*":                   dispatchTypeSwitch,
	"letrec":                 dispatchTypeSwitch,
	"letrec*":                dispatchTypeSwitch,

	// Expand-only — handled entirely during expansion; no compiler.
	"let-syntax":           dispatchExpandOnly,
	"letrec-syntax":        dispatchExpandOnly,
	TransformerSyntaxRules: dispatchExpandOnly,
}

// VerifyCompilers checks that every form registered in the forms package has a
// corresponding compiler — either a Tier-2 entry in compilerRegistry or a
// Tier-1/expand-only classification in formDispatch. Returns an error listing
// any form that has neither.
func VerifyCompilers() error {
	var missing []string
	for _, name := range forms.Names() {
		if compilerRegistry[name] != nil {
			continue
		}
		_, classified := formDispatch[name]
		if classified {
			continue
		}
		missing = append(missing, name+": no compiler and no dispatch classification")
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
