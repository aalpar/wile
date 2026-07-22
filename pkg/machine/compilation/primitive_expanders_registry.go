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
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// primitiveExpanderEntries is the single source of truth for all primitive
// expander registrations. Parallels syntaxCompilerEntries in
// syntax_compilers_registry.go.
//
// ADDING A NEW PRIMITIVE EXPANDER: add one entry here.
var primitiveExpanderEntries = []PhaseEntry[PrimitiveExpanderFunc]{
	// Forms that return unchanged (no expansion needed at expand time)
	{"quote", (*ExpanderTimeContinuation).expandUnchanged},
	{"define-syntax", (*ExpanderTimeContinuation).expandUnchanged},
	{"quasiquote", (*ExpanderTimeContinuation).expandUnchanged},
	{"unquote", (*ExpanderTimeContinuation).expandUnchanged},
	{"unquote-splicing", (*ExpanderTimeContinuation).expandUnchanged},
	{"include", (*ExpanderTimeContinuation).expandUnchanged},
	{"include-ci", (*ExpanderTimeContinuation).expandUnchanged},
	{"define-library", (*ExpanderTimeContinuation).expandUnchanged},
	{"cond-expand", (*ExpanderTimeContinuation).expandUnchanged},
	{"syntax", (*ExpanderTimeContinuation).expandUnchanged},
	{"syntax-case", (*ExpanderTimeContinuation).expandUnchanged},
	{TransformerERMacro, (*ExpanderTimeContinuation).expandUnchanged},
	{"quasisyntax", (*ExpanderTimeContinuation).expandUnchanged},
	{"unsyntax", (*ExpanderTimeContinuation).expandUnchanged},
	{"unsyntax-splicing", (*ExpanderTimeContinuation).expandUnchanged},
	{"with-syntax", (*ExpanderTimeContinuation).expandUnchanged},
	{"library", (*ExpanderTimeContinuation).expandUnchanged},
	{"export", (*ExpanderTimeContinuation).expandUnchanged},
	{"meta", (*ExpanderTimeContinuation).expandUnchanged},
	{"define-for-syntax", (*ExpanderTimeContinuation).expandUnchanged},
	{"begin-for-syntax", (*ExpanderTimeContinuation).expandUnchanged},
	{"eval-when", (*ExpanderTimeContinuation).expandUnchanged},
	{"let-syntax", (*ExpanderTimeContinuation).expandLetSyntax},
	{"letrec-syntax", (*ExpanderTimeContinuation).expandLetrecSyntax},

	// Binding scope for hygienic let/letrec macros
	{"with-binding-scope", (*ExpanderTimeContinuation).expandWithBindingScope},

	// R7RS §4.3.1: syntax-error raises compile-time errors
	{"syntax-error", (*ExpanderTimeContinuation).expandSyntaxError},

	// Forms that expand their subexpressions
	{"if", (*ExpanderTimeContinuation).expandIfForm},
	{"begin", (*ExpanderTimeContinuation).expandBeginForm},
	{"set!", (*ExpanderTimeContinuation).expandSetForm},
	{"define", (*ExpanderTimeContinuation).expandDefineForm},
	{"lambda", (*ExpanderTimeContinuation).expandLambdaForm},
	{"case-lambda", (*ExpanderTimeContinuation).expandCaseLambdaForm},
	{"with-continuation-mark", (*ExpanderTimeContinuation).expandWithContinuationMarkForm},
	{"let", (*ExpanderTimeContinuation).expandLetForm},
	{"let*", (*ExpanderTimeContinuation).expandLetStarForm},
	{"letrec", (*ExpanderTimeContinuation).expandLetrecForm},
	{"letrec*", (*ExpanderTimeContinuation).expandLetrecStarForm},

	// Import: loads libraries and makes bindings available during expansion
	{"import", (*ExpanderTimeContinuation).expandImportForm},
}

// RegisterPrimitiveExpanders binds all primitive expanders in the expand-time
// environment (env.Expand()). These are looked up by ExpandPrimitiveForm()
// when the expander encounters a special form.
//
// Each primitive has different expansion behavior:
//   - quote, define-syntax, define-library, quasiquote: return unchanged (no expansion)
//   - if: expand test, consequent, alternative separately
//   - begin: expand all subexpressions
//   - set!: expand only the value expression
//   - define: expand value if simple define
//   - lambda, case-lambda: expand body expressions
//   - syntax-case, cond-expand: return unchanged (compile-time forms)
func RegisterPrimitiveExpanders(env *environment.EnvironmentFrame) error {
	// Install special-form expanders in the phase-1 SEALED EXPAND base (via
	// SealedExpandBaseTarget), not the mutable expand child. A user (define-syntax let-syntax
	// …) then creates a distinct binding in the mutable child (a shadow), instead of reusing
	// and overwriting this slot in place — CreateGlobalBinding dedups by scopeSetsEqual
	// ignoring BindingType, and SetOwnGlobalValue overwrites the Primitive-typed slot's value,
	// which every lookup then rejects (let-syntax, having no Tier-1 fallback, dies). Lookup
	// still finds these via LookupPrimitiveExpander -> env.Expand() walking the parent chain,
	// which after the phaseParent reparent reaches sealedExpandBase. It must NOT be the phase-0
	// SealedBaseTarget(): a compile-time handler there is reachable by RUNTIME value resolution
	// and leaks a dialect-removed form's #<primitive-expander:…> into the value world. For a
	// flat library frame SealedExpandBaseTarget() falls back to env.Expand(), unchanged.
	taproot := func() *environment.EnvironmentFrame {
		return env.SealedExpandBaseTarget()
	}
	return RegisterPhaseBindings(env, taproot, primitiveExpanderEntries,
		func(name string, fn PrimitiveExpanderFunc) values.Value {
			return NewPrimitiveExpander(name, fn)
		})
}

// LookupPrimitiveExpander looks up a primitive expander by symbol in the expand
// environment. Returns the PrimitiveExpander if found, or nil if the symbol does
// not name a primitive expander.
//
// This function handles hygiene by using scoped lookup - it will only match
// bindings whose scopes are a subset of the symbol's scopes.
func LookupPrimitiveExpander(env *environment.EnvironmentFrame, sym *values.Symbol, scopes []*syntax.Scope) *PrimitiveExpander {
	return LookupPhaseBinding[*PrimitiveExpander](env.Expand(), sym, scopes)
}
