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

// letrec_semantics.go provides the shared binding pre-declaration used by
// R7RS §5.3.2 letrec* body semantics. In Scheme, bodies (lambda, library,
// begin, include) use letrec* semantics: all defined names are visible
// throughout the body, enabling forward references between defines.
//
// WHICH CONTEXTS ARE letrec*, PER THE SPEC — the list is closed, and the top
// level is NOT in it. R7RS §5.3.2 (p.26) enumerates: "the body of a lambda,
// let, let*, letrec, letrec*, let-values, let*-values, let-syntax,
// letrec-syntax, parameterize, guard, or case-lambda" (plus bodies that only
// become apparent after expanding other syntax). §5.3.2 also gives the rule
// that unifies begin: "Wherever an internal definition can occur, (begin
// ⟨definition₁⟩ …) is equivalent to the sequence of definitions that form the
// body of the begin" — begin SPLICES INTO ITS CONTEXT and never forms a region
// of its own. At the outermost level §4.2.3 (p.17) says the same from the other
// side: a top-level begin's contents are evaluated "exactly as if the enclosing
// begin construct were not present".
//
// This matters here because Engine.EvalProgram (pkg/wile/engine.go, wrapInBegin)
// splices EVERY --file/-e/--check/MCP program into one (begin …). That wrapper
// must stay semantically invisible; giving it body semantics makes a define on
// the last line visible on the first, which §4.2.3 forbids. Measured 2026-08-11
// on `(define captured (let () 5))` + `(define let 3)`, sequential vs wrapped:
// Racket 9.2 (racket -I r5rs) 5/5 and Wile master 5/5 both conform; Petite Chez
// 10.4.1 gives 5 then "Exception: invalid syntax ()" and DEVIATES. Do not take
// Chez as the reference for this rule.
//
// A LIBRARY BODY IS NOT STATED TO BE letrec* EITHER, contrary to common belief.
// §5.6.1 (p.28) leaves it out of §5.3.2's list, calls its begin "analogous to,
// but not the same as, the two types of begin defined in section 4.2.3", and
// says its expressions are expanded "in the order in which they occur" and
// executed "in textual order". Treat library-body letrec* as an implementation
// choice to be justified, not as a requirement to be preserved.
//
// WHAT A FORWARD REFERENCE ACTUALLY NEEDS is a global INDEX, not a binding:
// CompileSymbol's zero-scope arm raises ErrNoSuchBinding only when
// GetGlobalIndexWithScopes returns nil, and when the index exists with no
// binding it already emits a runtime-resolution load ("Binding not yet defined
// at compile time"). predeclareBinding below creates a full BindingTypeVariable,
// which is more than that path requires — and the surplus is what the
// validator's headDenotesSpecialForm reads as a variable shadowing a keyword.
// Top-level mutual recursion never needed letrec* at all: the reference sits in
// a lambda body and resolves when the procedure is called.
//
// The letrec* algorithm has two passes:
//   1. Pre-scan: walk forms, detect define forms, register placeholder bindings
//   2. Process: expand or compile each form sequentially (with all names visible)
//
// This pattern appears in several places with different input types and phases:
//
//   Expander (expansion-time):
//     expander_body.go: ExpandBodyWithDefineSyntax
//     Input: []syntax.SyntaxValue (pre-expansion)
//     Pre-scan: extractDefineName -> predeclareBinding
//     Process: ExpandExpression + compile define-syntax eagerly
//
//   Compiler (include/library bodies):
//     compile_time_continuation_include.go: processFormsWithLetrecSemantics
//     compile_time_continuation_library.go: compileLibraryBegin (delegates to processFormsWithLetrecSemantics)
//     Input: []syntax.SyntaxValue (post-expansion)
//     Pre-scan: extractDefineName -> predeclareBinding (via predeclareDefineBinding)
//     Process: CompileExpression
//
//   Compiler (lambda/begin bodies):
//     compile_closure.go: compileBody
//     compile_validated.go: CompileValidatedBegin
//     Input: []validate.ValidatedExpr (post-validation)
//     Pre-scan: ValidatedDefine type check -> predeclareBinding
//     Process: compileValidated / compileValidatedSequence
//
// Full generic unification was evaluated (see
// memory/2026-06-15-tech-debt-remediation.local.md)
// and rejected: type heterogeneity (SyntaxValue vs ValidatedExpr), different
// receiver types (ExpanderTimeContinuation vs CompileTimeContinuation), and
// phase differences (expansion vs compilation) make a generic scanner more
// complex than the duplication it eliminates. Instead, the shared binding
// creation logic is extracted as predeclareBinding below, while each site
// retains its own two-pass loop with phase-appropriate name extraction and
// form processing.

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// predeclareBinding creates a placeholder binding for a defined name,
// enabling forward references within bodies per R7RS §5.3.2 letrec* semantics.
//
// If a local environment exists, the binding is created there (lambda bodies).
// Otherwise, the binding is created as a global (library/top-level bodies).
// Scopes and source context are attached for macro hygiene tracking.
//
// This is the shared implementation used by both the expander
// (ExpandBodyWithDefineSyntax) and the compiler (predeclareDefineBinding,
// predeclareDefineFromValidatedRecursive).
func predeclareBinding(env *environment.EnvironmentFrame, name *values.Symbol, scopes []*syntax.Scope, source *syntax.SourceContext) {
	if env.LocalEnvironment() != nil {
		_, _ = env.MaybeCreateLocalBinding(name, environment.BindingTypeVariable, scopes, source)
		return
	}
	// Create global binding if no local environment exists. This is the case for library and top-level bodies, which
	// use the global environment for internal definitions.
	// The scope set is the slot's KEY, so it goes in at CREATION. Creating under
	// nil and stamping m.Scopes afterwards keyed the slot under the empty set and
	// then made it report a set it was never keyed under, so the next predeclare
	// of the same name mis-deduplicated against it. newGlobalBinding records the
	// set for us, which is why nothing below writes m.Scopes.
	//
	// The returned index is PINNED to the slot this call landed on, at THIS view's
	// own coordinates, so the source stamp below cannot land on another slot of the
	// same name — one belonging to another expansion, or to the sealed startup set.
	ownIndex, _ := env.MaybeCreateOwnGlobalBinding(name, environment.BindingTypeVariable, scopes)
	binding := env.GlobalEnvironment().GetOwnGlobalBinding(ownIndex)
	if binding == nil || source == nil {
		return
	}
	binding.UpdateMeta(func(m *environment.BindingMeta) bool {
		m.Source = source
		return true
	})
}
