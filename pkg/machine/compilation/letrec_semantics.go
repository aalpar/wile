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
// predeclareDefineBindingFromValidated).
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
