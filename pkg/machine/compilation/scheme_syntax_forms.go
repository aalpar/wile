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

	"github.com/aalpar/wile/pkg/environment"
)

// schemeSyntaxFormNames lists the forms bootstrap_syntax.scm defines as macros
// while their Go implementations still exist. It is two things at once (impl
// plan F1/F4): the names RegisterAllPhaseHandlersWithout omits from the phase-1
// sealed registration when the Scheme layer is on — each has a Go expander row
// at (phase 1, sealed), the coordinate a bootstrap define-syntax writes to, and
// MaybeCreateOwnGlobalBinding dedups on scopes and coordinates ignoring
// BindingType, so without the omission the Scheme closure lands in the
// Primitive-typed slot, which lookupMacroBinding (wants BindingTypeSyntax) and
// LookupPrimitiveExpander (wants a *PrimitiveExpander) both reject; the expander
// then leaves the head alone and the compiler dispatches the Go syntax compiler
// by name anyway (headDenotesSpecialForm reads a nil or non-Variable head as the
// form, validate.go), so the Scheme definition is ignored in SILENCE and every
// value assertion stays green — measured. That is why the P1 pin is a counter
// and not a value. It is also the list of rows P3 deletes.
// TestSchemeSyntaxFormsCoverExpanderRows keeps the slice, the expander table,
// and the Scheme file in step.
//
// P1: the six below. P2 appends "syntax-rules" and "er-macro-transformer". P3
// deletes this file with the rows.
var schemeSyntaxFormNames = []string{
	"syntax-case",
	"syntax",
	"with-syntax",
	"quasisyntax",
	"unsyntax",
	"unsyntax-splicing",
}

// SchemeSyntaxFormNames returns a copy of the Scheme-specified form names.
func SchemeSyntaxFormNames() []string {
	return slices.Clone(schemeSyntaxFormNames)
}

// RegisterAllPhaseHandlersWithout is RegisterAllPhaseHandlers with the named
// primitive expanders omitted; the syntax compilers are always registered
// (they are dead once the macro is bound, by dispatch order — Q10).
// RegisterAllPhaseHandlers itself keeps every row and stays the registration for
// callers that build an environment without an Engine (pkg/machine's own tests,
// pkg/registry/testhelpers), so the switch reaches engine-constructed
// environments only; Task 9 gates the two layers on that set.
func RegisterAllPhaseHandlersWithout(env *environment.EnvironmentFrame, exclude []string) error {
	err := RegisterSyntaxCompilers(env)
	if err != nil {
		return err
	}
	return registerPrimitiveExpandersWithout(env, exclude)
}
