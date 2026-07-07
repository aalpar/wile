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

package wile

import (
	"github.com/aalpar/wile/pkg/internal/forms"
)

// r5rsRemovedForms are the R6RS/R7RS/Racket-only special forms R5RS lacks that a
// forms-only dialect can actually disable — i.e. forms whose special-form-vs-call
// decision is made by the validator (which consults the per-engine registry).
// Each is verified effective by TestR5RSStrict_Engine_R5RSWorks_R7RSFormsRejected.
//
// The list is deliberately narrow. Many other R7RS/R6RS forms cannot be removed
// this way because the expander handles them before validation and does not
// consult the forms registry — see the R5RSStrict "Boundary" note.
var r5rsRemovedForms = []string{
	"letrec*",                // R6RS/R7RS; R5RS has letrec only
	"cond-expand",            // R7RS §4.2.1 feature test
	"include",                // R7RS §5.6 source inclusion
	"include-ci",             // R7RS §5.6
	"with-continuation-mark", // Racket extension; in neither R5RS nor R7RS-small
}

// R5RSStrict is a dialect that derives from the R7RS baseline ([DefaultDialect])
// and removes the R6RS/R7RS-only special forms R5RS lacks that the forms layer can
// actually disable (see r5rsRemovedForms), so an engine built with it rejects
// them as unbound identifiers.
//
// Boundary — this is a best-effort, forms-layer strict R5RS, not a certified R5RS.
// Three things it deliberately does not (and, by mechanism, cannot) do:
//
//   - The library / module system (import, export, define-library, library) and
//     the R6RS macro-transformer forms (syntax-case, with-syntax, syntax,
//     quasisyntax, unsyntax, unsyntax-splicing, begin-for-syntax, define-for-syntax,
//     eval-when, meta) are handled by the expander, which runs before validation
//     and does not consult the per-engine forms registry. Removing them from the
//     registry therefore does not disable well-formed uses, so R5RSStrict leaves
//     them in place rather than claim a removal that does not take effect.
//     (A few, e.g. define-library, are technically validator-gated, but the
//     system's entry point import is not, so the whole system is left intact
//     rather than rejecting an inconsistent fragment.) Disabling these needs
//     expander-level dialect control, which does not exist yet.
//   - case-lambda is retained even though it is R7RS: Wile's bootstrap stdlib is
//     itself defined using case-lambda, so removing it breaks engine construction.
//     A Wile implementation constraint, not an R5RS statement.
//   - Only special forms are affected. R5RS↔R7RS also differ in the procedure
//     surface (bytevectors, string ports, many procedures), which are primitives
//     the forms-only Dialect cannot reach; those remain. And several R5RS/R7RS
//     constructs (cond, case, and, or, do, when, unless, …) are syntax-rules macros
//     in the stdlib, not registry forms, so this dialect does not distinguish R5RS
//     from R7RS among them.
var R5RSStrict Dialect = r5rsStrictDialect{}

// r5rsStrictDialect is the concrete R5RSStrict — see R5RSStrict.
type r5rsStrictDialect struct{}

func (r5rsStrictDialect) Name() string {
	return "r5rs-strict"
}

func (r5rsStrictDialect) InstallForms(fr *forms.FormRegistry) error {
	for _, name := range r5rsRemovedForms {
		fr.Remove(name)
	}
	return nil
}
