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
// forms-layer dialect can disable — forms whose special-form-vs-call decision is made
// by the validator/compiler, which consults the per-engine registry. Each is verified
// effective by TestR5RSStrict_Engine_R5RSWorks_R7RSFormsRejected.
var r5rsRemovedForms = []string{
	"letrec*",                // R6RS/R7RS; R5RS has letrec only
	"cond-expand",            // R7RS §4.2.1 feature test
	"include",                // R7RS §5.6 source inclusion
	"include-ci",             // R7RS §5.6
	"with-continuation-mark", // Racket extension; in neither R5RS nor R7RS-small
}

// r5rsRemovedMacroForms are the R6RS macro-transformer / phasing forms R5RS lacks.
// R5RS's macro system is syntax-rules only (§4.3: define-syntax, let-syntax,
// letrec-syntax, syntax-rules) — those are kept; everything below is R6RS §12 and is
// removed.
//
// Each of these carries a compiler FormSpec (its expand-time entry is a no-op
// passthrough — the real handler is the compiler), so fr.Remove disables it: a use
// becomes an unbound identifier and a define-syntax transformer of that form is
// rejected as an unsupported transformer type. This corrects the earlier belief that
// these were beyond forms-layer removal because "the expander handles them" — the
// expander only passes them through. Each is verified bootstrap-free (the eager stdlib
// uses none of them, only syntax-rules/define-syntax), so removal never breaks engine
// construction, and verified effective by TestR5RSStrict_Engine_R6RSMacroFormsRejected.
var r5rsRemovedMacroForms = []string{
	"syntax-case",       // R6RS §12.4
	"with-syntax",       // R6RS §12.7
	"syntax",            // R6RS §12.4 (#' template)
	"quasisyntax",       // R6RS quasisyntax (#`)
	"unsyntax",          // R6RS (#,)
	"unsyntax-splicing", // R6RS (#,@)
	"begin-for-syntax",  // R6RS phasing
	"define-for-syntax", // R6RS phasing
	"eval-when",         // R6RS phasing
	"meta",              // phase-shift form
}

// R5RSStrict is a dialect that derives from the R7RS baseline ([DefaultDialect]) and
// removes the R6RS/R7RS-only special forms R5RS lacks that the forms layer can disable:
// the general forms in r5rsRemovedForms and the R6RS macro-transformer forms in
// r5rsRemovedMacroForms. An engine built with it rejects them as unbound identifiers
// while keeping R5RS's syntax-rules macro system.
//
// Boundary — this is a best-effort strict R5RS, not a certified R5RS. Three things it
// deliberately does not (and, by mechanism, cannot) do:
//
//   - The library / module system entry point, import, is handled entirely by the
//     EXPANDER (it loads libraries at expand time and has no compiler FormSpec), so
//     forms-layer removal cannot disable it — and it is used pervasively by Wile's own
//     base loading, so disabling it needs a bootstrap-vs-user phase boundary this
//     dialect does not draw. R5RSStrict leaves the library system in place; under it,
//     import fails the way it does on a bare engine (no library registry), NOT as an
//     unbound identifier. Pinned by TestR5RSStrict_ImportNotDisabled_ExpanderCeiling.
//     (The R6RS macro-transformer forms, by contrast, ARE removed — unlike import they
//     carry compiler FormSpecs the forms layer reaches.)
//   - case-lambda is retained even though it is R7RS: Wile's bootstrap stdlib is
//     itself defined using case-lambda, so removing it breaks engine construction.
//     A Wile implementation constraint, not an R5RS statement.
//   - Only special forms are affected. R5RS↔R7RS also differ in the procedure
//     surface (bytevectors, string ports, many procedures), which are primitives a
//     forms-layer removal cannot reach; those remain (see [NoMutation] for the
//     primitive-removal mechanism). And several R5RS/R7RS constructs (cond, case, and,
//     or, do, when, unless, …) are syntax-rules macros in the stdlib, not registry
//     forms, so this dialect does not distinguish R5RS from R7RS among them.
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
	for _, name := range r5rsRemovedMacroForms {
		fr.Remove(name)
	}
	return nil
}
