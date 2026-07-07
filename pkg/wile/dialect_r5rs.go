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

// r5rsRemovedLibraryForms are the R7RS §5.6 / R6RS library-system forms R5RS lacks —
// R5RS has a flat top level, no module system. import is handled by a REAL expander
// (expandImportForm) with no compiler fall-through, so Remove alone cannot disable it;
// InstallForms applies both Remove (drops the compiler FormSpec) and DisableExpandForm
// (drops the expander's recognition) to each, so every one rejects as an unbound
// identifier. Safe to disable: the eager bootstrap uses none of them and no library is
// expanded at construction (all loading is lazy, reachable only through a user import —
// now rejected). Verified by TestR5RSStrict_LibrarySystemRejected.
var r5rsRemovedLibraryForms = []string{
	"import",         // R7RS §5.6 import
	"define-library", // R7RS §5.6 library definition
	"library",        // R6RS library form
	"export",         // R7RS §5.6 export (only meaningful inside define-library)
}

// R5RSStrict is a dialect that derives from the R7RS baseline ([DefaultDialect]) and
// removes the R6RS/R7RS-only special forms R5RS lacks: the general forms in
// r5rsRemovedForms, the R6RS macro-transformer forms in r5rsRemovedMacroForms, and the
// library-system forms in r5rsRemovedLibraryForms (import/define-library/library/export).
// An engine built with it rejects all of them as unbound identifiers while keeping
// R5RS's flat top level and syntax-rules macro system.
//
// The library system is disabled at the EXPANDER (import is expander-driven with no
// compiler fall-through): safe because the eager bootstrap uses none of these forms and
// no library is expanded at construction — all loading is lazy, reachable only through
// a user import, which is now rejected. So there is no bootstrap-vs-user phase boundary
// to draw; the gate is unconditional. See [BootstrapProcedureRewriter]'s sibling
// mechanism forms.FormRegistry.DisableExpandForm.
//
// Boundary — this is a best-effort strict R5RS, not a certified R5RS. Two things it
// deliberately does not (and, by mechanism, cannot) do:
//
//   - The flat surface is the profile's job, not the dialect's: disabling import means
//     a bare R5RSStrict engine can no longer reach .sld-defined procedures, so its flat
//     top level is only what is pre-bound at construction (core + bootstrap, plus any
//     WithProfile/WithExtension surface). An embedder wanting a fuller R5RS surface flat
//     supplies it via a profile. The dialect says "no library forms"; what is flat is
//     orthogonal.
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
	// Library-system forms are expander-driven (import via a real expandImportForm),
	// so each needs BOTH the compiler-FormSpec drop (Remove) and the expander gate
	// (DisableExpandForm) to reject as unbound.
	for _, name := range r5rsRemovedLibraryForms {
		fr.Remove(name)
		fr.DisableExpandForm(name)
	}
	return nil
}
