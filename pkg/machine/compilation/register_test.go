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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/internal/forms"
)

func TestTypeSwitchFormsRegistered(t *testing.T) {
	// Tier 1 forms are dispatched by type switch in compileValidated.
	// Verify they are all classified dispatchTypeSwitch in formDispatch
	// (used by VerifyCompilers to skip them).
	for _, name := range []string{
		"if", "define", "lambda", "set!", "begin", "quote",
		"quasiquote", "case-lambda", "dynamic-wind", "apply",
		"with-continuation-mark", "let", "let*", "letrec", "letrec*",
	} {
		kind, ok := formDispatch[name]
		qt.Assert(t, ok, qt.IsTrue,
			qt.Commentf("%q not in formDispatch", name))
		qt.Assert(t, kind, qt.Equals, dispatchTypeSwitch,
			qt.Commentf("%q classified %v, want dispatchTypeSwitch", name, kind))
	}
}

func TestRegisterCompilers(t *testing.T) {
	// Tier 2 syntax passthrough compilers are registered in compilerRegistry
	// via init() from syntaxCompilerEntries. Verify LookupCompiler returns
	// non-nil for every entry in the shared slice.
	for _, entry := range syntaxCompilerEntries {
		qt.Assert(t, LookupCompiler(entry.Name), qt.IsNotNil,
			qt.Commentf("LookupCompiler(%q) returned nil — init() and syntaxCompilerEntries out of sync", entry.Name))
	}
}

func TestSyntaxCompilerRegistrationConsistency(t *testing.T) {
	// Verify compilerRegistry contains exactly the entries from
	// syntaxCompilerEntries (no extra, no missing). This guards against
	// someone adding a registerCompiler call outside the shared slice.
	entryNames := make(map[string]bool, len(syntaxCompilerEntries))
	for _, entry := range syntaxCompilerEntries {
		entryNames[entry.Name] = true
	}

	for name := range compilerRegistry {
		qt.Assert(t, entryNames[name], qt.IsTrue,
			qt.Commentf("compilerRegistry has %q which is not in syntaxCompilerEntries", name))
	}

	qt.Assert(t, len(compilerRegistry), qt.Equals, len(syntaxCompilerEntries),
		qt.Commentf("compilerRegistry has %d entries but syntaxCompilerEntries has %d",
			len(compilerRegistry), len(syntaxCompilerEntries)))
}

func TestLookupCompilerMiss(t *testing.T) {
	qt.Assert(t, LookupCompiler("definitely-not-a-form"), qt.IsNil)
}

func TestVerifyAllPhaseHandlers(t *testing.T) {
	err := VerifyAllPhaseHandlers()
	qt.Assert(t, err, qt.IsNil)
}

func TestVerifyExpanders_SyntaxCompilersHaveExpanders(t *testing.T) {
	// Every syntax compiler entry must have a corresponding expander.
	// A Tier 2 form without an expander is silently treated as a
	// procedure call during expansion.
	expanderNames := make(map[string]bool, len(primitiveExpanderEntries))
	for _, e := range primitiveExpanderEntries {
		expanderNames[e.Name] = true
	}
	for _, e := range syntaxCompilerEntries {
		qt.Assert(t, expanderNames[e.Name], qt.IsTrue,
			qt.Commentf("syntax compiler %q has no primitive expander entry", e.Name))
	}
}

// TestFormDispatchAreKnownForms cross-checks formDispatch against the form
// registry: every form the classification table claims to handle (Tier 1 or
// expand-only) must be an actually-registered form. formDispatch is
// hand-maintained to mirror the type switch / expander; VerifyCompilers iterates
// forms.Names() only, so it catches a form MISSING from the table (reported as
// "no dispatch classification") but NOT a phantom table entry that names no real
// form. This closes that remaining direction.
func TestFormDispatchAreKnownForms(t *testing.T) {
	known := make(map[string]bool)
	for _, name := range forms.Names() {
		known[name] = true
	}
	for name := range formDispatch {
		qt.Assert(t, known[name], qt.IsTrue,
			qt.Commentf("formDispatch lists %q, which is not a registered form (forms.Names())", name))
	}
}

// TestFormDispatchDisjointFromRegistry asserts the classification table and the
// Tier-2 compiler registry are disjoint: a formDispatch entry (Tier 1 or
// expand-only) must NOT also carry a compilerRegistry compiler. Overlap would be
// a misclassification — VerifyCompilers skips a name the moment it finds a
// registry entry, so a stale formDispatch entry would silently mask a Tier-2
// form's identity. This is the "assert set-equality" guard from the sweep plan.
func TestFormDispatchDisjointFromRegistry(t *testing.T) {
	for name := range formDispatch {
		qt.Assert(t, LookupCompiler(name), qt.IsNil,
			qt.Commentf("formDispatch classifies %q but it also has a Tier-2 compiler; one is wrong", name))
	}
}

func TestTier1CompilersRegisteredInForms(t *testing.T) {
	// After init(), every Tier-1 form name carries a CompilerFunc on its FormSpec.
	for _, entry := range tier1CompilerEntries {
		spec := forms.DefaultRegistry().Lookup(entry.Name)
		qt.Assert(t, spec, qt.IsNotNil,
			qt.Commentf("no FormSpec for Tier-1 form %q", entry.Name))
		_, ok := spec.Compile.(CompilerFunc)
		qt.Assert(t, ok, qt.IsTrue,
			qt.Commentf("Tier-1 form %q has Compile %T, want CompilerFunc", entry.Name, spec.Compile))
	}
}
