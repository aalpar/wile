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
	// Verify they are all listed in typeSwitchForms (used by VerifyCompilers).
	for _, name := range []string{
		"if", "define", "lambda", "set!", "begin", "quote",
		"quasiquote", "case-lambda", "dynamic-wind", "apply",
		"with-continuation-mark", "let", "let*", "letrec", "letrec*",
	} {
		qt.Assert(t, typeSwitchForms[name], qt.IsTrue,
			qt.Commentf("%q not in typeSwitchForms", name))
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

// TestTypeSwitchFormsAreKnownForms cross-checks typeSwitchForms against the form
// registry: every Tier 1 form the map claims compileValidated's type switch
// handles must be an actually-registered form. typeSwitchForms is hand-maintained
// to mirror that switch; VerifyCompilers iterates forms.Names() only, so it catches
// a switch form MISSING from the map (reported as "missing compiler") but NOT a
// phantom map entry that names no real form. This closes that remaining direction.
func TestTypeSwitchFormsAreKnownForms(t *testing.T) {
	known := make(map[string]bool)
	for _, name := range forms.Names() {
		known[name] = true
	}
	for name := range typeSwitchForms {
		qt.Assert(t, known[name], qt.IsTrue,
			qt.Commentf("typeSwitchForms lists %q, which is not a registered form (forms.Names())", name))
	}
}
