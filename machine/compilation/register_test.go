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
