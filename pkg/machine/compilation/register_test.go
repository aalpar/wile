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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/forms"
)

// TestCompileValidatedDispatchesThroughPerEngineRegistry proves Tier-1 dispatch
// consults the per-engine forms registry by FormName, not a hardcoded type
// switch. It clones the default registry, overrides "if" with a sentinel
// compiler, and asserts the sentinel runs when (if ...) is compiled on an engine
// using that clone. Under the concrete-type switch this fails (the override is
// ignored); under registry dispatch it passes.
func TestCompileValidatedDispatchesThroughPerEngineRegistry(t *testing.T) {
	reg := forms.DefaultRegistry().Clone()
	called := false
	reg.RegisterCompiler("if", CompilerFunc(func(_ *CompileTimeContinuation, _ CompileTimeCallContext, _ forms.ValidatedExpr) error {
		called = true
		return nil
	}))

	ns := environment.NewNamespace()
	ns.SetFormRegistry(reg)
	env := newNamespace(ns.Runtime())

	prog := parseSchemeExpr(t, env, "(if #t 1 2)")
	_, err := newTopLevelThunk(prog, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, called, qt.IsTrue,
		qt.Commentf("compileValidated did not dispatch (if ...) through the per-engine registry"))
}

func TestTier2CompilersRegisteredInForms(t *testing.T) {
	// Every Tier-2 syntax compiler is attached to its FormSpec (forms registry),
	// replacing the deleted global compilerRegistry.
	for _, entry := range syntaxCompilerEntries {
		spec := forms.DefaultRegistry().Lookup(entry.Name)
		qt.Assert(t, spec, qt.IsNotNil,
			qt.Commentf("no FormSpec for Tier-2 form %q", entry.Name))
		_, ok := spec.Compile.(CompilerFunc)
		qt.Assert(t, ok, qt.IsTrue,
			qt.Commentf("Tier-2 form %q has Compile %T, want CompilerFunc", entry.Name, spec.Compile))
	}
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
// registry: every expand-only form the classification table lists must be an
// actually-registered form. formDispatch is hand-maintained to enumerate the
// forms that legitimately have NO compiler (consumed during expansion);
// VerifyCompilers iterates forms.Names() only, so it catches a form MISSING from
// the table (reported as "no dispatch classification") but NOT a phantom table
// entry that names no real form. This closes that remaining direction.
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

// TestFormDispatchDisjointFromRegistry asserts the expand-only classification and
// the compiler registration are disjoint: an expand-only form must NOT carry a
// spec.Compile. Overlap would be a misclassification.
func TestFormDispatchDisjointFromRegistry(t *testing.T) {
	for name := range formDispatch {
		spec := forms.DefaultRegistry().Lookup(name)
		qt.Assert(t, spec, qt.IsNotNil,
			qt.Commentf("expand-only form %q is not registered", name))
		qt.Assert(t, spec.Compile, qt.IsNil,
			qt.Commentf("expand-only form %q also carries a compiler; one is wrong", name))
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

// TestNoCompilerRegisteredOutsideSharedSlices is the reverse-direction guard:
// every registered form that carries a spec.Compile must originate from one of
// the two shared registration slices (tier1CompilerEntries or
// syntaxCompilerEntries). A compiler attached to any other name — a stray
// forms.RegisterCompiler call — is drift the forward-direction presence tests
// cannot catch (VerifyCompilers only checks each form HAS a compiler-or-expand-only,
// never that a compiler wasn't attached outside the slices). Restores the
// set-equality "no extra registration" half of the deleted
// TestSyntaxCompilerRegistrationConsistency.
func TestNoCompilerRegisteredOutsideSharedSlices(t *testing.T) {
	expected := make(map[string]bool)
	for _, entry := range tier1CompilerEntries {
		expected[entry.Name] = true
	}
	for _, entry := range syntaxCompilerEntries {
		expected[entry.Name] = true
	}
	for _, name := range forms.DefaultRegistry().Names() {
		spec := forms.DefaultRegistry().Lookup(name)
		if spec.Compile == nil {
			continue
		}
		qt.Assert(t, expected[name], qt.IsTrue,
			qt.Commentf("form %q carries a compiler but is in neither tier1CompilerEntries nor syntaxCompilerEntries", name))
	}
}
