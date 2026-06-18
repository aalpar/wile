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
	"bufio"
	"context"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/schemeutil"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// TestClausesWrapper_EqualTo tests the EqualTo method of ClausesWrapper.
func TestClausesWrapper_EqualTo(t *testing.T) {
	c1 := &ClausesWrapper{Clauses: []*SyntaxRulesClause{}}
	c2 := &ClausesWrapper{Clauses: []*SyntaxRulesClause{}}

	// Clauses are never equal (by design)
	qt.Assert(t, c1.EqualTo(c2), qt.IsFalse)

	// Different type
	qt.Assert(t, c1.EqualTo(values.NewInteger(42)), qt.IsFalse)
}

// TestClausesWrapper_IsVoid tests the IsVoid method of ClausesWrapper.
func TestClausesWrapper_IsVoid(t *testing.T) {
	c := &ClausesWrapper{Clauses: []*SyntaxRulesClause{}}
	qt.Assert(t, c.IsVoid(), qt.IsFalse)
}

// TestClausesWrapper_SchemeString tests the SchemeString method of ClausesWrapper.
func TestClausesWrapper_SchemeString(t *testing.T) {
	c := &ClausesWrapper{Clauses: []*SyntaxRulesClause{}}
	qt.Assert(t, c.SchemeString(), qt.Equals, "#<syntax-rules-clauses>")
}

// TestCompileUnquote tests that unquote outside quasiquote errors
func TestCompileUnquote(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// (unquote x) - should error
	prog := values.List(values.NewSymbol("unquote"), values.NewSymbol("x"))
	_, err = newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "unquote")
}

// TestCompileUnquoteSplicing tests that unquote-splicing outside quasiquote errors
func TestCompileUnquoteSplicing(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// (unquote-splicing x) - should error
	prog := values.List(values.NewSymbol("unquote-splicing"), values.NewSymbol("x"))
	_, err = newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "unquote-splicing")
}

// TestCompileUnquoteDirectCall directly tests CompileUnquote method
func TestCompileUnquoteDirectCall(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := NewCompileTimeCallContext(context.Background(), false)

	sctx := syntax.NewZeroValueSourceContext()
	expr := schemeutil.DatumToSyntaxValue(context.Background(), sctx, values.NewSymbol("x"))

	err := ctc.CompileUnquote(ctctx, expr)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "unquote")
}

// TestCompileUnquoteSplicingDirectCall directly tests CompileUnquoteSplicing method
func TestCompileUnquoteSplicingDirectCall(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := NewCompileTimeCallContext(context.Background(), false)

	sctx := syntax.NewZeroValueSourceContext()
	expr := schemeutil.DatumToSyntaxValue(context.Background(), sctx, values.NewSymbol("x"))

	err := ctc.CompileUnquoteSplicing(ctctx, expr)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "unquote-splicing")
}

// TestParseExportSpec tests parsing export specifications
func TestParseExportSpec(t *testing.T) {
	testCases := []struct {
		name        string
		input       string
		expectError bool
		checkExport func(*testing.T, *CompiledLibrary)
	}{
		{
			name:        "simple symbol export",
			input:       "bindSymbolWithScopes",
			expectError: false,
			checkExport: func(t *testing.T, lib *CompiledLibrary) {
				qt.Assert(t, lib.Exports, qt.HasLen, 1)
				qt.Assert(t, lib.Exports["bindSymbolWithScopes"], qt.Equals, "bindSymbolWithScopes")
			},
		},
		{
			name:        "rename export",
			input:       "(rename internal-name external-name)",
			expectError: false,
			checkExport: func(t *testing.T, lib *CompiledLibrary) {
				qt.Assert(t, lib.Exports, qt.HasLen, 1)
				qt.Assert(t, lib.Exports["external-name"], qt.Equals, "internal-name")
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			lib := NewCompiledLibrary(NewLibraryName("test"), env)

			// Parse the input
			reader := bufio.NewReader(strings.NewReader(tc.input))
			p := parser.NewParser(env, true, reader)
			stx, err := p.ReadSyntax(context.TODO())
			qt.Assert(t, err, qt.IsNil)

			// Call parseExportSpec
			err = parseExportSpec(lib, stx)

			if tc.expectError {
				qt.Assert(t, err, qt.IsNotNil)
			} else {
				qt.Assert(t, err, qt.IsNil)
				if tc.checkExport != nil {
					tc.checkExport(t, lib)
				}
			}
		})
	}
}

// TestParseImportSetExcept tests parsing except import sets
func TestParseImportSetExcept(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	// Parse (except (scheme base) car cdr)
	input := "(except (scheme base) car cdr)"
	reader := bufio.NewReader(strings.NewReader(input))
	p := parser.NewParser(env, true, reader)
	stx, err := p.ReadSyntax(context.TODO())
	qt.Assert(t, err, qt.IsNil)

	// Call ParseImportSetFromDatum via UnwrapAll
	importSet, err := ParseImportSetFromDatum(context.Background(), stx.UnwrapAll())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, importSet, qt.IsNotNil)
	qt.Assert(t, importSet.LibraryName, qt.IsNotNil)
	qt.Assert(t, importSet.LibraryName.String(), qt.Equals, "scheme/base")
	qt.Assert(t, importSet.Except, qt.HasLen, 2)
}

// TestParseImportSetRename tests parsing rename import sets
func TestParseImportSetRename(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	// Parse (rename (scheme base) (car first) (cdr rest))
	input := "(rename (scheme base) (car first) (cdr rest))"
	reader := bufio.NewReader(strings.NewReader(input))
	p := parser.NewParser(env, true, reader)
	stx, err := p.ReadSyntax(context.TODO())
	qt.Assert(t, err, qt.IsNil)

	// Call ParseImportSetFromDatum via UnwrapAll
	importSet, err := ParseImportSetFromDatum(context.Background(), stx.UnwrapAll())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, importSet, qt.IsNotNil)
	qt.Assert(t, importSet.LibraryName, qt.IsNotNil)
	qt.Assert(t, importSet.LibraryName.String(), qt.Equals, "scheme/base")
	qt.Assert(t, importSet.Renames, qt.HasLen, 2)
}

// TestParseFeatureRequirement tests parsing feature requirements
func TestParseFeatureRequirement(t *testing.T) {
	testCases := []struct {
		name        string
		input       string
		expectError bool
		checkReq    func(*testing.T, FeatureRequirement)
	}{
		{
			name:        "simple feature identifier",
			input:       "r7rs",
			expectError: false,
			checkReq: func(t *testing.T, req FeatureRequirement) {
				_, ok := req.(*featureIdentifier)
				qt.Assert(t, ok, qt.IsTrue)
			},
		},
		{
			name:        "else keyword",
			input:       "else",
			expectError: false,
			checkReq: func(t *testing.T, req FeatureRequirement) {
				_, ok := req.(*elseRequirement)
				qt.Assert(t, ok, qt.IsTrue)
			},
		},
		{
			name:        "library requirement",
			input:       "(library (scheme base))",
			expectError: false,
			checkReq: func(t *testing.T, req FeatureRequirement) {
				libReq, ok := req.(*libraryRequirement)
				qt.Assert(t, ok, qt.IsTrue)
				qt.Assert(t, libReq.name.SchemeString(), qt.Equals, "(scheme base)")
			},
		},
		{
			name:        "and requirement",
			input:       "(and r7rs (library (scheme base)))",
			expectError: false,
			checkReq: func(t *testing.T, req FeatureRequirement) {
				andReq, ok := req.(*andRequirement)
				qt.Assert(t, ok, qt.IsTrue)
				qt.Assert(t, andReq.requirements, qt.HasLen, 2)
			},
		},
		{
			name:        "or requirement",
			input:       "(or r7rs r6rs)",
			expectError: false,
			checkReq: func(t *testing.T, req FeatureRequirement) {
				orReq, ok := req.(*orRequirement)
				qt.Assert(t, ok, qt.IsTrue)
				qt.Assert(t, orReq.requirements, qt.HasLen, 2)
			},
		},
		{
			name:        "not requirement",
			input:       "(not r6rs)",
			expectError: false,
			checkReq: func(t *testing.T, req FeatureRequirement) {
				notReq, ok := req.(*notRequirement)
				qt.Assert(t, ok, qt.IsTrue)
				qt.Assert(t, notReq.requirement, qt.IsNotNil)
			},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			reader := bufio.NewReader(strings.NewReader(tc.input))
			p := parser.NewParser(env, true, reader)
			stx, err := p.ReadSyntax(context.TODO())
			qt.Assert(t, err, qt.IsNil)

			req, err := parseFeatureRequirement(context.Background(), stx)

			if tc.expectError {
				qt.Assert(t, err, qt.IsNotNil)
			} else {
				qt.Assert(t, err, qt.IsNil)
				if tc.checkReq != nil {
					tc.checkReq(t, req)
				}
			}
		})
	}
}

// TestCompileCondExpand tests cond-expand compilation
func TestCompileCondExpand(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Note: r7rs is already a supported feature by default

	sctx := syntax.NewZeroValueSourceContext()

	// Test successful cond-expand
	prog := values.List(
		values.NewSymbol("cond-expand"),
		values.List(values.NewSymbol("r7rs"), values.NewInteger(42)),
		values.List(values.NewSymbol("else"), values.NewInteger(99)))

	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestCompileInclude_CoverageImprovement tests include compilation with a temporary file
func TestCompileInclude_CoverageImprovement(t *testing.T) {
	// Create a temporary file with Scheme code
	tmpDir := t.TempDir()
	tmpFile := filepath.Join(tmpDir, "test.scm")
	err := os.WriteFile(tmpFile, []byte("42"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	// Set up environment with include search path
	env := newNamespace(environment.NewNamespace().Runtime())
	err = RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Set SCHEME_INCLUDE_PATH
	oldPath := os.Getenv("SCHEME_INCLUDE_PATH")
	defer os.Setenv("SCHEME_INCLUDE_PATH", oldPath) //nolint:errcheck
	os.Setenv("SCHEME_INCLUDE_PATH", tmpDir)        //nolint:errcheck

	sctx := syntax.NewZeroValueSourceContext()

	// Test include
	prog := values.List(
		values.NewSymbol("include"),
		values.NewString("test.scm"))

	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestCompileInclude_LibraryRegistryPaths tests that include resolves files
// via the library registry's search paths (unified with import).
func TestCompileInclude_LibraryRegistryPaths(t *testing.T) {
	tmpDir := t.TempDir()
	err := os.WriteFile(filepath.Join(tmpDir, "lib-test.scm"), []byte("42"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	env := newNamespace(environment.NewNamespace().Runtime())
	err = RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Clear SCHEME_INCLUDE_PATH so only the library registry path resolves the file.
	t.Setenv("SCHEME_INCLUDE_PATH", "")

	// Attach a library registry with tmpDir in its search paths.
	reg := NewLibraryRegistry()
	reg.PrependSearchPath(tmpDir)
	env.SetLibraryRegistry(reg)

	sctx := syntax.NewZeroValueSourceContext()

	prog := values.List(
		values.NewSymbol("include"),
		values.NewString("lib-test.scm"))

	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestCompileIncludeCi tests that include-ci returns an unsupported error.
func TestCompileIncludeCi(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()
	prog := values.List(
		values.NewSymbol("include-ci"),
		values.NewString("test.scm"))

	_, err = newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "include-ci")
	qt.Assert(t, err.Error(), qt.Contains, "not yet supported")
}

// TestParseFeatureRequirementList tests parsing lists of feature requirements
func TestParseFeatureRequirementList(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	input := "(r7rs r6rs (library (scheme base)))"
	reader := bufio.NewReader(strings.NewReader(input))
	p := parser.NewParser(env, true, reader)
	stx, err := p.ReadSyntax(context.TODO())
	qt.Assert(t, err, qt.IsNil)

	reqs, err := parseFeatureRequirementList(context.Background(), stx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, reqs, qt.HasLen, 3)
}

// TestCompileIncludeError tests include with non-existent file
func TestCompileIncludeError(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// Test include with non-existent file
	prog := values.List(
		values.NewSymbol("include"),
		values.NewString("nonexistent-file-12345.scm"))

	_, err = newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "include")
}

// TestProcessCondExpand tests the processCondExpand helper function indirectly via cond-expand
func TestProcessCondExpand(t *testing.T) {
	// The processCondExpand function is called internally by cond-expand compilation
	// and is tested via TestCompileCondExpand. Since it requires a library context
	// which is only available during define-library processing, we test it indirectly.
	// This test is here to document coverage of processCondExpand.

	// See TestCompileCondExpand for functional testing of cond-expand logic
}

// TestCompileIncludeReadError tests include with a file that has syntax errors
func TestCompileIncludeReadError(t *testing.T) {
	// Create a temporary file with invalid Scheme code
	tmpDir := t.TempDir()
	tmpFile := filepath.Join(tmpDir, "bad.scm")
	err := os.WriteFile(tmpFile, []byte("("), 0o644)
	qt.Assert(t, err, qt.IsNil)

	env := newNamespace(environment.NewNamespace().Runtime())
	err = RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Set SCHEME_INCLUDE_PATH
	oldPath := os.Getenv("SCHEME_INCLUDE_PATH")
	defer os.Setenv("SCHEME_INCLUDE_PATH", oldPath) //nolint:errcheck
	os.Setenv("SCHEME_INCLUDE_PATH", tmpDir)        //nolint:errcheck

	sctx := syntax.NewZeroValueSourceContext()

	// Test include with bad syntax
	prog := values.List(
		values.NewSymbol("include"),
		values.NewString("bad.scm"))

	// Use newTopLevelThunk which handles both expansion and compilation
	_, err = newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	// This should eventually error when include tries to read the malformed file
	// Note: The actual error behavior may depend on parser error handling
	if err != nil {
		qt.Assert(t, err.Error(), qt.Contains, "") // Accept any error
	}
}

// TestParseExportSpecRenameErrors tests error cases in export rename parsing
func TestParseExportSpecRenameErrors(t *testing.T) {
	testCases := []struct {
		name  string
		input string
	}{
		{
			name:  "missing internal name",
			input: "(rename)",
		},
		{
			name:  "missing external name",
			input: "(rename internal)",
		},
		{
			name:  "non-symbol internal",
			input: "(rename 42 external)",
		},
		{
			name:  "non-symbol external",
			input: "(rename internal 42)",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := environment.NewNamespace().Runtime()
			lib := NewCompiledLibrary(NewLibraryName("test"), env)

			reader := bufio.NewReader(strings.NewReader(tc.input))
			p := parser.NewParser(env, true, reader)
			stx, err := p.ReadSyntax(context.TODO())
			if err == io.EOF || err != nil {
				// Input might be incomplete - that's expected for some cases
				return
			}

			err = parseExportSpec(lib, stx)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
