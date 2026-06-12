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

package repl

import (
	"bytes"
	"context"
	"slices"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/docparse"
	"github.com/aalpar/wile/pkg/wile"
	"github.com/aalpar/wile/stdlib"
	"github.com/aalpar/wile/values"
)

func newTestEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background())
	qt.Assert(t, err, qt.IsNil)
	return eng
}

func TestMetaCommandHandler(t *testing.T) {
	tcs := []struct {
		name    string
		input   string
		debug   bool
		handled bool
		contain string
	}{
		{"unknown command", ",bogus", false, true, "Unknown command"},
		{"empty comma", ",", false, true, ""},
		{"not a command", "(+ 1 2)", false, false, ""},
		{"delegates to debug", ",break foo.scm:10", true, true, "Breakpoint"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			var buf bytes.Buffer
			h := NewMetaCommandHandler(nil)
			if tc.debug {
				h.SetDebugContext(NewDebugContext())
			}
			handled := h.Handle(context.Background(), tc.input, &buf)
			qt.Assert(t, handled, qt.Equals, tc.handled)
			if tc.contain != "" {
				qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
					qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
			}
		})
	}
}

func TestCmdVersion(t *testing.T) {
	tcs := []struct {
		name    string
		version string
		contain string
	}{
		{"version injected", "Wile Scheme v9.9.9 (abcdef0)", "Wile Scheme v9.9.9 (abcdef0)"},
		{"version not injected", "", "version information unavailable"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			var buf bytes.Buffer
			var opts []MetaOption
			if tc.version != "" {
				opts = append(opts, WithMetaVersion(tc.version))
			}
			h := NewMetaCommandHandler(nil, opts...)
			handled := h.Handle(context.Background(), ",version", &buf)
			qt.Assert(t, handled, qt.IsTrue)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}

func TestCmdEdit(t *testing.T) {
	tcs := []struct {
		name    string
		args    []string
		editor  string
		contain string
	}{
		{"no args", nil, "", "Usage"},
		{"no editor", []string{"foo.scm"}, "", "$EDITOR"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("EDITOR", tc.editor)
			var buf bytes.Buffer
			h := NewMetaCommandHandler(nil)
			h.cmdEdit(tc.args, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}

func TestCmdEdit_EditorExec(t *testing.T) {
	// Use "true" as editor — it always succeeds and exits immediately
	t.Setenv("EDITOR", "true")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil)
	h.cmdEdit([]string{"/dev/null"}, &buf)
	// Should succeed silently
	qt.Assert(t, buf.String(), qt.Equals, "")
}

func TestCmdDoc(t *testing.T) {
	eng := newTestEngine(t)
	docProv := NewRegistryDocProvider(eng.Registry(), nil)

	tcs := []struct {
		name    string
		args    []string
		contain string
	}{
		{"no args", nil, "Usage"},
		{"primitive with doc", []string{"+"}, "+"},
		{"unbound identifier", []string{"nonexistent-xyz"}, "nbound"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("PAGER", "") // disable pager in tests
			var buf bytes.Buffer
			h := NewMetaCommandHandler(eng, WithMetaDocProvider(docProv))
			h.cmdDoc(tc.args, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}

func TestCmdHelp(t *testing.T) {
	t.Run("lists all commands", func(t *testing.T) {
		t.Setenv("PAGER", "")
		var buf bytes.Buffer
		h := NewMetaCommandHandler(nil)
		h.cmdHelp(nil, &buf)
		output := buf.String()
		for _, cmd := range []string{",doc", ",edit", ",help", ",break"} {
			qt.Assert(t, strings.Contains(output, cmd), qt.IsTrue,
				qt.Commentf("help output should contain %q", cmd))
		}
	})

	t.Run("specific command detail", func(t *testing.T) {
		t.Setenv("PAGER", "")
		var buf bytes.Buffer
		h := NewMetaCommandHandler(nil)
		h.cmdHelp([]string{"doc"}, &buf)
		output := buf.String()
		qt.Assert(t, strings.Contains(output, ",doc"), qt.IsTrue)
		// Should have more detail than the list view
		qt.Assert(t, strings.Contains(output, "binding"), qt.IsTrue,
			qt.Commentf("help doc output should mention bindings: %q", output))
	})

	t.Run("unknown command help", func(t *testing.T) {
		t.Setenv("PAGER", "")
		var buf bytes.Buffer
		h := NewMetaCommandHandler(nil)
		h.cmdHelp([]string{"nonexistent"}, &buf)
		qt.Assert(t, strings.Contains(buf.String(), "Unknown"), qt.IsTrue)
	})
}

func TestCmdDoc_BindingLookup(t *testing.T) {
	eng := newTestEngine(t)

	// No doc provider — falls through to environment lookup
	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(eng)
	// "if" is a syntax binding, should be found in phase environments
	h.cmdDoc([]string{"if"}, &buf)
	output := buf.String()
	// Should find something (not "Unbound")
	qt.Assert(t, strings.Contains(output, "nbound"), qt.IsFalse,
		qt.Commentf("output was: %q", output))
}

func TestCmdDoc_ClosureDocstring(t *testing.T) {
	ctx := context.Background()
	eng := newTestEngine(t)

	// Define a procedure with a Guile-style docstring
	_, err := eng.EvalMultiple(ctx, `(define (f x) "Adds one to x." (+ x 1))`)
	qt.Assert(t, err, qt.IsNil)

	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(eng)
	h.cmdDoc([]string{"f"}, &buf)
	output := buf.String()
	qt.Assert(t, strings.Contains(output, "Adds one to x."), qt.IsTrue,
		qt.Commentf(",doc should show closure docstring: %q", output))
}

func TestCmdDoc_SpecialFormStructuredFormat(t *testing.T) {
	eng := newTestEngine(t)
	docProv := NewRegistryDocProvider(eng.Registry(), nil)

	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(eng, WithMetaDocProvider(docProv))
	h.cmdDoc([]string{"if"}, &buf)
	output := buf.String()

	// Should use structured format: syntax as header, type label, category
	qt.Assert(t, strings.Contains(output, "(if TEST CONSEQUENT ALTERNATE)"), qt.IsTrue,
		qt.Commentf("should have syntax pattern in header: %q", output))
	qt.Assert(t, strings.Contains(output, "Form: special form"), qt.IsTrue,
		qt.Commentf("should have form type: %q", output))
	qt.Assert(t, strings.Contains(output, "Category: conditionals"), qt.IsTrue,
		qt.Commentf("should have category: %q", output))
	// Should NOT use old format
	qt.Assert(t, strings.Contains(output, "if: special form"), qt.IsFalse,
		qt.Commentf("should not use old header format: %q", output))
}

func TestCmdDoc_MacroStructuredFormat(t *testing.T) {
	eng := newTestEngine(t)
	docProv := NewRegistryDocProvider(eng.Registry(), nil)

	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(eng, WithMetaDocProvider(docProv))
	h.cmdDoc([]string{"and"}, &buf)
	output := buf.String()

	// Bootstrap macros should also use structured format
	qt.Assert(t, strings.Contains(output, "(and TEST1 ...)"), qt.IsTrue,
		qt.Commentf("should have syntax pattern: %q", output))
	qt.Assert(t, strings.Contains(output, "Form: syntax"), qt.IsTrue,
		qt.Commentf("should have form type: %q", output))
	qt.Assert(t, strings.Contains(output, "Category: conditionals"), qt.IsTrue,
		qt.Commentf("should have category: %q", output))
}

func TestMetaCommandHandlerCommands(t *testing.T) {
	h := NewMetaCommandHandler(nil)
	h.SetDebugContext(NewDebugContext())
	cmds := h.Commands()
	qt.Assert(t, len(cmds) > 0, qt.IsTrue)

	// Session commands
	for _, expected := range []string{"help", "doc", "edit", "apropos", "topics", "topic", "libraries", "libs", "disassemble", "dis"} {
		qt.Assert(t, slices.Contains(cmds, expected), qt.IsTrue,
			qt.Commentf("Commands() should contain session command %q, got %v", expected, cmds))
	}

	// Debug commands (delegated from DebugContext)
	for _, expected := range []string{"break", "step", "continue", "backtrace", "where"} {
		qt.Assert(t, slices.Contains(cmds, expected), qt.IsTrue,
			qt.Commentf("Commands() should contain debug command %q, got %v", expected, cmds))
	}

	// Aliases should also be present
	for _, expected := range []string{"h", "?", "a", "b", "s", "c", "bt"} {
		qt.Assert(t, slices.Contains(cmds, expected), qt.IsTrue,
			qt.Commentf("Commands() should contain alias %q, got %v", expected, cmds))
	}
}

func TestMetaCommandHandlerCommands_NoDebugCtx(t *testing.T) {
	h := NewMetaCommandHandler(nil)
	cmds := h.Commands()
	// Should still have session commands
	qt.Assert(t, slices.Contains(cmds, "help"), qt.IsTrue)
	// Should NOT have debug commands
	qt.Assert(t, slices.Contains(cmds, "break"), qt.IsFalse)
}

func TestMetaHandleUnknown(t *testing.T) {
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil)

	// Unknown command still returns true (it's a meta-command, just unrecognized)
	handled := h.Handle(context.Background(), ",totally_unknown_cmd", &buf)
	qt.Assert(t, handled, qt.IsTrue)
	qt.Assert(t, strings.Contains(buf.String(), "Unknown command"), qt.IsTrue,
		qt.Commentf("output was: %q", buf.String()))
}

func TestCmdDoc_ExamplesFiltering(t *testing.T) {
	eng := newTestEngine(t)
	docProv := NewRegistryDocProvider(eng.Registry(), nil)

	t.Run("strips examples by default", func(t *testing.T) {
		t.Setenv("PAGER", "")
		var buf bytes.Buffer
		h := NewMetaCommandHandler(eng, WithMetaDocProvider(docProv))
		h.cmdDoc([]string{"car"}, &buf)
		output := buf.String()
		qt.Assert(t, strings.Contains(output, "car"), qt.IsTrue,
			qt.Commentf("output: %q", output))
		qt.Assert(t, strings.Contains(output, "Examples:"), qt.IsFalse,
			qt.Commentf(",doc car should not contain examples by default: %q", output))
	})

	t.Run("shows examples with -x", func(t *testing.T) {
		t.Setenv("PAGER", "")
		var buf bytes.Buffer
		h := NewMetaCommandHandler(eng, WithMetaDocProvider(docProv))
		h.cmdDoc([]string{"-x", "car"}, &buf)
		output := buf.String()
		qt.Assert(t, strings.Contains(output, "car"), qt.IsTrue,
			qt.Commentf("output: %q", output))
		qt.Assert(t, strings.Contains(output, "Examples:"), qt.IsTrue,
			qt.Commentf(",doc -x car should contain examples: %q", output))
	})

	t.Run("-x alone shows usage", func(t *testing.T) {
		t.Setenv("PAGER", "")
		var buf bytes.Buffer
		h := NewMetaCommandHandler(eng, WithMetaDocProvider(docProv))
		h.cmdDoc([]string{"-x"}, &buf)
		qt.Assert(t, strings.Contains(buf.String(), "Usage"), qt.IsTrue)
	})
}

func TestFormatPrimitiveDoc_WithTypes(t *testing.T) {
	c := qt.New(t)
	var buf strings.Builder
	info := DocInfo{
		Doc:        "Returns the kth character of string.",
		TypeLabel:  "primitive",
		ParamNames: []string{"string", "k"},
		Category:   "strings",
		ParamCount: 2,
		ParamTypes: []values.TypeConstraint{values.TypeString, values.TypeInteger},
		ReturnType: values.TypeCharacter,
	}
	formatPrimitiveDoc(&buf, "string-ref", info, true)
	output := buf.String()
	c.Assert(strings.Contains(output, "→ character"), qt.IsTrue,
		qt.Commentf("output: %s", output))
	c.Assert(strings.Contains(output, "Form: primitive"), qt.IsTrue,
		qt.Commentf("output: %s", output))
	c.Assert(strings.Contains(output, "STRING : string"), qt.IsTrue,
		qt.Commentf("output: %s", output))
	c.Assert(strings.Contains(output, "K : integer"), qt.IsTrue,
		qt.Commentf("output: %s", output))
	c.Assert(strings.Contains(output, "Returns: character"), qt.IsTrue,
		qt.Commentf("output: %s", output))
}

func TestFormatPrimitiveDoc_WithoutTypes(t *testing.T) {
	c := qt.New(t)
	var buf strings.Builder
	info := DocInfo{
		Doc:        "Returns the length of string.",
		ParamNames: []string{"string"},
		Category:   "strings",
		ParamCount: 1,
	}
	formatPrimitiveDoc(&buf, "string-length", info, true)
	output := buf.String()
	// Without ParamTypes, should have no type annotations
	c.Assert(strings.Contains(output, " : "), qt.IsFalse,
		qt.Commentf("output should have no type annotations: %s", output))
	c.Assert(strings.Contains(output, "→"), qt.IsFalse,
		qt.Commentf("output should have no return type: %s", output))
}

func TestFormatPrimitiveDoc_ReturnTypeWithoutParamTypes(t *testing.T) {
	c := qt.New(t)
	var buf strings.Builder
	info := DocInfo{
		Doc:        "Returns a new empty hashtable.",
		Category:   "hashtables",
		ReturnType: values.TypeHashtable,
	}
	formatPrimitiveDoc(&buf, "make-hashtable", info, true)
	output := buf.String()
	c.Assert(strings.Contains(output, "→ hashtable"), qt.IsTrue,
		qt.Commentf("should show return type even without ParamTypes: %s", output))
	c.Assert(strings.Contains(output, "Returns: hashtable"), qt.IsTrue,
		qt.Commentf("should show Returns section: %s", output))
	c.Assert(strings.Contains(output, " : "), qt.IsFalse,
		qt.Commentf("should have no parameter type annotations: %s", output))
}

func TestCmdApropos(t *testing.T) {
	eng := newTestEngine(t)
	docProv := NewRegistryDocProvider(eng.Registry(), nil)

	tcs := []struct {
		name    string
		args    []string
		contain string
	}{
		{"no args", nil, "Usage"},
		{"matches name", []string{"string-app"}, "string-append"},
		{"matches category", []string{"arithmetic"}, "+"},
		{"no match", []string{"zzzzzzzzz"}, "No matches"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("PAGER", "")
			var buf bytes.Buffer
			h := NewMetaCommandHandler(eng, WithMetaDocProvider(docProv))
			h.cmdApropos(context.Background(), tc.args, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}

func TestCmdApropos_SpecialFormCategory(t *testing.T) {
	eng := newTestEngine(t)
	docProv := NewRegistryDocProvider(eng.Registry(), nil)

	// Special forms and macros should show [category] in apropos output.
	// These are found via registry binding specs and doc entries;
	// category is extracted from the embedded docstring metadata.
	tcs := []struct {
		name    string
		pattern string
		wantCat string
	}{
		{"special form: include-ci", "include-ci", "[libraries]"},
		{"special form: if", "if", "[conditionals]"},
		{"macro: case", "case", "[conditionals]"},
		{"macro: guard", "guard", "[exceptions]"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("PAGER", "")
			var buf bytes.Buffer
			h := NewMetaCommandHandler(eng, WithMetaDocProvider(docProv))
			h.cmdApropos(context.Background(), []string{tc.pattern}, &buf)
			output := buf.String()
			qt.Assert(t, strings.Contains(output, tc.pattern), qt.IsTrue,
				qt.Commentf("output should contain %q: %q", tc.pattern, output))
			qt.Assert(t, strings.Contains(output, tc.wantCat), qt.IsTrue,
				qt.Commentf("output should contain category %q: %q", tc.wantCat, output))
		})
	}
}

func TestCmdApropos_Library(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	// Import a real library so it's loaded.
	_, err = eng.EvalMultiple(ctx, `(import (wile algebra))`)
	qt.Assert(t, err, qt.IsNil)

	docProv := NewRegistryDocProvider(eng.Registry(), eng)

	tcs := []struct {
		name    string
		pattern string
		contain string
	}{
		{"matches library name part", "algebra", "(wile algebra)"},
		{"matches library description", "lattices", "(wile algebra)"},
		{"matches library prefix", "wile", "(wile algebra)"},
		{"no match", "zzzzzzzzz", "No matches"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("PAGER", "")
			var buf bytes.Buffer
			h := NewMetaCommandHandler(eng, WithMetaDocProvider(docProv))
			h.cmdApropos(context.Background(), []string{tc.pattern}, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}

func TestCmdTopics(t *testing.T) {
	eng := newTestEngine(t)
	docProv := NewRegistryDocProvider(eng.Registry(), nil)

	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, WithMetaDocProvider(docProv))
	h.cmdTopics(context.Background(), &buf)
	output := buf.String()
	qt.Assert(t, strings.Contains(output, "arithmetic"), qt.IsTrue,
		qt.Commentf("output: %q", output))
	qt.Assert(t, strings.Contains(output, "strings"), qt.IsTrue,
		qt.Commentf("output: %q", output))
}

func TestCmdTopic(t *testing.T) {
	eng := newTestEngine(t)
	docProv := NewRegistryDocProvider(eng.Registry(), nil)

	tcs := []struct {
		name    string
		args    []string
		contain string
	}{
		{"no args", nil, "Usage"},
		{"valid category", []string{"arithmetic"}, "+"},
		{"unknown category", []string{"nonexistent"}, "No category"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("PAGER", "")
			var buf bytes.Buffer
			h := NewMetaCommandHandler(nil, WithMetaDocProvider(docProv))
			h.cmdTopic(context.Background(), tc.args, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}

func TestCmdDocLibrary(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	// Import a real library with known properties.
	_, err = eng.EvalMultiple(ctx, `(import (wile algebra))`)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name    string
		args    []string
		contain string
	}{
		{"library with description", []string{"(wile", "algebra)"}, "Algebraic structures"},
		{"library exports", []string{"(wile", "algebra)"}, "Exports"},
		{"unknown library", []string{"(unknown", "lib)"}, "not loaded"},
		{"empty parens", []string{"()"}, "Usage"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("PAGER", "")
			var buf bytes.Buffer
			h := NewMetaCommandHandler(eng)
			h.cmdDoc(tc.args, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}

	// Engine without library registry: LookupLibrary returns nil -> "not loaded"
	t.Run("no registry configured", func(t *testing.T) {
		t.Setenv("PAGER", "")
		noRegEng := newTestEngine(t)
		var buf bytes.Buffer
		h := NewMetaCommandHandler(noRegEng)
		h.cmdDoc([]string{"(some", "lib)"}, &buf)
		qt.Assert(t, strings.Contains(buf.String(), "not loaded"), qt.IsTrue,
			qt.Commentf("output was: %q", buf.String()))
	})
}

func TestCmdLibraries(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	_, err = eng.EvalMultiple(ctx, `(import (wile algebra))`)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name    string
		contain string
	}{
		{"shows loaded count", "Loaded libraries"},
		{"shows library name", "(wile algebra)"},
		{"shows description", "Algebraic structures"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			var buf bytes.Buffer
			h := NewMetaCommandHandler(eng)
			h.SetPager("")
			h.cmdLibraries(context.Background(), &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}

	t.Run("no env", func(t *testing.T) {
		var buf bytes.Buffer
		h := NewMetaCommandHandler(nil)
		h.cmdLibraries(context.Background(), &buf)
		qt.Assert(t, strings.Contains(buf.String(), "No environment"), qt.IsTrue)
	})

	t.Run("no registry — shows no libraries", func(t *testing.T) {
		noRegEng := newTestEngine(t)
		var buf bytes.Buffer
		h := NewMetaCommandHandler(noRegEng)
		h.cmdLibraries(context.Background(), &buf)
		// Engine with no library registry returns nil from LoadedLibraries
		qt.Assert(t, strings.Contains(buf.String(), "No libraries loaded"), qt.IsTrue,
			qt.Commentf("output was: %q", buf.String()))
	})

	t.Run("alias libs", func(t *testing.T) {
		var buf bytes.Buffer
		h := NewMetaCommandHandler(eng)
		h.SetPager("")
		h.Handle(context.Background(), ",libs", &buf)
		qt.Assert(t, strings.Contains(buf.String(), "(wile algebra)"), qt.IsTrue)
	})
}

func TestCmdLibraries_UnloadedFromExportIndex(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	// Do NOT import (wile algebra) — it should appear as an available library.
	docProv := NewRegistryDocProvider(eng.Registry(), eng)

	var buf bytes.Buffer
	h := NewMetaCommandHandler(eng, WithMetaDocProvider(docProv))
	h.SetPager("")
	h.cmdLibraries(context.Background(), &buf)
	output := buf.String()

	qt.Assert(t, strings.Contains(output, "Available libraries"), qt.IsTrue,
		qt.Commentf("should show unloaded libraries section; got: %q", output))
	qt.Assert(t, strings.Contains(output, "(wile algebra)"), qt.IsTrue,
		qt.Commentf("should list (wile algebra) as available; got: %q", output))
}

func TestCmdDisassemble(t *testing.T) {
	ctx := context.Background()
	eng := newTestEngine(t)

	// Define a procedure so we can disassemble it.
	_, err := eng.EvalMultiple(ctx, `(define (add1 x) (+ x 1))`)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name    string
		args    []string
		contain string
	}{
		{"no args", nil, "Usage"},
		{"native closure", []string{"add1"}, "OP"},
		{"unbound identifier", []string{"nonexistent-xyz"}, "unbound identifier"},
		{"syntax binding", []string{"if"}, "not a procedure"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("PAGER", "")
			var buf bytes.Buffer
			h := NewMetaCommandHandler(eng)
			h.cmdDisassemble(tc.args, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}

func TestCmdDisassemble_ForeignClosure(t *testing.T) {
	eng := newTestEngine(t)

	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(eng)
	h.cmdDisassemble([]string{"car"}, &buf)
	output := buf.String()
	qt.Assert(t, strings.Contains(output, "foreign"), qt.IsTrue,
		qt.Commentf("output was: %q", output))
	qt.Assert(t, strings.Contains(output, "car"), qt.IsTrue,
		qt.Commentf("output was: %q", output))
}

func TestCmdDisassemble_Alias(t *testing.T) {
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil)
	handled := h.Handle(context.Background(), ",dis", &buf)
	qt.Assert(t, handled, qt.IsTrue)
	qt.Assert(t, strings.Contains(buf.String(), "Usage"), qt.IsTrue)
}

func TestFormatPrimitiveDoc_FromDocparse(t *testing.T) {
	c := qt.New(t)

	raw := "Multiply two numbers.\nParameters:\n  x : number\n  y : number\nReturns: number\nCategory: arithmetic"
	parsed := docparse.ParseDocstring(raw)
	c.Assert(parsed.HasStructuredMetadata(), qt.IsTrue)

	info := DocInfo{
		Doc:        parsed.Doc,
		ParamNames: parsed.ParamNames,
		ParamTypes: parsed.ParamTypes,
		ReturnType: parsed.ReturnType,
		Category:   parsed.Category,
	}

	var buf strings.Builder
	formatPrimitiveDoc(&buf, "my-multiply", info, false)
	output := buf.String()

	c.Assert(strings.Contains(output, "(my-multiply X Y)"), qt.IsTrue,
		qt.Commentf("should have signature: %s", output))
	c.Assert(strings.Contains(output, "number"), qt.IsTrue,
		qt.Commentf("should have return type: %s", output))
	c.Assert(strings.Contains(output, "X : number"), qt.IsTrue,
		qt.Commentf("should have param type for X: %s", output))
	c.Assert(strings.Contains(output, "Y : number"), qt.IsTrue,
		qt.Commentf("should have param type for Y: %s", output))
	c.Assert(strings.Contains(output, "Category: arithmetic"), qt.IsTrue,
		qt.Commentf("should have category: %s", output))
	c.Assert(strings.Contains(output, "Multiply two numbers."), qt.IsTrue,
		qt.Commentf("should have description: %s", output))
}

func TestFormatPrimitiveDoc_WithSyntax(t *testing.T) {
	c := qt.New(t)

	info := DocInfo{
		Doc:       "Conditional expression. R7RS §4.1.5.",
		Syntax:    "(if <test> <consequent> <alternate>)",
		TypeLabel: "special form",
		Category:  "conditionals",
	}

	var buf strings.Builder
	formatPrimitiveDoc(&buf, "if", info, false)
	output := buf.String()

	c.Assert(strings.Contains(output, "(if <test> <consequent> <alternate>)"), qt.IsTrue,
		qt.Commentf("should have syntax pattern: %s", output))
	c.Assert(strings.Contains(output, "Form: special form"), qt.IsTrue,
		qt.Commentf("should have form type: %s", output))
	c.Assert(strings.Contains(output, "Conditional expression."), qt.IsTrue,
		qt.Commentf("should have description: %s", output))
	c.Assert(strings.Contains(output, "Category: conditionals"), qt.IsTrue,
		qt.Commentf("should have category: %s", output))
	// Should NOT build a signature from params
	c.Assert(strings.Contains(output, "(if)"), qt.IsFalse,
		qt.Commentf("should not have param-based signature: %s", output))
}

func TestFormatPrimitiveDoc_MultiLineDescription(t *testing.T) {
	c := qt.New(t)

	info := DocInfo{
		Doc:       "First line.\nSecond line.\nThird line.",
		Syntax:    "(foo <bar>)",
		TypeLabel: "special form",
		Category:  "test",
	}

	var buf strings.Builder
	formatPrimitiveDoc(&buf, "foo", info, false)
	output := buf.String()

	// All description lines should be indented
	c.Assert(strings.Contains(output, "  First line.\n  Second line.\n  Third line."), qt.IsTrue,
		qt.Commentf("multi-line desc should be indented: %s", output))
}

func TestCmdApropos_KeywordMatchAfterLibraryImport(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	// Import the algebra library so its bindings are in the environment.
	_, err = eng.EvalMultiple(ctx, `(import (wile algebra))`)
	qt.Assert(t, err, qt.IsNil)

	// Build a doc provider with the live engine (as the MCP server does).
	docProv := NewRegistryDocProvider(eng.Registry(), eng)

	// Search for "abelian" — make-group has it only in Keywords, not in name or doc prose.
	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(eng, WithMetaDocProvider(docProv))
	h.cmdApropos(ctx, []string{"abelian"}, &buf)
	output := buf.String()

	qt.Assert(t, strings.Contains(output, "make-group"), qt.IsTrue,
		qt.Commentf("apropos should find make-group via keyword match; got: %q", output))
}

func TestCmdApropos_UnloadedLibraryNameMatch(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)

	// Do NOT import (wile algebra) — it must be found via the export index.
	docProv := NewRegistryDocProvider(eng.Registry(), eng)

	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(eng, WithMetaDocProvider(docProv))
	h.cmdApropos(ctx, []string{"algebra"}, &buf)
	output := buf.String()

	qt.Assert(t, strings.Contains(output, "(wile algebra)"), qt.IsTrue,
		qt.Commentf("apropos should find (wile algebra) by library name when not imported; got: %q", output))
}

func TestMetaHandleDebugDelegation(t *testing.T) {
	tcs := []struct {
		name    string
		input   string
		contain string
	}{
		{"break command", ",break test.scm:1", "Breakpoint"},
		{"step command", ",step", "Will step into"},
		{"continue command", ",continue", "Continuing"},
		{"list command", ",list", "No breakpoints"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			h := NewMetaCommandHandler(nil)
			h.SetDebugContext(NewDebugContext())
			var buf bytes.Buffer
			handled := h.Handle(context.Background(), tc.input, &buf)
			qt.Assert(t, handled, qt.IsTrue)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}
