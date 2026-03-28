package repl

import (
	"bytes"
	"context"
	"slices"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/machine"
	wileruntime "github.com/aalpar/wile/runtime"
	"github.com/aalpar/wile/values"
)

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
			var debugCtx *DebugContext
			if tc.debug {
				debugCtx = NewDebugContext()
			}
			h := NewMetaCommandHandler(nil, debugCtx, nil)
			handled := h.Handle(tc.input, &buf)
			qt.Assert(t, handled, qt.Equals, tc.handled)
			if tc.contain != "" {
				qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
					qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
			}
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
			h := NewMetaCommandHandler(nil, nil, nil)
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
	h := NewMetaCommandHandler(nil, nil, nil)
	h.cmdEdit([]string{"/dev/null"}, &buf)
	// Should succeed silently
	qt.Assert(t, buf.String(), qt.Equals, "")
}

func TestCmdDoc(t *testing.T) {
	ctx := context.Background()
	env, reg, err := bootstrap.NewTopLevelWithRegistry(ctx)
	qt.Assert(t, err, qt.IsNil)
	docProv := NewRegistryDocProvider(reg)

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
			h := NewMetaCommandHandler(env, nil, docProv)
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
		h := NewMetaCommandHandler(nil, nil, nil)
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
		h := NewMetaCommandHandler(nil, nil, nil)
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
		h := NewMetaCommandHandler(nil, nil, nil)
		h.cmdHelp([]string{"nonexistent"}, &buf)
		qt.Assert(t, strings.Contains(buf.String(), "Unknown"), qt.IsTrue)
	})
}

func TestCmdDoc_BindingLookup(t *testing.T) {
	ctx := context.Background()
	env, _, err := bootstrap.NewTopLevelWithRegistry(ctx)
	qt.Assert(t, err, qt.IsNil)

	// No doc provider — falls through to environment lookup
	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(env, nil, nil)
	// "if" is a syntax binding, should be found in phase environments
	h.cmdDoc([]string{"if"}, &buf)
	output := buf.String()
	// Should find something (not "Unbound")
	qt.Assert(t, strings.Contains(output, "nbound"), qt.IsFalse,
		qt.Commentf("output was: %q", output))
}

func TestCmdDoc_ClosureDocstring(t *testing.T) {
	ctx := context.Background()
	env, _, err := bootstrap.NewTopLevelWithRegistry(ctx)
	qt.Assert(t, err, qt.IsNil)

	// Define a procedure with a Guile-style docstring
	rdr := strings.NewReader(`(define (f x) "Adds one to x." (+ x 1))`)
	p := parser.NewParser(env, true, rdr)
	stx, err := p.ReadSyntax(ctx)
	qt.Assert(t, err, qt.IsNil)
	tpl, err := wileruntime.Compile(ctx, env, stx)
	qt.Assert(t, err, qt.IsNil)
	_, err = wileruntime.Run(ctx, tpl, env)
	qt.Assert(t, err, qt.IsNil)

	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(env, nil, nil)
	h.cmdDoc([]string{"f"}, &buf)
	output := buf.String()
	qt.Assert(t, strings.Contains(output, "Adds one to x."), qt.IsTrue,
		qt.Commentf(",doc should show closure docstring: %q", output))
}

func TestMetaCommandHandlerCommands(t *testing.T) {
	debugCtx := NewDebugContext()
	h := NewMetaCommandHandler(nil, debugCtx, nil)
	cmds := h.Commands()
	qt.Assert(t, len(cmds) > 0, qt.IsTrue)

	// Session commands
	for _, expected := range []string{"help", "doc", "edit", "apropos", "topics", "topic", "libraries", "libs"} {
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
	h := NewMetaCommandHandler(nil, nil, nil)
	cmds := h.Commands()
	// Should still have session commands
	qt.Assert(t, slices.Contains(cmds, "help"), qt.IsTrue)
	// Should NOT have debug commands
	qt.Assert(t, slices.Contains(cmds, "break"), qt.IsFalse)
}

func TestMetaHandleUnknown(t *testing.T) {
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, nil, nil)

	// Unknown command still returns true (it's a meta-command, just unrecognized)
	handled := h.Handle(",totally_unknown_cmd", &buf)
	qt.Assert(t, handled, qt.IsTrue)
	qt.Assert(t, strings.Contains(buf.String(), "Unknown command"), qt.IsTrue,
		qt.Commentf("output was: %q", buf.String()))
}

func TestFormatPrimitiveDoc_WithTypes(t *testing.T) {
	c := qt.New(t)
	var buf strings.Builder
	info := DocInfo{
		Doc:        "Returns the kth character of string.",
		ParamNames: []string{"string", "k"},
		Category:   "strings",
		ParamCount: 2,
		ParamTypes: []values.ValueType{values.TypeString, values.TypeExactInteger},
		ReturnType: values.TypeCharacter,
	}
	formatPrimitiveDoc(&buf, "string-ref", info)
	output := buf.String()
	c.Assert(strings.Contains(output, "→ character"), qt.IsTrue,
		qt.Commentf("output: %s", output))
	c.Assert(strings.Contains(output, "string : string"), qt.IsTrue,
		qt.Commentf("output: %s", output))
	c.Assert(strings.Contains(output, "k : exact-integer"), qt.IsTrue,
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
	formatPrimitiveDoc(&buf, "string-length", info)
	output := buf.String()
	// Without ParamTypes, should have no type annotations
	c.Assert(strings.Contains(output, " : "), qt.IsFalse,
		qt.Commentf("output should have no type annotations: %s", output))
	c.Assert(strings.Contains(output, "→"), qt.IsFalse,
		qt.Commentf("output should have no return type: %s", output))
}

func TestCmdApropos(t *testing.T) {
	ctx := context.Background()
	env, reg, err := bootstrap.NewTopLevelWithRegistry(ctx)
	qt.Assert(t, err, qt.IsNil)
	docProv := NewRegistryDocProvider(reg)

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
			h := NewMetaCommandHandler(env, nil, docProv)
			h.cmdApropos(tc.args, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}

func TestCmdApropos_Library(t *testing.T) {
	ctx := context.Background()
	env, reg, err := bootstrap.NewTopLevelWithRegistry(ctx)
	qt.Assert(t, err, qt.IsNil)
	docProv := NewRegistryDocProvider(reg)

	// Register a library in the env's library registry
	libReg := machine.NewLibraryRegistry()
	lib := machine.NewCompiledLibrary(machine.NewLibraryName("wile", "algebra"), env)
	lib.Description = "Algebraic structures: orders, lattices, monoids."
	err = libReg.Register(lib)
	qt.Assert(t, err, qt.IsNil)
	env.SetLibraryRegistry(libReg)

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
			h := NewMetaCommandHandler(env, nil, docProv)
			h.cmdApropos([]string{tc.pattern}, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}

func TestCmdTopics(t *testing.T) {
	ctx := context.Background()
	_, reg, err := bootstrap.NewTopLevelWithRegistry(ctx)
	qt.Assert(t, err, qt.IsNil)
	docProv := NewRegistryDocProvider(reg)

	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, nil, docProv)
	h.cmdTopics(&buf)
	output := buf.String()
	qt.Assert(t, strings.Contains(output, "arithmetic"), qt.IsTrue,
		qt.Commentf("output: %q", output))
	qt.Assert(t, strings.Contains(output, "strings"), qt.IsTrue,
		qt.Commentf("output: %q", output))
}

func TestCmdTopic(t *testing.T) {
	ctx := context.Background()
	_, reg, err := bootstrap.NewTopLevelWithRegistry(ctx)
	qt.Assert(t, err, qt.IsNil)
	docProv := NewRegistryDocProvider(reg)

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
			h := NewMetaCommandHandler(nil, nil, docProv)
			h.cmdTopic(tc.args, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}

func TestCmdDocLibrary(t *testing.T) {
	ctx := context.Background()
	env, _, err := bootstrap.NewTopLevelWithRegistry(ctx)
	qt.Assert(t, err, qt.IsNil)

	// Set up a library registry with a test library
	reg := machine.NewLibraryRegistry()
	lib := machine.NewCompiledLibrary(machine.NewLibraryName("test", "lib"), env)
	lib.Description = "A test library for documentation."
	lib.AddExport("foo", "foo")
	lib.AddExport("bar", "bar")
	err = reg.Register(lib)
	qt.Assert(t, err, qt.IsNil)
	env.SetLibraryRegistry(reg)

	tcs := []struct {
		name    string
		args    []string
		contain string
	}{
		{"library with description", []string{"(test", "lib)"}, "A test library"},
		{"library exports", []string{"(test", "lib)"}, "Exports (2)"},
		{"unknown library", []string{"(unknown", "lib)"}, "not loaded"},
		{"empty parens", []string{"()"}, "Usage"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv("PAGER", "")
			var buf bytes.Buffer
			h := NewMetaCommandHandler(env, nil, nil)
			h.cmdDoc(tc.args, &buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}

	// Separate test: env without library registry
	t.Run("no registry configured", func(t *testing.T) {
		t.Setenv("PAGER", "")
		noRegEnv, _, err := bootstrap.NewTopLevelWithRegistry(ctx)
		qt.Assert(t, err, qt.IsNil)
		var buf bytes.Buffer
		h := NewMetaCommandHandler(noRegEnv, nil, nil)
		h.cmdDoc([]string{"(some", "lib)"}, &buf)
		qt.Assert(t, strings.Contains(buf.String(), "no library registry"), qt.IsTrue,
			qt.Commentf("output was: %q", buf.String()))
	})
}

func TestCmdLibraries(t *testing.T) {
	ctx := context.Background()
	env, _, err := bootstrap.NewTopLevelWithRegistry(ctx)
	qt.Assert(t, err, qt.IsNil)

	reg := machine.NewLibraryRegistry()
	lib := machine.NewCompiledLibrary(machine.NewLibraryName("test", "lib"), env)
	lib.Description = "A test library."
	err = reg.Register(lib)
	qt.Assert(t, err, qt.IsNil)
	env.SetLibraryRegistry(reg)

	tcs := []struct {
		name    string
		contain string
	}{
		{"shows loaded count", "Loaded libraries (1)"},
		{"shows library name", "(test lib)"},
		{"shows description", "A test library."},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			var buf bytes.Buffer
			h := NewMetaCommandHandler(env, nil, nil)
			h.SetPager("")
			h.cmdLibraries(&buf)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}

	t.Run("no env", func(t *testing.T) {
		var buf bytes.Buffer
		h := NewMetaCommandHandler(nil, nil, nil)
		h.cmdLibraries(&buf)
		qt.Assert(t, strings.Contains(buf.String(), "No environment"), qt.IsTrue)
	})

	t.Run("no registry", func(t *testing.T) {
		noRegEnv, _, noRegErr := bootstrap.NewTopLevelWithRegistry(ctx)
		qt.Assert(t, noRegErr, qt.IsNil)
		var buf bytes.Buffer
		h := NewMetaCommandHandler(noRegEnv, nil, nil)
		h.cmdLibraries(&buf)
		qt.Assert(t, strings.Contains(buf.String(), "No library registry"), qt.IsTrue)
	})

	t.Run("alias libs", func(t *testing.T) {
		var buf bytes.Buffer
		h := NewMetaCommandHandler(env, nil, nil)
		h.SetPager("")
		h.Handle(",libs", &buf)
		qt.Assert(t, strings.Contains(buf.String(), "(test lib)"), qt.IsTrue)
	})
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
			debugCtx := NewDebugContext()
			h := NewMetaCommandHandler(nil, debugCtx, nil)
			var buf bytes.Buffer
			handled := h.Handle(tc.input, &buf)
			qt.Assert(t, handled, qt.IsTrue)
			qt.Assert(t, strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}
