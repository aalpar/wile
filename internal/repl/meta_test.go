package repl

import (
	"bytes"
	"context"
	"slices"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/internal/bootstrap"
)

func TestMetaCommandHandler(t *testing.T) {
	c := qt.New(t)

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
			c.Assert(handled, qt.Equals, tc.handled)
			if tc.contain != "" {
				c.Assert(strings.Contains(buf.String(), tc.contain), qt.IsTrue,
					qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
			}
		})
	}
}

func TestCmdEdit(t *testing.T) {
	c := qt.New(t)

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
			c.Assert(strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}

func TestCmdEdit_EditorExec(t *testing.T) {
	c := qt.New(t)
	// Use "true" as editor — it always succeeds and exits immediately
	t.Setenv("EDITOR", "true")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, nil, nil)
	h.cmdEdit([]string{"/dev/null"}, &buf)
	// Should succeed silently
	c.Assert(buf.String(), qt.Equals, "")
}

func TestCmdDoc(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	env, reg, err := bootstrap.NewTopLevelWithRegistry(ctx)
	c.Assert(err, qt.IsNil)
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
			c.Assert(strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}

func TestCmdHelp(t *testing.T) {
	c := qt.New(t)

	t.Run("lists all commands", func(t *testing.T) {
		t.Setenv("PAGER", "")
		var buf bytes.Buffer
		h := NewMetaCommandHandler(nil, nil, nil)
		h.cmdHelp(nil, &buf)
		output := buf.String()
		for _, cmd := range []string{",doc", ",edit", ",help", ",break"} {
			c.Assert(strings.Contains(output, cmd), qt.IsTrue,
				qt.Commentf("help output should contain %q", cmd))
		}
	})

	t.Run("specific command detail", func(t *testing.T) {
		t.Setenv("PAGER", "")
		var buf bytes.Buffer
		h := NewMetaCommandHandler(nil, nil, nil)
		h.cmdHelp([]string{"doc"}, &buf)
		output := buf.String()
		c.Assert(strings.Contains(output, ",doc"), qt.IsTrue)
		// Should have more detail than the list view
		c.Assert(strings.Contains(output, "binding"), qt.IsTrue,
			qt.Commentf("help doc output should mention bindings: %q", output))
	})

	t.Run("unknown command help", func(t *testing.T) {
		t.Setenv("PAGER", "")
		var buf bytes.Buffer
		h := NewMetaCommandHandler(nil, nil, nil)
		h.cmdHelp([]string{"nonexistent"}, &buf)
		c.Assert(strings.Contains(buf.String(), "Unknown"), qt.IsTrue)
	})
}

func TestCmdDoc_BindingLookup(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	env, _, err := bootstrap.NewTopLevelWithRegistry(ctx)
	c.Assert(err, qt.IsNil)

	// No doc provider — falls through to environment lookup
	t.Setenv("PAGER", "")
	var buf bytes.Buffer
	h := NewMetaCommandHandler(env, nil, nil)
	// "if" is a syntax binding, should be found in phase environments
	h.cmdDoc([]string{"if"}, &buf)
	output := buf.String()
	// Should find something (not "Unbound")
	c.Assert(strings.Contains(output, "nbound"), qt.IsFalse,
		qt.Commentf("output was: %q", output))
}

func TestMetaCommandHandlerCommands(t *testing.T) {
	c := qt.New(t)

	debugCtx := NewDebugContext()
	h := NewMetaCommandHandler(nil, debugCtx, nil)
	cmds := h.Commands()
	c.Assert(len(cmds) > 0, qt.IsTrue)

	// Session commands
	for _, expected := range []string{"help", "doc", "edit"} {
		c.Assert(slices.Contains(cmds, expected), qt.IsTrue,
			qt.Commentf("Commands() should contain session command %q, got %v", expected, cmds))
	}

	// Debug commands (delegated from DebugContext)
	for _, expected := range []string{"break", "step", "continue", "backtrace", "where"} {
		c.Assert(slices.Contains(cmds, expected), qt.IsTrue,
			qt.Commentf("Commands() should contain debug command %q, got %v", expected, cmds))
	}

	// Aliases should also be present
	for _, expected := range []string{"h", "?", "b", "s", "c", "bt"} {
		c.Assert(slices.Contains(cmds, expected), qt.IsTrue,
			qt.Commentf("Commands() should contain alias %q, got %v", expected, cmds))
	}
}

func TestMetaCommandHandlerCommands_NoDebugCtx(t *testing.T) {
	c := qt.New(t)

	h := NewMetaCommandHandler(nil, nil, nil)
	cmds := h.Commands()
	// Should still have session commands
	c.Assert(slices.Contains(cmds, "help"), qt.IsTrue)
	// Should NOT have debug commands
	c.Assert(slices.Contains(cmds, "break"), qt.IsFalse)
}

func TestMetaHandleUnknown(t *testing.T) {
	c := qt.New(t)

	var buf bytes.Buffer
	h := NewMetaCommandHandler(nil, nil, nil)

	// Unknown command still returns true (it's a meta-command, just unrecognized)
	handled := h.Handle(",totally_unknown_cmd", &buf)
	c.Assert(handled, qt.IsTrue)
	c.Assert(strings.Contains(buf.String(), "Unknown command"), qt.IsTrue,
		qt.Commentf("output was: %q", buf.String()))
}

func TestMetaHandleDebugDelegation(t *testing.T) {
	c := qt.New(t)

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
			c.Assert(handled, qt.IsTrue)
			c.Assert(strings.Contains(buf.String(), tc.contain), qt.IsTrue,
				qt.Commentf("output %q should contain %q", buf.String(), tc.contain))
		})
	}
}
