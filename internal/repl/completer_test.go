package repl

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/internal/bootstrap"
)

func TestSchemeCompleter_MetaCommand(t *testing.T) {
	c := qt.New(t)
	sc := NewSchemeCompleter(nil, []string{"help", "doc", "edit"})

	line := []rune(",he")
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, 2) // "he" prefix
	c.Assert(len(newLines) > 0, qt.IsTrue)
	// Should suggest "lp" (completing "help")
	c.Assert(string(newLines[0]), qt.Equals, "lp")
}

func TestSchemeCompleter_SchemeBinding(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	env, _, err := bootstrap.NewTopLevelWithRegistry(ctx)
	c.Assert(err, qt.IsNil)

	sc := NewSchemeCompleter(env, nil)
	line := []rune("(ca")
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, 2) // "ca" prefix
	// Should include completions like "r" (for car), "dr" (for cdr)
	c.Assert(len(newLines) > 0, qt.IsTrue)
}

func TestSchemeCompleter_EmptyInput(t *testing.T) {
	c := qt.New(t)
	sc := NewSchemeCompleter(nil, []string{"help"})
	line := []rune("")
	newLines, _ := sc.Do(line, 0)
	// Empty input: no completions
	c.Assert(len(newLines), qt.Equals, 0)
}

func TestSchemeCompleter_CommaOnly(t *testing.T) {
	c := qt.New(t)
	sc := NewSchemeCompleter(nil, []string{"help", "doc", "edit"})
	line := []rune(",")
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, 0) // empty prefix after ","
	// Should list all meta-commands
	c.Assert(len(newLines), qt.Equals, 3)
}

func TestSchemeCompleter_FileCompletion(t *testing.T) {
	c := qt.New(t)
	sc := NewSchemeCompleter(nil, []string{"edit"})

	// Create a temp dir with known files
	dir := t.TempDir()
	err := os.WriteFile(filepath.Join(dir, "foo.scm"), []byte(""), 0644)
	c.Assert(err, qt.IsNil)
	err = os.WriteFile(filepath.Join(dir, "foobar.scm"), []byte(""), 0644)
	c.Assert(err, qt.IsNil)

	// ",edit <dir>/foo" should complete to both files
	prefix := filepath.Join(dir, "foo")
	line := []rune(",edit " + prefix)
	newLines, length := sc.Do(line, len(line))
	c.Assert(length, qt.Equals, len(prefix))
	c.Assert(len(newLines), qt.Equals, 2) // foo.scm and foobar.scm

	// Verify ",edit " doesn't fall through to meta-command completion
	line2 := []rune(",edit nonexistent-path-xyz")
	newLines2, _ := sc.Do(line2, len(line2))
	c.Assert(len(newLines2), qt.Equals, 0) // no matches, but didn't complete as meta-command
}
