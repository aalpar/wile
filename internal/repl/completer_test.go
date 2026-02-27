package repl

import (
	"context"
	"testing"

	"github.com/aalpar/wile/internal/bootstrap"
	qt "github.com/frankban/quicktest"
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
