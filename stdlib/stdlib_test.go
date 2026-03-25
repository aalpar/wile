package stdlib_test

import (
	"io/fs"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/stdlib"
)

func TestFS_ContainsSchemeBase(t *testing.T) {
	c := qt.New(t)
	f, err := stdlib.FS.Open("lib/scheme/base.sld")
	c.Assert(err, qt.IsNil)
	f.Close()
}

func TestFS_ContainsExpectedLibraries(t *testing.T) {
	c := qt.New(t)

	expected := []string{
		"lib/scheme/base.sld",
		"lib/scheme/write.sld",
		"lib/scheme/char.sld",
		"lib/scheme/file.sld",
		"lib/chibi/test.sld",
		"lib/srfi/1.sld",
		"lib/wile/kanren.sld",
		"lib/wile/algebra.sld",
	}
	for _, path := range expected {
		_, err := fs.Stat(stdlib.FS, path)
		c.Assert(err, qt.IsNil, qt.Commentf("missing: %s", path))
	}
}
