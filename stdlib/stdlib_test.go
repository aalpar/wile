package stdlib_test

import (
	"io/fs"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/stdlib"
)

func TestFS_ContainsSchemeBase(t *testing.T) {
	c := qt.New(t)
	f, err := stdlib.FS.Open("scheme/base.sld")
	c.Assert(err, qt.IsNil)
	c.Assert(f.Close(), qt.IsNil)
}

func TestFS_ContainsExpectedLibraries(t *testing.T) {
	c := qt.New(t)

	expected := []string{
		"scheme/base.sld",
		"scheme/write.sld",
		"scheme/char.sld",
		"scheme/file.sld",
		"chibi/test.sld",
		"srfi/1.sld",
		"wile/kanren.sld",
		"wile/algebra.sld",
	}
	for _, path := range expected {
		_, err := fs.Stat(stdlib.FS, path)
		c.Assert(err, qt.IsNil, qt.Commentf("missing: %s", path))
	}
}
