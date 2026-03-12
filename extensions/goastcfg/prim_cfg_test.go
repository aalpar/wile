package goastcfg_test

import (
	"testing"

	extgoastcfg "github.com/aalpar/wile/extensions/goastcfg"

	qt "github.com/frankban/quicktest"
)

func TestExtensionLibraryName(t *testing.T) {
	type libraryNamer interface {
		LibraryName() []string
	}
	namer, ok := extgoastcfg.Extension.(libraryNamer)
	qt.New(t).Assert(ok, qt.IsTrue)
	qt.New(t).Assert(namer.LibraryName(), qt.DeepEquals, []string{"wile", "goast", "cfg"})
}
