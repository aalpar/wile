package goastlint_test

import (
	"testing"

	extgoastlint "github.com/aalpar/wile/extensions/goastlint"

	qt "github.com/frankban/quicktest"
)

func TestExtensionLibraryName(t *testing.T) {
	type libraryNamer interface {
		LibraryName() []string
	}
	namer, ok := extgoastlint.Extension.(libraryNamer)
	qt.New(t).Assert(ok, qt.IsTrue)
	qt.New(t).Assert(namer.LibraryName(), qt.DeepEquals, []string{"wile", "goast", "lint"})
}
