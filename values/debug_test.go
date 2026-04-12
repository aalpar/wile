package values_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func TestDebugLocation_Fields(t *testing.T) {
	loc := &values.DebugLocation{File: "test.scm", Line: 10, Column: 5}
	qt.Assert(t, loc.File, qt.Equals, "test.scm")
	qt.Assert(t, loc.Line, qt.Equals, 10)
	qt.Assert(t, loc.Column, qt.Equals, 5)
}
