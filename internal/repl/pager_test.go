package repl

import (
	"bytes"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestWriteWithPager_NoPager(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	writeWithPager(&buf, "hello world", "")
	c.Assert(buf.String(), qt.Equals, "hello world")
}

func TestWriteWithPager_EmptyContent(t *testing.T) {
	c := qt.New(t)
	var buf bytes.Buffer
	writeWithPager(&buf, "", "")
	c.Assert(buf.String(), qt.Equals, "")
}
