package sourceload_test

import (
	"sort"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine/compilation/sourceload"
)

// sldOnly is a reusable accept function that accepts .sld files.
func sldOnly(name string) bool {
	return len(name) > 4 && name[len(name)-4:] == ".sld"
}

func TestWalk_BasicWalk(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"lib/scheme/base.sld":  {},
		"lib/scheme/write.sld": {},
		"lib/srfi/1.sld":       {},
		"lib/readme.txt":       {},
	}

	var got []string
	err := sourceload.Walk(fsys, []string{"lib"}, sldOnly, func(relPath string) {
		got = append(got, relPath)
	})

	c.Assert(err, qt.IsNil)
	sort.Strings(got)
	c.Assert(got, qt.DeepEquals, []string{
		"scheme/base.sld",
		"scheme/write.sld",
		"srfi/1.sld",
	})
}

func TestWalk_HiddenDirsSkipped(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"lib/scheme/base.sld":    {},
		"lib/.hidden/secret.sld": {},
	}

	var got []string
	err := sourceload.Walk(fsys, []string{"lib"}, sldOnly, func(relPath string) {
		got = append(got, relPath)
	})

	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.DeepEquals, []string{"scheme/base.sld"})
}

func TestWalk_MultipleSearchDirs(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"a/foo.sld": {},
		"b/bar.sld": {},
	}

	var got []string
	err := sourceload.Walk(fsys, []string{"a", "b"}, sldOnly, func(relPath string) {
		got = append(got, relPath)
	})

	c.Assert(err, qt.IsNil)
	sort.Strings(got)
	c.Assert(got, qt.DeepEquals, []string{"bar.sld", "foo.sld"})
}

func TestWalk_NonExistentDirSilentlySkipped(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"lib/base.sld": {},
	}

	var got []string
	err := sourceload.Walk(fsys, []string{"does-not-exist"}, sldOnly, func(relPath string) {
		got = append(got, relPath)
	})

	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.HasLen, 0)
}

func TestWalk_NoDotFallback(t *testing.T) {
	c := qt.New(t)

	// Files at root — not found unless "." is explicitly in searchDirs.
	fsys := fstest.MapFS{
		"base.sld": {},
	}

	var got []string
	err := sourceload.Walk(fsys, []string{"lib"}, sldOnly, func(relPath string) {
		got = append(got, relPath)
	})

	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.HasLen, 0)
}

func TestWalk_DotSearchDirFindsRootFiles(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"base.sld": {},
	}

	var got []string
	err := sourceload.Walk(fsys, []string{"."}, sldOnly, func(relPath string) {
		got = append(got, relPath)
	})

	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.DeepEquals, []string{"base.sld"})
}

func TestWalk_RelPathRelativeToSearchDir(t *testing.T) {
	c := qt.New(t)

	fsys := fstest.MapFS{
		"lib/scheme/base.sld": {},
	}

	var got []string
	err := sourceload.Walk(fsys, []string{"lib"}, sldOnly, func(relPath string) {
		got = append(got, relPath)
	})

	c.Assert(err, qt.IsNil)
	// relPath must be relative to "lib", not include it.
	c.Assert(got, qt.DeepEquals, []string{"scheme/base.sld"})
}
