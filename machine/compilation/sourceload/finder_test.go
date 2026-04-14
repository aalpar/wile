package sourceload_test

import (
	"errors"
	"io"
	"strings"
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/machine/compilation/sourceload"
)

func TestFinder_BasicFind(t *testing.T) {
	fsys := fstest.MapFS{
		"lib/foo.scm": &fstest.MapFile{Data: []byte("(define x 1)")},
	}
	f := sourceload.NewFinder(fsys, []string{"lib"})
	file, resolved, err := f.Open("foo.scm")
	if err != nil {
		t.Fatalf("Open: unexpected error: %v", err)
	}
	defer file.Close()
	if resolved != "lib/foo.scm" {
		t.Errorf("resolved path: got %q, want %q", resolved, "lib/foo.scm")
	}
}

func TestFinder_SearchOrder(t *testing.T) {
	// Same filename exists in two dirs; first dir must win.
	fsys := fstest.MapFS{
		"first/foo.scm":  &fstest.MapFile{Data: []byte("first")},
		"second/foo.scm": &fstest.MapFile{Data: []byte("second")},
	}
	f := sourceload.NewFinder(fsys, []string{"first", "second"})
	file, resolved, err := f.Open("foo.scm")
	if err != nil {
		t.Fatalf("Open: unexpected error: %v", err)
	}
	defer file.Close()
	if resolved != "first/foo.scm" {
		t.Errorf("resolved path: got %q, want %q", resolved, "first/foo.scm")
	}
}

func TestFinder_StackCurrentDir(t *testing.T) {
	fsys := fstest.MapFS{
		"lib/bar.scm": &fstest.MapFile{Data: []byte("bar")},
		"lib/foo.scm": &fstest.MapFile{Data: []byte("foo")},
	}
	stack := sourceload.NewLoadStack()
	stack.Push("lib/bar.scm")

	f := sourceload.NewFinder(fsys, nil, sourceload.WithStack(stack))
	file, resolved, err := f.Open("foo.scm")
	if err != nil {
		t.Fatalf("Open: unexpected error: %v", err)
	}
	defer file.Close()
	if resolved != "lib/foo.scm" {
		t.Errorf("resolved path: got %q, want %q", resolved, "lib/foo.scm")
	}
}

func TestFinder_FSRootFallback(t *testing.T) {
	fsys := fstest.MapFS{
		"foo.scm": &fstest.MapFile{Data: []byte("root")},
	}
	// No searchDirs — should still find via "." fallback.
	f := sourceload.NewFinder(fsys, nil)
	file, resolved, err := f.Open("foo.scm")
	if err != nil {
		t.Fatalf("Open: unexpected error: %v", err)
	}
	defer file.Close()
	if resolved != "foo.scm" {
		t.Errorf("resolved path: got %q, want %q", resolved, "foo.scm")
	}
}

func TestFinder_NotFound(t *testing.T) {
	fsys := fstest.MapFS{}
	f := sourceload.NewFinder(fsys, []string{"lib"})
	_, _, err := f.Open("missing.scm")
	if !errors.Is(err, sourceload.ErrNotFound) {
		t.Errorf("expected ErrNotFound, got %v", err)
	}
}

func TestFinder_EmptyPath(t *testing.T) {
	fsys := fstest.MapFS{
		"foo.scm": &fstest.MapFile{Data: []byte("x")},
	}
	f := sourceload.NewFinder(fsys, nil)
	_, _, err := f.Open("")
	if !errors.Is(err, sourceload.ErrNotFound) {
		t.Errorf("expected ErrNotFound for empty path, got %v", err)
	}
}

func TestFinder_Canonicalize(t *testing.T) {
	fsys := fstest.MapFS{
		"lib/foo.scm": &fstest.MapFile{Data: []byte("x")},
	}
	f := sourceload.NewFinder(fsys, []string{"lib"},
		sourceload.WithCanonicalize(strings.ToUpper),
	)
	file, resolved, err := f.Open("foo.scm")
	if err != nil {
		t.Fatalf("Open: unexpected error: %v", err)
	}
	defer file.Close()
	if resolved != "LIB/FOO.SCM" {
		t.Errorf("resolved path: got %q, want %q", resolved, "LIB/FOO.SCM")
	}
}

func TestFinder_NilStack(t *testing.T) {
	fsys := fstest.MapFS{
		"lib/foo.scm": &fstest.MapFile{Data: []byte("x")},
	}
	// WithStack(nil) must not cause a panic.
	f := sourceload.NewFinder(fsys, []string{"lib"}, sourceload.WithStack(nil))
	file, _, err := f.Open("foo.scm")
	if err != nil {
		t.Fatalf("Open with nil stack: unexpected error: %v", err)
	}
	defer file.Close()
}

func TestFinder_FileContent(t *testing.T) {
	want := "(define answer 42)"
	fsys := fstest.MapFS{
		"lib/answer.scm": &fstest.MapFile{Data: []byte(want)},
	}
	f := sourceload.NewFinder(fsys, []string{"lib"})
	file, _, err := f.Open("answer.scm")
	if err != nil {
		t.Fatalf("Open: unexpected error: %v", err)
	}
	defer file.Close()

	data, err := io.ReadAll(file)
	if err != nil {
		t.Fatalf("ReadAll: %v", err)
	}
	if string(data) != want {
		t.Errorf("file content: got %q, want %q", string(data), want)
	}
}
