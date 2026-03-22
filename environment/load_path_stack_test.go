package environment

import (
	"errors"
	"path/filepath"
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/werr"
)

func TestLoadPathStack_EmptyStack(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	c.Assert(stack.Current(), qt.Equals, "")
	c.Assert(stack.CurrentDir(), qt.Equals, "")
	c.Assert(stack.Depth(), qt.Equals, 0)

	// Pop on empty stack should not panic
	stack.Pop()
	c.Assert(stack.Depth(), qt.Equals, 0)
}

func TestLoadPathStack_LIFOOrdering(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	paths := []string{
		"/app/main.scm",
		"/app/sub/helper.scm",
		"/app/util.scm",
	}

	// Push all paths
	for _, p := range paths {
		c.Assert(stack.Push(p), qt.IsNil)
	}

	c.Assert(stack.Depth(), qt.Equals, len(paths))

	// Pop in reverse order (LIFO)
	for i := len(paths) - 1; i >= 0; i-- {
		c.Assert(stack.Current(), qt.Equals, paths[i])
		stack.Pop()
	}

	c.Assert(stack.Depth(), qt.Equals, 0)
}

func TestLoadPathStack_CurrentDir(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	tcs := []struct {
		name string
		path string
		want string
	}{
		{
			name: "file in directory",
			path: "/app/scripts/main.scm",
			want: "/app/scripts",
		},
		{
			name: "file in root",
			path: "/main.scm",
			want: "/",
		},
		{
			name: "nested path",
			path: "/usr/local/share/wile/lib.scm",
			want: "/usr/local/share/wile",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(stack.Push(tc.path), qt.IsNil)
			c.Assert(stack.CurrentDir(), qt.Equals, tc.want)
			stack.Pop()
		})
	}
}

func TestLoadPathStack_PushEmptyPathReturnsError(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	err := stack.Push("")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrInvalidLoadPath), qt.IsTrue)
}

func TestLoadPathStack_RelativePaths(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	c.Assert(stack.Push("lib/math.sld"), qt.IsNil)
	c.Assert(stack.Current(), qt.Equals, "lib/math.sld")
	c.Assert(stack.CurrentDir(), qt.Equals, "lib")
	c.Assert(stack.Depth(), qt.Equals, 1)

	c.Assert(stack.Push("lib/impl.scm"), qt.IsNil)
	c.Assert(stack.Current(), qt.Equals, "lib/impl.scm")
	c.Assert(stack.CurrentDir(), qt.Equals, "lib")
	c.Assert(stack.Depth(), qt.Equals, 2)

	stack.Pop()
	c.Assert(stack.Current(), qt.Equals, "lib/math.sld")
	stack.Pop()
	c.Assert(stack.Depth(), qt.Equals, 0)
}

func TestLoadPathStack_MixedAbsoluteAndRelative(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	c.Assert(stack.Push("/app/main.scm"), qt.IsNil)
	c.Assert(stack.Push("lib/helper.scm"), qt.IsNil)
	c.Assert(stack.Current(), qt.Equals, "lib/helper.scm")
	c.Assert(stack.CurrentDir(), qt.Equals, "lib")

	stack.Pop()
	c.Assert(stack.Current(), qt.Equals, "/app/main.scm")
	c.Assert(stack.CurrentDir(), qt.Equals, "/app")
}

func TestLoadPathStack_CurrentDir_RelativeRootFile(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	c.Assert(stack.Push("main.scm"), qt.IsNil)
	c.Assert(stack.CurrentDir(), qt.Equals, ".")
}

func TestLoadPathStack_ConcurrentAccess(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	// This test verifies thread-safety (no data races), not LIFO correctness
	// under concurrent access. The plan documents that concurrent push/pop
	// from multiple goroutines can corrupt LIFO ordering.

	const numGoroutines = 10
	const opsPerGoroutine = 100

	var wg sync.WaitGroup
	wg.Add(numGoroutines)

	for i := range numGoroutines {
		go func(id int) {
			defer wg.Done()
			for range opsPerGoroutine {
				base := filepath.Join(string(filepath.Separator)+"tmp", "file")
				path := filepath.Join(base, string(rune('a'+id)), "test.scm")
				_ = stack.Push(path)
				_ = stack.Current()
				_ = stack.CurrentDir()
				_ = stack.Depth()
				stack.Pop()
			}
		}(i)
	}

	wg.Wait()

	// All goroutines pushed and popped the same number of times
	// Stack should be empty (though LIFO ordering may have been violated)
	c.Assert(stack.Depth(), qt.Equals, 0)
}

func TestLoadPathStack_MultipleOperations(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	// Push, check, push, check, pop, check
	c.Assert(stack.Push("/app/main.scm"), qt.IsNil)
	c.Assert(stack.Current(), qt.Equals, "/app/main.scm")
	c.Assert(stack.CurrentDir(), qt.Equals, "/app")
	c.Assert(stack.Depth(), qt.Equals, 1)

	c.Assert(stack.Push("/app/sub/helper.scm"), qt.IsNil)
	c.Assert(stack.Current(), qt.Equals, "/app/sub/helper.scm")
	c.Assert(stack.CurrentDir(), qt.Equals, "/app/sub")
	c.Assert(stack.Depth(), qt.Equals, 2)

	stack.Pop()
	c.Assert(stack.Current(), qt.Equals, "/app/main.scm")
	c.Assert(stack.CurrentDir(), qt.Equals, "/app")
	c.Assert(stack.Depth(), qt.Equals, 1)

	stack.Pop()
	c.Assert(stack.Current(), qt.Equals, "")
	c.Assert(stack.CurrentDir(), qt.Equals, "")
	c.Assert(stack.Depth(), qt.Equals, 0)
}
