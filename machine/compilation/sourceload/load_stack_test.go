package sourceload

import (
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestLoadStack_PushAndCurrent(t *testing.T) {
	s := NewLoadStack()
	s.Push("lib/scheme/base.sld")
	qt.Assert(t, s.Current(), qt.Equals, "lib/scheme/base.sld")
}

func TestLoadStack_PushPopCurrent(t *testing.T) {
	s := NewLoadStack()
	s.Push("lib/scheme/base.sld")
	s.Push("lib/scheme/write.sld")
	s.Pop()
	qt.Assert(t, s.Current(), qt.Equals, "lib/scheme/base.sld")
}

func TestLoadStack_CurrentDir(t *testing.T) {
	s := NewLoadStack()
	s.Push("lib/scheme/base.sld")
	qt.Assert(t, s.CurrentDir(), qt.Equals, "lib/scheme")
}

func TestLoadStack_CurrentDirEmpty(t *testing.T) {
	s := NewLoadStack()
	qt.Assert(t, s.CurrentDir(), qt.Equals, "")
}

func TestLoadStack_CurrentEmpty(t *testing.T) {
	s := NewLoadStack()
	qt.Assert(t, s.Current(), qt.Equals, "")
}

func TestLoadStack_Depth(t *testing.T) {
	s := NewLoadStack()
	s.Push("a.sld")
	s.Push("b.sld")
	s.Push("c.sld")
	qt.Assert(t, s.Depth(), qt.Equals, 3)
	s.Pop()
	qt.Assert(t, s.Depth(), qt.Equals, 2)
}

func TestLoadStack_PopOnEmpty(t *testing.T) {
	s := NewLoadStack()
	// Must not panic.
	s.Pop()
	qt.Assert(t, s.Depth(), qt.Equals, 0)
}

func TestLoadStack_PushEmptyPanics(t *testing.T) {
	s := NewLoadStack()
	defer func() {
		r := recover()
		qt.Assert(t, r, qt.IsNotNil)
	}()
	s.Push("")
}

func TestLoadStack_ConcurrentAccess(t *testing.T) {
	s := NewLoadStack()
	const goroutines = 50
	var wg sync.WaitGroup
	wg.Add(goroutines * 2)
	for range goroutines {
		go func() {
			defer wg.Done()
			s.Push("lib/scheme/base.sld")
		}()
		go func() {
			defer wg.Done()
			s.Pop()
		}()
	}
	wg.Wait()
}
