package sourceload

import (
	"context"
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestLoadStackFromContext_AbsentReturnsNil(t *testing.T) {
	qt.Assert(t, LoadStackFromContext(context.Background()), qt.IsNil)
}

func TestWithLoadStack_RoundTrip(t *testing.T) {
	s := NewLoadStack()
	ctx := WithLoadStack(context.Background(), s)
	qt.Assert(t, LoadStackFromContext(ctx), qt.Equals, s)
}

// A child context derived from a parent that carries a stack sees the parent's
// stack, but installing a new stack on the child shadows it without disturbing
// the parent's — the immutable-context layering the loader relies on for nested
// loads.
func TestWithLoadStack_ChildShadowsParent(t *testing.T) {
	parentStack := NewLoadStack()
	parent := WithLoadStack(context.Background(), parentStack)

	childStack := NewLoadStack()
	child := WithLoadStack(parent, childStack)

	qt.Assert(t, LoadStackFromContext(child), qt.Equals, childStack)
	qt.Assert(t, LoadStackFromContext(parent), qt.Equals, parentStack)
}

// The core isolation property behind the per-thread include fix: two contexts,
// each with its own stack, never observe each other's pushes. Run under -race to
// confirm the per-load-chain stacks are genuinely independent (no shared mutable
// LoadStack across goroutines).
func TestWithLoadStack_PerChainIsolation(t *testing.T) {
	var wg sync.WaitGroup
	results := make([]string, 2)
	dirs := []string{"srfi/1/predicates.scm", "srfi/13/internal.scm"}
	wantDirs := []string{"srfi/1", "srfi/13"}

	for i := range dirs {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			ctx := WithLoadStack(context.Background(), NewLoadStack())
			s := LoadStackFromContext(ctx)
			s.Push(dirs[i])
			results[i] = s.CurrentDir()
		}(i)
	}
	wg.Wait()

	for i := range wantDirs {
		qt.Assert(t, results[i], qt.Equals, wantDirs[i])
	}
}
