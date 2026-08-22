package sourceload

import (
	"path"
	"slices"
	"sync"

	"github.com/aalpar/wile/pkg/werr"
)

// LoadStack tracks the chain of source files currently being loaded.
// Each Push records the path of a file being processed; Pop removes it
// when loading is complete. The stack enables directory-relative resolution
// of include paths — the current file's directory is the base for includes.
//
// All methods are safe for concurrent use.
type LoadStack struct {
	mu    sync.RWMutex
	paths []string
}

// NewLoadStack returns an empty LoadStack with pre-allocated capacity.
func NewLoadStack() *LoadStack {
	return &LoadStack{
		paths: make([]string, 0, 8),
	}
}

// Push records path as the current file being loaded.
// It panics if path is empty, as that indicates a programming error —
// only meaningful, non-empty paths should appear on the stack.
func (p *LoadStack) Push(path string) {
	if path == "" {
		panic(werr.WrapForeignErrorf(werr.ErrInvalidLoadPath, "LoadStack.Push: empty path"))
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	p.paths = append(p.paths, path)
}

// Pop removes the top path from the stack.
// It is a no-op if the stack is empty, which supports defer patterns
// where Pop may be called regardless of whether Push succeeded.
func (p *LoadStack) Pop() {
	p.mu.Lock()
	defer p.mu.Unlock()
	if len(p.paths) == 0 {
		return
	}
	p.paths = p.paths[:len(p.paths)-1]
}

// Current returns the path at the top of the stack, or "" if the stack is empty.
func (p *LoadStack) Current() string {
	p.mu.RLock()
	defer p.mu.RUnlock()
	if len(p.paths) == 0 {
		return ""
	}
	return p.paths[len(p.paths)-1]
}

// CurrentDir returns the directory component of the top path using
// slash-separated semantics (path.Dir, not filepath.Dir).
// Returns "" if the stack is empty.
func (p *LoadStack) CurrentDir() string {
	cur := p.Current()
	if cur == "" {
		return ""
	}
	return path.Dir(cur)
}

// Depth returns the number of paths on the stack.
func (p *LoadStack) Depth() int {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return len(p.paths)
}

// Paths returns a copy of the stack's paths, outermost first. It exists so a
// derived stack can be seeded from an existing one without sharing its storage:
// Engine.ContextWithLoadPath hands back a context carrying its OWN stack, so a
// caller cannot leak a push into the context it was given.
func (p *LoadStack) Paths() []string {
	p.mu.RLock()
	defer p.mu.RUnlock()
	return slices.Clone(p.paths)
}
