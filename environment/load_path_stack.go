package environment

import (
	"path"
	"path/filepath"
	"sync"

	"github.com/aalpar/wile/werr"
)

// LoadPathStack tracks the stack of files currently being loaded.
// It maintains a LIFO stack of file paths (absolute or relative), enabling
// path resolution and load provenance tracking.
//
// # Threading and Concurrency
//
// The stack is thread-safe for concurrent access (uses sync.RWMutex), but does
// not guarantee correct LIFO ordering when multiple goroutines push/pop
// concurrently. This is an acceptable limitation:
//
//   - Single-threaded loading (the common case): Fully correct LIFO semantics
//   - SRFI-18 threads calling (load ...) concurrently: LIFO can corrupt
//   - Impact: Relative path resolution may use wrong directory
//
// # Design Rationale: Per-VM vs Per-Thread
//
// LoadPathStack is stored on Namespace (per-VM, not per-thread).
// This choice supports library loading across environment boundaries: when
// a library is loaded, it needs to resolve paths relative to the importing
// file, even though the library executes in its own isolated environment.
//
// Alternative considered: per-thread stacks (map[threadID]*LoadPathStack).
// Pros: Correct LIFO even with concurrent loads. Cons: More complex, and
// Wile's SRFI-18 threading is not yet complete enough to justify the complexity.
//
// Future: If SRFI-18 threading becomes more complete and concurrent file loading
// becomes common, consider migrating to per-thread stacks.
type LoadPathStack struct {
	mu    sync.RWMutex
	paths []string // absolute or relative paths; top = paths[len-1]
}

// NewLoadPathStack creates an empty load path stack.
func NewLoadPathStack() *LoadPathStack {
	return &LoadPathStack{
		paths: make([]string, 0, 8),
	}
}

// Push adds a path to the top of the stack.
// Returns a wrapped ErrInvalidLoadPath if the path is empty.
func (p *LoadPathStack) Push(s string) error {
	if s == "" {
		return werr.WrapForeignErrorf(werr.ErrInvalidLoadPath, "path must not be empty")
	}

	p.mu.Lock()
	defer p.mu.Unlock()

	p.paths = append(p.paths, s)
	return nil
}

// Pop removes the top path from the stack.
// Does nothing if the stack is empty (no error, no panic). This silent behavior
// is intentional to support defer patterns where the depth cannot be checked
// before popping.
func (p *LoadPathStack) Pop() {
	p.mu.Lock()
	defer p.mu.Unlock()

	if len(p.paths) > 0 {
		p.paths = p.paths[:len(p.paths)-1]
	}
}

// Current returns the path at the top of the stack without removing it.
// Returns empty string if the stack is empty.
func (p *LoadPathStack) Current() string {
	p.mu.RLock()
	defer p.mu.RUnlock()

	if len(p.paths) == 0 {
		return ""
	}
	return p.paths[len(p.paths)-1]
}

// CurrentDir returns the directory of the path at the top of the stack.
// Returns empty string if the stack is empty.
func (p *LoadPathStack) CurrentDir() string {
	current := p.Current()
	if current == "" {
		return ""
	}
	if filepath.IsAbs(current) {
		return filepath.Dir(current)
	}
	return path.Dir(current)
}

// Depth returns the number of paths on the stack.
func (p *LoadPathStack) Depth() int {
	p.mu.RLock()
	defer p.mu.RUnlock()

	return len(p.paths)
}
