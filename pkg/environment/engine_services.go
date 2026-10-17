package environment

import (
	"sync"
	"sync/atomic"
)

// EngineServices co-locates the engine-lifetime, layering-opaque services that
// were previously scattered as bare any fields on Namespace. Allocated once by
// the root Namespace and shared by pointer: every child copies the
// *EngineServices at construction (exactly as it already copies
// registry/authorizer), so the whole namespace tree reads and writes one struct
// — no root() walk, one sync.RWMutex. Adding the next such service is a field
// here plus its two Namespace accessors.
//
// Concurrency layout invariant: read-mostly handles are grouped first; fields
// written at runtime follow them (currentDir), and the exportIndex lazy-cache and
// its RWMutex are LAST. exportIndexMu's word is written on every RLock
// (concurrent (apropos)); keeping it off the cache line shared with the
// read-mostly handles prevents false sharing across SRFI-18 threads. A
// write-once-at-build tenant joins the read-mostly group; a runtime-written one
// joins currentDir, never after the mutex.
type EngineServices struct {
	// Read-mostly service handles (write-once at engine build, then read across
	// all threads). New tenants join HERE, before the lazy-index block.
	ioState      any // *io.State (extensions/io), set at engine build via namespace-init hook
	formRegistry any // *forms.FormRegistry (internal/forms), set at engine build; nil => default

	// inlineThreshold forwards the engine's configured procedure-inlining
	// body-length bound (WithInlineThreshold) so runtime-triggered library
	// compilation (LoadLibrary) honors it instead of silently using the compiler
	// default. inlineThresholdSet distinguishes an explicit 0 (inlining disabled)
	// from "never configured" — a namespace not built by an Engine (e.g. a direct
	// LoadLibrary in a unit test) — where the reader falls back to its own default.
	inlineThreshold    int
	inlineThresholdSet bool

	// maxExpandDepth forwards the engine's configured macro-expansion recursion
	// bound (WithMaxExpandDepth) so runtime-triggered library compilation honors
	// it instead of silently using the expander default — the sibling of
	// inlineThreshold for the expand phase. maxExpandDepthSet distinguishes an
	// explicit 0 (bound disabled / unlimited) from "never configured" — a
	// namespace not built by an Engine — where the reader falls back to the
	// expander default.
	maxExpandDepth    int
	maxExpandDepthSet bool

	// Written at runtime, read across all threads.

	// currentDir is the engine's current directory, written by
	// set-current-directory! and read by every relative-path resolution. nil
	// means never set: readers take the host process's working directory LIVE, so
	// an engine that never enters a directory follows a host that chdirs. See
	// Namespace.CurrentDirectory.
	currentDir atomic.Pointer[string]

	// Lazy library-export cache + its guards, relocated verbatim from Namespace.
	exportIndex      any // *compilation.LibraryExportIndex; nil until built
	exportIndexBuilt bool
	exportIndexMu    sync.RWMutex
}
