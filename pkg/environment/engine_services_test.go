package environment

import (
	"testing"
	"unsafe"
)

// TestEngineServices_IOStateSharedAcrossTree pins the seam-critical property: a
// child namespace and its root observe ONE ioState slot. Writes from either side
// are visible to the other. This holds under the current root()-delegation and
// must still hold after the pointer-shared refactor.
func TestEngineServices_IOStateSharedAcrossTree(t *testing.T) {
	root := NewNamespace()
	child := root.NewChildNamespace()

	root.SetIOState("io-A")
	got := child.IOState()
	if got != "io-A" {
		t.Fatalf("child.IOState() = %v, want io-A (child must see root's slot)", got)
	}

	child.SetIOState("io-B")
	got = root.IOState()
	if got != "io-B" {
		t.Fatalf("root.IOState() = %v, want io-B (child write must hit the shared slot)", got)
	}
}

// TestEngineServices_ExportIndexSharedAcrossTree pins the lazy-index slot's
// (value, built) round-trip across the tree, exercising the RWMutex path.
func TestEngineServices_ExportIndexSharedAcrossTree(t *testing.T) {
	root := NewNamespace()
	child := root.NewChildNamespace()

	_, built := root.ExportIndex()
	if built {
		t.Fatal("fresh namespace must report exportIndex not built")
	}

	root.SetExportIndex("idx")
	got, built := child.ExportIndex()
	if !built || got != "idx" {
		t.Fatalf("child.ExportIndex() = (%v, %v), want (idx, true)", got, built)
	}
}

// TestEngineServices_FormRegistrySharedAcrossTree pins that formRegistry rides
// the same shared struct as ioState — a child and its root observe one slot.
func TestEngineServices_FormRegistrySharedAcrossTree(t *testing.T) {
	root := NewNamespace()
	child := root.NewChildNamespace()

	root.SetFormRegistry("fr-A")
	got := child.FormRegistry()
	if got != "fr-A" {
		t.Fatalf("child.FormRegistry() = %v, want fr-A (shared slot)", got)
	}

	child.SetFormRegistry("fr-B")
	got = root.FormRegistry()
	if got != "fr-B" {
		t.Fatalf("root.FormRegistry() = %v, want fr-B (child write hits shared slot)", got)
	}
}

// TestEngineServices_ChildRuntimeSharesSlot confirms the library-loading frame
// (NewChildRuntime shares the parent *Namespace outright) also sees the slot.
func TestEngineServices_ChildRuntimeSharesSlot(t *testing.T) {
	root := NewNamespace()
	root.SetIOState("io-A")

	libFrame := root.NewChildRuntime()
	got := libFrame.Namespace().IOState()
	if got != "io-A" {
		t.Fatalf("library frame IOState() = %v, want io-A", got)
	}
}

// TestEngineServices_MTLayoutMutexBlockLast enforces the concurrency layout
// invariant structurally (see the EngineServices doc comment). SRFI-18 threads
// share one EngineServices; exportIndexMu's word is written on every RLock
// (concurrent (apropos)) while the read-mostly handles (ioState, formRegistry)
// are read lock-free. If the mutex block shared a cache line with the handles,
// that RLock churn would false-share with their reads. The invariant that
// prevents it is "read-mostly handles grouped first, mutex block last"; this
// test fails loudly if a future field reorder breaks it, converting the prose
// invariant into an enforced one.
func TestEngineServices_MTLayoutMutexBlockLast(t *testing.T) {
	var es EngineServices
	const cacheLine = 64

	ioOff := unsafe.Offsetof(es.ioState)
	frOff := unsafe.Offsetof(es.formRegistry)
	xiOff := unsafe.Offsetof(es.exportIndex)
	builtOff := unsafe.Offsetof(es.exportIndexBuilt)
	muOff := unsafe.Offsetof(es.exportIndexMu)

	// Declared field order must hold: the read-mostly handles (ioState,
	// formRegistry) first, then the exportIndex lazy-cache, with the churned
	// exportIndexMu last. Any reorder that moves the mutex ahead of a read-mostly
	// handle — or a handle behind the mutex — trips this and false-shares RLock
	// churn with the handles' lock-free reads. Strict-`<` ordering over the known
	// fields is robust: it needs no RWMutex-internal offsets or padding math.
	ordered := ioOff < frOff && frOff < xiOff && xiOff < builtOff && builtOff < muOff
	if !ordered {
		t.Fatalf("EngineServices field order broken (offsets ioState=%d formRegistry=%d exportIndex=%d "+
			"exportIndexBuilt=%d exportIndexMu=%d); required: read-mostly handles first, mutex block last",
			ioOff, frOff, xiOff, builtOff, muOff)
	}

	// The read-mostly handles must stay compact within the first cache line, so
	// churn on the mutex's later words cannot invalidate the line they occupy.
	handlesEnd := frOff + unsafe.Sizeof(es.formRegistry)
	if handlesEnd > cacheLine {
		t.Fatalf("read-mostly handles span %d bytes, exceeding a %d-byte cache line; "+
			"keep them compact at the front to avoid false sharing with the mutex",
			handlesEnd, cacheLine)
	}
}
