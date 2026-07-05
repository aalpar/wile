package environment

import "testing"

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
