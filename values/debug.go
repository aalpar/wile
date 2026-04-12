package values

// DebugLocation holds file/line/column for debug and error display.
// This is a simple struct for presentation layers (REPL, debugger UI),
// distinct from the SourceLocation interface which is a full Value type
// used by procedure-source-location.
type DebugLocation struct {
	File   string
	Line   int
	Column int
}

// DebugState provides read-only access to VM execution state.
// Implemented by the VM's MachineContext; consumed by presentation
// layers (REPL, debugger UI) without importing machine/.
type DebugState interface {
	// CurrentLocation returns the source location at the current
	// execution point, or nil if no source info is available.
	CurrentLocation() *DebugLocation

	// FormatStackTrace returns a human-readable stack trace string,
	// walking at most maxDepth frames.
	FormatStackTrace(maxDepth int) string
}
