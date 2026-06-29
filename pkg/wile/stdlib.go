package wile

import (
	"io/fs"

	"github.com/aalpar/wile/pkg/stdlib"
)

// StdLibFS provides the standard Scheme libraries shipped with wile
// (e.g., (wile algebra), (wile control), etc.) as an embedded filesystem.
// Library paths resolve as "lib/...". Consumers add it to the engine with
// WithSourceFS(StdLibFS).
//
// The bytes live in the internal stdlib package's embed (//go:embed lib).
// Because go:embed forbids "..", this package re-exports stdlib.LibFS rather
// than embedding the tree a second time; the two share one copy and one shape.
var StdLibFS fs.FS = stdlib.LibFS
