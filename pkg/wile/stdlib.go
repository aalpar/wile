package wile

import (
	"io/fs"

	"github.com/aalpar/wile/pkg/stdlib"
)

// StdLibFS provides the standard Scheme libraries shipped with wile
// (e.g., (wile algebra), (wile control), etc.) as an embedded filesystem.
// Library paths resolve as "lib/...". Consumers add it to the engine with
// WithSourceFS(StdLibFS) paired with WithLibraryPaths("lib"): the tree keeps
// its "lib/" prefix, which is not on the default search path, so WithSourceFS
// alone resolves nothing. (stdlib.FS is the prefix-stripped variant and needs
// no extra search path.)
//
// The bytes live in the pkg/stdlib package's embed (//go:embed lib).
// Because go:embed forbids "..", this package re-exports stdlib.LibFS rather
// than embedding the tree a second time; the two share one copy and one shape.
var StdLibFS fs.FS = stdlib.LibFS
