// Package sourceload provides file-finding and load-stack tracking
// for locating source files across virtual filesystems.
//
// The package is focused on file-path traversal, file loading, and
// load-stack management — isolated from Scheme evaluation concerns.
package sourceload

import "github.com/aalpar/wile/werr"

// ErrNotFound is returned when no matching file can be located across
// all provided search directories.
var ErrNotFound = werr.NewStaticError("sourceload: file not found")
