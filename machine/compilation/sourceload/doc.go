// Package sourceload provides isolated, dependency-free file-finding
// utilities for locating Scheme source files across virtual filesystems.
//
// It is intentionally kept free of project imports so it can be tested
// and reasoned about independently of the rest of the machine package.
package sourceload

import "errors"

// ErrNotFound is returned when no matching file can be located across
// all provided search directories.
var ErrNotFound = errors.New("sourceload: file not found") //nolint:gocritic // intentional: zero project deps
