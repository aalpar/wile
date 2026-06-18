package sourceload

import (
	"errors"
	"io/fs"
	"path"
)

// Walk traverses fsys under each search directory, calling fn for every
// file where accept returns true. Hidden directories (name starting with
// ".") are skipped. Non-existent directories are silently skipped.
// Other root-level errors (permission, I/O) are propagated.
//
// accept receives the filename (not the full path).
// fn receives the slash-separated path relative to the search directory.
//
// No "." fallback — only walks directories explicitly provided.
// No deduplication — caller handles domain-specific identity.
// Per-directory errors are accumulated via errors.Join and returned
// alongside partial results. Individual file-level errors within a
// directory are skipped (best-effort enumeration).
func Walk(fsys fs.FS, searchDirs []string, accept func(name string) bool, fn func(relPath string)) error {
	var errs []error
	for _, dir := range searchDirs {
		err := walkDir(fsys, dir, accept, fn)
		if err != nil {
			errs = append(errs, err)
		}
	}
	return errors.Join(errs...)
}

// walkDir walks a single directory within fsys, calling fn for each file
// where accept returns true. Hidden directories are skipped. Non-existent
// directories are silently skipped.
func walkDir(fsys fs.FS, baseDir string, accept func(name string) bool, fn func(relPath string)) error {
	err := fs.WalkDir(fsys, baseDir, func(filePath string, d fs.DirEntry, walkErr error) error {
		if d == nil {
			// d is nil when the root itself fails to open.
			// Non-existent dirs are expected (silently skip).
			// Other errors (permission, I/O) propagate.
			if walkErr != nil && errors.Is(walkErr, fs.ErrNotExist) {
				return fs.SkipAll
			}
			return walkErr
		}
		if walkErr != nil {
			if d.IsDir() {
				return fs.SkipDir
			}
			return nil
		}
		if d.IsDir() {
			// Skip hidden subdirectories, but never skip the walk root itself.
			// The walk root can be "." whose Name() is "." and would otherwise
			// match the hidden check.
			if filePath != baseDir && IsHidden(d.Name()) {
				return fs.SkipDir
			}
			return nil
		}
		if !accept(d.Name()) {
			return nil
		}
		// Compute path relative to baseDir. path.Rel is not available for
		// slash paths, so we strip the prefix manually.
		relPath := relativeToBase(baseDir, filePath)
		fn(relPath)
		return nil
	})
	// fs.WalkDir returns an error if the root directory does not exist.
	// Treat that as a silent skip — non-existent search dirs are allowed.
	if err != nil && errors.Is(err, fs.ErrNotExist) {
		return nil // non-existent search dir: skip silently
	}
	return err
}

// relativeToBase returns filePath relative to baseDir using slash semantics.
// baseDir and filePath are both slash-separated FS paths.
func relativeToBase(baseDir, filePath string) string {
	if baseDir == "." {
		return filePath
	}
	prefix := baseDir + "/"
	if len(filePath) > len(prefix) && filePath[:len(prefix)] == prefix {
		return filePath[len(prefix):]
	}
	// Fallback: return the base name. Should not happen under normal use.
	return path.Base(filePath)
}

// IsHidden reports whether name is a hidden file or directory (starts with ".").
func IsHidden(name string) bool {
	return len(name) > 0 && name[0] == '.'
}
