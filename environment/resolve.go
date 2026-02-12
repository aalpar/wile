package environment

import (
	"os"
	"path/filepath"
	"strings"

	"github.com/aalpar/wile/values"
)

// ResolveFile finds a file by trying resolution strategies in order:
//  1. If path is absolute, use as-is
//  2. If stack has a current directory, try relative to it
//  3. Try each fallback directory in order
//
// Returns the absolute path of the first match, or an error listing
// all searched paths.
//
// All returned paths are guaranteed to be absolute. Symlinks in paths are
// preserved (not resolved to their target). For example, if /app/lib is a
// symlink to /usr/local/lib, resolving "foo.scm" from /app/lib/ will return
// /app/lib/foo.scm, not /usr/local/lib/foo.scm.
func ResolveFile(stack *LoadPathStack, path string, fallbackDirs []string) (string, error) {
	// Strategy 1: Absolute path - use as-is
	if filepath.IsAbs(path) {
		_, err := os.Stat(path)
		if err == nil {
			return path, nil
		}
		return "", values.NewForeignErrorf("file %q not found (absolute path)", path)
	}

	var searched []string

	// Strategy 2: Try relative to current load directory
	if stack != nil {
		currentDir := stack.CurrentDir()
		if currentDir != "" {
			candidate := filepath.Join(currentDir, path)
			_, err := os.Stat(candidate)
			if err == nil {
				absPath, err := filepath.Abs(candidate)
				if err != nil {
					return "", values.WrapForeignErrorf(err, "failed to get absolute path for %q", candidate)
				}
				return absPath, nil
			}
			searched = append(searched, currentDir+"/")
		}
	}

	// Strategy 3: Try each fallback directory
	for _, dir := range fallbackDirs {
		if dir == "" {
			continue
		}
		candidate := filepath.Join(dir, path)
		_, err := os.Stat(candidate)
		if err == nil {
			absPath, err := filepath.Abs(candidate)
			if err != nil {
				return "", values.WrapForeignErrorf(err, "failed to get absolute path for %q", candidate)
			}
			return absPath, nil
		}
		searched = append(searched, dir+"/")
	}

	// Not found - report all searched paths
	if len(searched) == 0 {
		return "", values.NewForeignErrorf("file %q not found (no search paths available)", path)
	}

	searchedList := strings.Join(searched, ", ")
	hint := ""
	if stack == nil || stack.CurrentDir() == "" {
		hint = " (load from a file context or set search paths)"
	}

	return "", values.NewForeignErrorf("file %q not found; searched: %s%s", path, searchedList, hint)
}
