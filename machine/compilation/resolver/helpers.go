// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package resolver

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine/compilation/sourceload"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/werr"
)

// SchemeIncludePathEnv is the environment variable name for the Scheme include path.
const SchemeIncludePathEnv = "SCHEME_INCLUDE_PATH"

// libraryExtensions lists recognized file extensions for Scheme library files,
// in resolution priority order. .sld (R7RS) first, .scm fallback for older code.
var libraryExtensions = []string{".sld", ".scm"}

// LibraryExtensions returns a copy of the recognized Scheme library file extensions.
func LibraryExtensions() []string {
	return append([]string(nil), libraryExtensions...)
}

// isSchemeFile reports whether the filename has a recognized Scheme file extension.
func isSchemeFile(name string) bool {
	for _, ext := range libraryExtensions {
		if strings.HasSuffix(name, ext) {
			return true
		}
	}
	return false
}

// isAuthorized reports whether the security authorizer permits loading the
// given path. Returns true when no authorizer is configured (open sandbox).
func isAuthorized(auth security.Authorizer, target string) bool {
	return security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   target,
	}) == nil
}

// osSearchDirs returns the fallback directory list for OS-based file search.
// Search order: library registry paths, SCHEME_INCLUDE_PATH, CWD.
// This order is shared by OSFileResolver.ResolveAndOpen and EnumerateFiles.
func osSearchDirs(env *environment.EnvironmentFrame) []string {
	var dirs []string
	reg := env.LibraryRegistry()
	if reg != nil {
		dirs = append(dirs, reg.GetSearchPaths()...)
	}
	includePath := os.Getenv(SchemeIncludePathEnv)
	if includePath != "" {
		dirs = append(dirs, filepath.SplitList(includePath)...)
	}
	cwd, err := os.Getwd()
	if err == nil {
		dirs = append(dirs, cwd)
	}
	return dirs
}

// openAuthorized performs security authorization then opens absPath on the OS filesystem.
func openAuthorized(auth security.Authorizer, absPath string) (fs.File, string, error) {
	err := security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   absPath,
	})
	if err != nil {
		return nil, "", err
	}
	f, err := os.Open(absPath)
	if err != nil {
		sentinel := werr.ErrFileOpen
		if errors.Is(err, os.ErrNotExist) {
			sentinel = werr.ErrFileNotFound
		}
		return nil, "", werr.WrapForeignErrorWithCause(sentinel, err, "open %s", absPath)
	}
	return f, absPath, nil
}

// WalkOSSchemeFiles walks baseDir on the OS filesystem, calling fn with the
// slash-separated path of each .sld/.scm file relative to baseDir.
// Hidden directories and unauthorized files are silently skipped.
// Returns the WalkDir error so callers can observe unexpected walk failures.
func WalkOSSchemeFiles(baseDir string, auth security.Authorizer, fn func(relPath string)) error {
	return filepath.WalkDir(baseDir, func(path string, d fs.DirEntry, walkErr error) error {
		if d == nil {
			return fs.SkipAll
		}
		if d.IsDir() {
			if walkErr != nil {
				return filepath.SkipDir
			}
			if path != baseDir && sourceload.IsHidden(d.Name()) {
				return filepath.SkipDir
			}
			return nil
		}
		absPath, absErr := filepath.Abs(path)
		if walkErr != nil || absErr != nil || !isSchemeFile(d.Name()) || !isAuthorized(auth, absPath) {
			return nil //nolint:nilerr // skip unreadable/irrelevant/denied files, continue walking
		}
		rel, relErr := filepath.Rel(baseDir, path)
		if relErr == nil {
			fn(filepath.ToSlash(rel))
		}
		return nil
	})
}

// WalkFSSchemeFiles walks baseDir in fsys, calling fn with the path of each
// .sld/.scm file relative to baseDir. Hidden directories and unauthorized
// files are silently skipped. Non-existent directories are skipped (fs.SkipAll).
// If skipSubdir is non-nil, subdirectory paths returning true are also skipped.
// Returns the WalkDir error so callers can observe unexpected walk failures.
func WalkFSSchemeFiles(fsys fs.FS, baseDir string, auth security.Authorizer, skipSubdir func(string) bool, fn func(relPath string)) error {
	prefix := baseDir
	if prefix == "." {
		prefix = ""
	}
	return fs.WalkDir(fsys, baseDir, func(path string, d fs.DirEntry, walkErr error) error {
		if d == nil {
			return fs.SkipAll
		}
		if d.IsDir() {
			if walkErr != nil {
				return fs.SkipDir
			}
			if path != baseDir && (sourceload.IsHidden(d.Name()) || (skipSubdir != nil && skipSubdir(path))) {
				return fs.SkipDir
			}
			return nil
		}
		if walkErr != nil || !isSchemeFile(d.Name()) {
			return nil //nolint:nilerr // skip unreadable/irrelevant files, continue walking
		}
		relPath := strings.TrimPrefix(path, prefix+"/")
		if prefix == "" {
			relPath = path
		}
		if !isAuthorized(auth, path) {
			return nil
		}
		fn(relPath)
		return nil
	})
}
