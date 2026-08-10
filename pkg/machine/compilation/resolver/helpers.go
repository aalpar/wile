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
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation/sourceload"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/werr"
)

// SelectLoadStack returns the LoadStack that governs directory-relative
// resolution for this call. It is the single selection point shared by the
// FS/OS resolvers (which read the current directory from it) and the
// (include …) compiler (which pushes the included file onto it), so the push
// target and the read target are guaranteed to be the same object — that
// identity is what closes the per-thread include-resolution race.
//
// The stack is carried on ctx and nowhere else, so it is per load CHAIN:
// concurrent library loads, concurrent (load …) calls on separate SRFI-18
// threads, and an embedder's own Engine.ContextWithLoadPath each get their own.
// Returns nil outside any load, which is not an error — a top-level (include …)
// with no enclosing load resolves against the search paths.
//
// This used to fall back to a single LoadStack hung off the Namespace when ctx
// carried none. That stack was shared by every thread in the engine, so two
// threads loading files in different directories interleaved their pushes and
// one compiled the other's file — a source substitution, invisible to -race
// because it is not a data race. The fallback is gone and the per-namespace
// stack with it; there is one discipline now, not two.
func SelectLoadStack(ctx context.Context) *sourceload.LoadStack {
	return sourceload.LoadStackFromContext(ctx)
}

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
// given path, drawn from source (empty = the host OS filesystem).
// Returns true when no authorizer is configured (open sandbox).
func isAuthorized(auth security.Authorizer, target, source string) bool {
	return security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource:     security.ResourceCode,
		Action:       security.ActionLoad,
		Target:       target,
		TargetSource: source,
	}) == nil
}

// IsNotFound reports whether err means the file is genuinely absent, which is
// the only condition that licenses continuing a search: falling through to the
// next resolver in a chain, or from a library's .sld to its .scm.
//
// It accepts both sentinels because both are minted for absence and neither
// implies the other: werr.ErrFileNotFound is a bare static sentinel with no
// fs.ErrNotExist in its chain, and a raw fs.ErrNotExist arrives from the
// virtual-filesystem side without ever being relabelled.
func IsNotFound(err error) bool {
	return errors.Is(err, werr.ErrFileNotFound) || errors.Is(err, fs.ErrNotExist)
}

// authorizeCandidates walks candidates in search order, authorizing each one
// BEFORE open is allowed to touch it, and returns the first candidate that is
// both permitted and present.
//
// The ordering is the point. Locating a candidate first and gating afterwards
// hands out an oracle (a denied path that exists answers differently from one
// that does not) and, when open blocks or has side effects — a FIFO under
// os.DirFS, an embedder's instrumented fs.FS — performs the very act the
// authorizer was about to refuse. Here a refusal costs the candidate nothing.
//
// A denial does not abort the search: a later search directory may hold a
// permitted copy. The FIRST denial is remembered and reported only if no
// candidate succeeds, so denied-and-existent and denied-and-absent are
// indistinguishable. Absence continues the search; any other open failure is
// propagated with its cause, never relabelled as absence.
//
// source labels the namespace the candidates are drawn from; see
// security.AccessRequest.TargetSource. Returns sourceload.ErrNotFound when
// every candidate was absent, so the caller can build its own searched-dirs
// diagnostic.
func authorizeCandidates(
	auth security.Authorizer,
	source string,
	candidates []string,
	open func(candidate string) (fs.File, error),
) (fs.File, string, error) {
	var denied error
	for _, candidate := range candidates {
		authErr := security.CheckWithAuthorizer(auth, security.AccessRequest{
			Resource:     security.ResourceCode,
			Action:       security.ActionLoad,
			Target:       candidate,
			TargetSource: source,
		})
		if authErr != nil {
			if denied == nil {
				denied = authErr
			}
			continue
		}
		f, openErr := open(candidate)
		if openErr == nil {
			return f, candidate, nil
		}
		if errors.Is(openErr, fs.ErrNotExist) {
			continue
		}
		return nil, "", werr.WrapForeignErrorWithCause(werr.ErrFileOpen, openErr, "open %s", candidate)
	}
	if denied != nil {
		return nil, "", denied
	}
	return nil, "", sourceload.ErrNotFound
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

// openAuthorized performs security authorization then opens absPath on the OS
// filesystem, through os.Root when the authorizer confines access to a root so
// the check and the open cannot observe different files (see confined.go).
func openAuthorized(auth security.Authorizer, absPath string) (fs.File, string, error) {
	err := security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceCode,
		Action:   security.ActionLoad,
		Target:   absPath,
	})
	if err != nil {
		return nil, "", err
	}
	f, err := confinedOpenFile(auth, absPath)
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
		if walkErr != nil || absErr != nil || !isSchemeFile(d.Name()) || !isAuthorized(auth, absPath, "") {
			return nil //nolint:nilerr // skip unreadable/irrelevant/denied files, continue walking
		}
		rel, relErr := filepath.Rel(baseDir, path)
		if relErr == nil {
			fn(filepath.ToSlash(rel))
		}
		return nil
	})
}
