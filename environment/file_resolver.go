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

package environment

import (
	"context"
	"io/fs"
)

// FileResolver resolves and opens files for include/load operations.
// Implementations control where files are found: the OS filesystem,
// an embedded filesystem, or any other fs.FS.
//
// The concrete implementations (OSFileResolver, FSFileResolver,
// EmbedFileResolver, ChainFileResolver) live in machine/compilation/resolver/.
// This interface is defined here so environment/ can store it without
// creating a circular import.
type FileResolver interface {
	// ResolveAndOpen finds a file by name and returns an open handle plus
	// the resolved path (used for load-path-stack tracking and error messages).
	ResolveAndOpen(ctx context.Context, path string) (fs.File, string, error)
}

// LibrarySearcher is implemented by library registries that support
// path-based file discovery. It is the minimum interface environment/
// needs from a LibraryRegistry: the set of directories to search when
// resolving include and load paths.
//
// The full *compilation.LibraryRegistry type implements this interface.
// Callers that need the full registry can type-assert from LibrarySearcher.
type LibrarySearcher interface {
	GetSearchPaths() []string
}

// PathTracker tracks the stack of files currently being loaded.
// Implementations provide relative path resolution for include/load
// and load provenance introspection.
//
// The concrete implementation is sourceload.LoadStack. This interface
// is defined here so environment/ can store it without importing
// machine/compilation/sourceload/.
type PathTracker interface {
	Push(path string)
	Pop()
	Current() string
	CurrentDir() string
	Depth() int
}
