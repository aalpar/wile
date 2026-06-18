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

// Package resolver provides file resolution infrastructure for the Scheme
// compiler. It owns the concrete FileResolver implementations that locate
// and open source files on the OS filesystem, embedded filesystems, and
// virtual fs.FS instances.
//
// The resolver package deals in file paths, not library names. Library
// identity and semantics are handled by the compilation package.
//
// FileResolver is defined in environment.FileResolver and is not
// redeclared here. The FileEnumerator interface extends resolvers with
// file discovery capability.
package resolver

// FileEnumerator is an optional interface that FileResolvers can implement
// to support file discovery. EnumerateFiles returns slash-separated
// relative paths to .sld/.scm files found by the resolver.
//
// Results are returned in discovery order with no deduplication; callers
// are responsible for library-level dedup and interpretation of paths.
type FileEnumerator interface {
	EnumerateFiles() ([]string, error)
}
