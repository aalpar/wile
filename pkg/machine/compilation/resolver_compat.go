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

package compilation

import (
	"io/fs"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation/resolver"
)

// Backward-compatible type aliases for resolver types.
// External callers (engine.go, options.go, bootstrap) continue using
// compilation.NewOSFileResolver etc. without import changes.

type OSFileResolver = resolver.OSFileResolver
type EmbedFileResolver = resolver.EmbedFileResolver
type FSFileResolver = resolver.FSFileResolver
type ChainFileResolver = resolver.ChainFileResolver

// SchemeIncludePathEnv is the environment variable name for the Scheme include path.
const SchemeIncludePathEnv = resolver.SchemeIncludePathEnv

// NewOSFileResolver creates a resolver backed by the OS filesystem.
func NewOSFileResolver(env *environment.EnvironmentFrame) *OSFileResolver {
	return resolver.NewOSFileResolver(env)
}

// NewEmbedFileResolver creates a resolver backed by an embedded filesystem.
func NewEmbedFileResolver(fsys fs.FS) *EmbedFileResolver {
	return resolver.NewEmbedFileResolver(fsys)
}

// NewFSFileResolver creates a resolver backed by a virtual filesystem.
func NewFSFileResolver(fsys fs.FS, env *environment.EnvironmentFrame) *FSFileResolver {
	return resolver.NewFSFileResolver(fsys, env)
}

// NewChainFileResolver creates a resolver that searches multiple resolvers.
func NewChainFileResolver(resolvers []environment.FileResolver) *ChainFileResolver {
	return resolver.NewChainFileResolver(resolvers)
}
