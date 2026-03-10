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

package core

import (
	"embed"
	"io/fs"

	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/werr"
)

// BootstrapFS embeds bootstrap.scm and the files it includes.
// bootstrap.scm uses (include ...) directives to load bootstrap_macros.scm
// and bootstrap_procedures.scm. The engine passes this FS to an
// EmbedFileResolver so the compiler's include form can resolve them.
//
//go:embed bootstrap.scm bootstrap_macros.scm bootstrap_procedures.scm
var BootstrapFS embed.FS

// addBootstrapSources registers bootstrap.scm as a single macro source.
// The (include ...) directives inside it are resolved by the compiler
// against BootstrapFS via EmbedFileResolver.
func addBootstrapSources(r *registry.Registry) error {
	source, err := fs.ReadFile(BootstrapFS, "bootstrap.scm")
	if err != nil {
		return werr.WrapForeignErrorf(
			werr.ErrFileNotFound, "bootstrap: reading bootstrap.scm",
		)
	}
	r.AddMacroSource(string(source))
	return nil
}
