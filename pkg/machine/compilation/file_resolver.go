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

import "github.com/aalpar/wile/pkg/environment"

// FileResolver resolves and opens files for include/load operations.
// The interface is defined in the environment package; this alias keeps
// the name available in compilation without re-declaration.
//
// Concrete resolver types (OSFileResolver, EmbedFileResolver, FSFileResolver,
// ChainFileResolver) and their constructors live in the resolver sub-package.
// Backward-compatible aliases are provided in resolver_compat.go.
type FileResolver = environment.FileResolver
