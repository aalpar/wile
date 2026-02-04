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
	_ "embed"

	"github.com/aalpar/wile/registry"
)

// bootstrapMacroSource contains essential derived expression forms.
// These macros are required for standard Scheme to work.
//
// Source: go/registry/core/bootstrap.scm (embedded at compile-time)
//
//go:embed bootstrap.scm
var bootstrapMacroSource string

func addBootstrapMacros(r *registry.Registry) error {
	r.AddMacroSource(bootstrapMacroSource)
	return nil
}
