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

// bootstrapMacroSource contains essential derived expression forms
// (define-syntax). Loaded first so that procedures can use these macros.
//
// Source: registry/core/bootstrap_macros.scm (embedded at compile-time)
//
//go:embed bootstrap_macros.scm
var bootstrapMacroSource string

// bootstrapProcedureSource contains Scheme procedure definitions (define).
// Loaded after macros because these procedures use macros like case-lambda,
// let, begin, and and.
//
// Source: registry/core/bootstrap_procedures.scm (embedded at compile-time)
//
//go:embed bootstrap_procedures.scm
var bootstrapProcedureSource string

func addBootstrapMacros(r *registry.Registry) error {
	r.AddMacroSource(bootstrapMacroSource)
	r.AddMacroSource(bootstrapProcedureSource)
	return nil
}
