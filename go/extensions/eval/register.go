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

// Package eval provides eval and environment primitives.
//
//nolint:govet // Using unkeyed struct fields for concise primitive specs
package eval

import (
	"github.com/aalpar/wile/go/registry"
)

// Extension is the eval extension.
var Extension = registry.NewExtension("eval", AddToRegistry)

// Builder aggregates all eval registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all eval primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"eval", 2, false, PrimEval},
		{"load", 1, false, PrimLoad},
		{"interaction-environment", 0, false, PrimInteractionEnvironment},
		{"scheme-report-environment", 1, false, PrimSchemeReportEnvironment},
		{"null-environment", 1, false, PrimNullEnvironment},
		{"environment", 1, true, PrimEnvironment},
		{"expand", 1, false, PrimExpand},
		{"expand-once", 1, false, PrimExpandOnce},
		{"compile", 1, false, PrimCompile},
		{"syntax-local-value", 1, false, PrimSyntaxLocalValue},
		{"make-compile-time-value", 1, false, PrimMakeCompileTimeValue},
		{"syntax-local-introduce", 1, false, PrimSyntaxLocalIntroduce},
		{"syntax-local-identifier-as-binding", 1, false, PrimSyntaxLocalIdentifierAsBinding},
	}, registry.PhaseRuntime)
	return nil
}
