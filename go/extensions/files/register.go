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

// Package files provides file I/O primitives.
//
//nolint:govet // Using unkeyed struct fields for concise primitive specs
package files

import (
	"wile/registry"
	"wile/runtime/primitives"
)

// Extension is the file I/O extension.
var Extension = registry.NewExtension("files", AddToRegistry)

// Builder aggregates all file registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all file primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"open-input-file", 1, false, primitives.PrimOpenInputFile},
		{"open-output-file", 1, false, primitives.PrimOpenOutputFile},
		{"open-binary-input-file", 1, false, primitives.PrimOpenBinaryInputFile},
		{"open-binary-output-file", 1, false, primitives.PrimOpenBinaryOutputFile},
		{"file-exists?", 1, false, primitives.PrimFileExistsQ},
		{"delete-file", 1, false, primitives.PrimDeleteFile},
		{"call-with-input-file", 2, false, primitives.PrimCallWithInputFile},
		{"call-with-output-file", 2, false, primitives.PrimCallWithOutputFile},
		{"with-input-from-file", 2, false, primitives.PrimWithInputFromFile},
		{"with-output-to-file", 2, false, primitives.PrimWithOutputToFile},
	}, registry.PhaseRuntime)
	return nil
}
