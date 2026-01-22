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
)

// Extension is the file I/O extension.
var Extension = registry.NewExtension("files", AddToRegistry)

// Builder aggregates all file registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all file primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{"open-input-file", 1, false, PrimOpenInputFile},
		{"open-output-file", 1, false, PrimOpenOutputFile},
		{"open-binary-input-file", 1, false, PrimOpenBinaryInputFile},
		{"open-binary-output-file", 1, false, PrimOpenBinaryOutputFile},
		{"file-exists?", 1, false, PrimFileExistsQ},
		{"delete-file", 1, false, PrimDeleteFile},
		{"call-with-input-file", 2, false, PrimCallWithInputFile},
		{"call-with-output-file", 2, false, PrimCallWithOutputFile},
		{"with-input-from-file", 2, false, PrimWithInputFromFile},
		{"with-output-to-file", 2, false, PrimWithOutputToFile},
	}, registry.PhaseRuntime)
	return nil
}
