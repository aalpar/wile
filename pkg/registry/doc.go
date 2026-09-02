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

// Package registry provides a plugin architecture for registering Scheme primitives.
//
// The registry allows extensions to register primitives that are applied to
// environments at initialization time. Primitives can be registered for
// runtime and expand-time; phase 2 and above are unnamed tower rungs the
// registry does not place primitives at.
//
// # Key Types
//
//   - [PrimitiveRegistry]: central store for primitive registrations
//   - [PrimitiveSpec]: defines a single primitive (name, params, implementation)
//   - [PhaseSet]: bit flags controlling environment placement (Runtime, Expand)
//   - [Extension]: interface for modular primitive packages
//
// # Registration
//
//	reg := registry.NewRegistry()
//	reg.AddPrimitives([]registry.PrimitiveSpec{
//	    {Name: "my-func", ParamCount: 1, Impl: myFuncImpl},
//	}, registry.PhaseSetRuntime|registry.PhaseSetExpand)
//
// # Application
//
// After registration, apply the registry to an environment:
//
//	err := reg.Apply(ctx, env)
//
// # Extensions
//
// Extensions implement the [Extension] interface and can be composed:
//
//	var Extension = registry.NewExtension("myext", AddToRegistry)
//
// The [RegistryBuilder] type provides a convenient way to compose multiple
// registration functions into a single extension.
package registry
