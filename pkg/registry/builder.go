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

package registry

// RegistryBuilder collects functions that add primitives to a registry.
//
//nolint:revive // The name "RegistryBuilder" is intentionally explicit
type RegistryBuilder []func(*PrimitiveRegistry) error

// NewRegistryBuilder creates a builder with the given registration functions.
func NewRegistryBuilder(funcs ...func(*PrimitiveRegistry) error) RegistryBuilder {
	var q RegistryBuilder
	q.Register(funcs...)
	return q
}

// Register adds registration functions to the builder.
func (p *RegistryBuilder) Register(funcs ...func(*PrimitiveRegistry) error) {
	*p = append(*p, funcs...)
}

// AddToRegistry applies all registration functions to the registry.
func (p RegistryBuilder) AddToRegistry(r *PrimitiveRegistry) error {
	for _, f := range p {
		err := f(r)
		if err != nil {
			return err
		}
	}
	return nil
}

// Build creates a new registry and applies all registration functions.
func (p RegistryBuilder) Build() (*PrimitiveRegistry, error) {
	r := NewRegistry()
	err := p.AddToRegistry(r)
	if err != nil {
		return nil, err
	}
	return r, nil
}
