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

// Extension represents a loadable extension that adds primitives to a registry.
type Extension interface {
	// Name returns the extension name for logging/debugging.
	Name() string
	// AddToRegistry registers primitives with the registry.
	AddToRegistry(r *Registry) error
}

// LibraryNamer is an optional interface that extensions can implement
// to control their R7RS library name. Extensions that don't implement
// this get the default name (wile <ext.Name()>).
type LibraryNamer interface {
	LibraryName() []string
}

// Closeable is an opt-in interface for extensions that hold resources
// (goroutines, file handles, connections) and need cleanup when the
// engine is shut down. Extensions that implement this interface will
// have Close called by Engine.Close().
type Closeable interface {
	Close() error
}

// ExtensionFunc adapts a function to the Extension interface.
type ExtensionFunc struct {
	name string
	fn   func(*Registry) error
}

// NewExtension creates an Extension from a name and function.
func NewExtension(name string, fn func(*Registry) error) Extension {
	q := &ExtensionFunc{name: name, fn: fn}
	return q
}

// Name returns the extension name.
func (p *ExtensionFunc) Name() string {
	return p.name
}

// AddToRegistry registers primitives with the registry.
func (p *ExtensionFunc) AddToRegistry(r *Registry) error {
	return p.fn(r)
}
