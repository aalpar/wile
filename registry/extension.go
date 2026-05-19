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

// Describer is an optional interface that extensions can implement
// to provide a human-readable library description. The description
// is shown by ,doc (wile <ext>) and ,libraries in the REPL.
type Describer interface {
	Description() string
}

// Closeable is an opt-in interface for extensions that hold resources
// (goroutines, file handles, connections) and need cleanup when the
// engine is shut down. Extensions that implement this interface will
// have Close called by Engine.Close().
type Closeable interface {
	Close() error
}

// ExtensionFunc adapts a function to the Extension interface and
// carries the three optional capabilities as slots. The slots are
// populated via the ExtensionOption functions passed to NewExtension.
//
// *ExtensionFunc unconditionally satisfies LibraryNamer, Describer,
// and Closeable. Unset slots are reported as the zero value (empty
// string, nil slice, nil error). Engine code interprets emptiness
// uniformly as "not set" and falls back to defaults — there is no
// behavioral distinction between "did not opt in" and "opted in with
// the zero value."
type ExtensionFunc struct {
	name          string
	addToRegistry func(*Registry) error
	description   string
	libraryName   []string
	closeFn       func() error
}

// ExtensionOption configures an ExtensionFunc at construction time.
type ExtensionOption func(*ExtensionFunc)

// WithDescription attaches a human-readable description, surfaced by
// ,doc (wile <ext>) and ,libraries in the REPL.
func WithDescription(s string) ExtensionOption {
	return func(p *ExtensionFunc) {
		p.description = s
	}
}

// WithLibraryName overrides the default R7RS library name. The default,
// applied when this option is not used, is (wile <ext.Name()>).
func WithLibraryName(parts ...string) ExtensionOption {
	return func(p *ExtensionFunc) {
		p.libraryName = parts
	}
}

// WithClose attaches a cleanup hook called by Engine.Close().
func WithClose(fn func() error) ExtensionOption {
	return func(p *ExtensionFunc) {
		p.closeFn = fn
	}
}

// NewExtension creates an Extension from a name, a registration function,
// and zero or more capability options.
func NewExtension(name string, fn func(*Registry) error, opts ...ExtensionOption) Extension {
	q := &ExtensionFunc{
		name:          name,
		addToRegistry: fn,
	}
	for _, opt := range opts {
		opt(q)
	}
	return q
}

// NewDescribedExtension creates an Extension with a human-readable
// description. Retained as a thin forwarder for backward compatibility;
// new code should use NewExtension(..., WithDescription(desc)) directly.
func NewDescribedExtension(name, description string, fn func(*Registry) error) Extension {
	return NewExtension(name, fn, WithDescription(description))
}

// Name returns the extension name.
func (p *ExtensionFunc) Name() string {
	return p.name
}

// Description returns the extension's description, or "" if unset.
func (p *ExtensionFunc) Description() string {
	return p.description
}

// LibraryName returns the configured R7RS library name parts, or nil
// if unset. A nil/empty return signals "use the default (wile <name>)"
// to the engine.
func (p *ExtensionFunc) LibraryName() []string {
	return p.libraryName
}

// Close runs the configured close hook, or returns nil if no hook was set.
func (p *ExtensionFunc) Close() error {
	if p.closeFn == nil {
		return nil
	}
	return p.closeFn()
}

// AddToRegistry registers primitives with the registry.
func (p *ExtensionFunc) AddToRegistry(r *Registry) error {
	return p.addToRegistry(r)
}
