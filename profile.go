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

package wile

import (
	exteval "github.com/aalpar/wile/extensions/eval"
	"github.com/aalpar/wile/extensions/files"
	"github.com/aalpar/wile/extensions/gointerop"
	"github.com/aalpar/wile/extensions/introspection"
	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/extensions/process"
	"github.com/aalpar/wile/extensions/system"
	"github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/internal/extensions/all"
	"github.com/aalpar/wile/internal/extensions/envvars"
	ioext "github.com/aalpar/wile/internal/extensions/io"
	nsext "github.com/aalpar/wile/internal/extensions/namespace"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/security"
)

// Profile identifies a named environment configuration.
// Each profile defines which extensions are loaded and what
// authorization constraints apply.
type Profile int

const (
	// Tiny is a pure computational Scheme -- core primitives only.
	// No I/O, no filesystem, no threads. The lowest common denominator:
	// every other profile is a superset of Tiny.
	Tiny Profile = iota

	// Console adds I/O and sandboxed file access to Tiny.
	// All port primitives work. File operations restricted to /tmp.
	// stdin/stdout/stderr available. Environment variables read from
	// virtual env map only (no os.Getenv fallthrough).
	Console

	// ConsoleWithLoad is Console plus the eval extension, with an
	// authorizer that allows `code:load` under /tmp (in addition to
	// file r/w/d under /tmp). Enables (eval ...) and (load ...) within
	// the same /tmp security envelope. Process execution still denied.
	// Primary consumer: wile-goast and similar embedders that stage
	// Scheme files into /tmp and load them.
	ConsoleWithLoad

	// Small is R7RS-small complete -- all 16 (scheme ...) libraries.
	// Includes file I/O, system interface. No threads, no Go interop.
	Small

	// KitchenSink includes every available extension: threads, Go interop,
	// process execution, namespace manipulation.
	KitchenSink
)

// String returns the kebab-case name of the profile.
func (p Profile) String() string {
	switch p {
	case Tiny:
		return "tiny"
	case Console:
		return "console"
	case ConsoleWithLoad:
		return "console-with-load"
	case Small:
		return "small"
	case KitchenSink:
		return "kitchen-sink"
	default:
		return "unknown"
	}
}

// extensions returns the registry extensions for this profile.
// Tiny returns nil (core only). envvars.Extension will be added
// to each non-Tiny profile's list in Task 4 of this plan.
func (p Profile) extensions() []registry.Extension {
	switch p {
	case Tiny:
		return nil
	case Console:
		return []registry.Extension{
			ioext.Extension,
			files.Extension,
			math.Extension,
			all.SafeExtension,
			envvars.Extension,
		}
	case ConsoleWithLoad:
		return []registry.Extension{
			ioext.Extension,
			files.Extension,
			math.Extension,
			all.SafeExtension,
			exteval.Extension,
			envvars.Extension,
		}
	case Small:
		return []registry.Extension{
			ioext.Extension,
			files.Extension,
			math.Extension,
			introspection.Extension,
			exteval.Extension,
			all.Extension,
			system.Extension,
			envvars.Extension,
		}
	case KitchenSink:
		return []registry.Extension{
			ioext.Extension,
			files.Extension,
			math.Extension,
			introspection.Extension,
			exteval.Extension,
			nsext.Extension,
			threads.Extension,
			gointerop.Extension,
			all.Extension,
			system.Extension,
			process.Extension,
			envvars.Extension,
		}
	default:
		return nil
	}
}

// authorizer returns the built-in authorizer for this profile, or nil.
// Console uses ConsoleAuthorizer (/tmp file access, deny code/process).
// ConsoleWithLoad uses ConsoleWithLoadAuthorizer (Console + /tmp code load).
// Other profiles have no built-in authorizer.
func (p Profile) authorizer() security.Authorizer {
	switch p {
	case Console:
		return security.ConsoleAuthorizer()
	case ConsoleWithLoad:
		return security.ConsoleWithLoadAuthorizer()
	default:
		return nil
	}
}

// WithProfile configures the engine with the named profile's
// extensions and authorization constraints. WithProfile is additive:
// it appends the profile's extensions to any already configured via
// WithExtension/WithExtensions, and sets the profile's authorizer
// only if one is defined (otherwise leaves any prior authorizer in place).
func WithProfile(p Profile) EngineOption {
	return func(cfg *engineConfig) {
		cfg.extensions = append(cfg.extensions, p.extensions()...)
		auth := p.authorizer()
		if auth != nil {
			cfg.authorizer = auth
		}
	}
}
