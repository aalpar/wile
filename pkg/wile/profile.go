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
	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/security"
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

// extensions returns the registry extensions for this profile. Delegates to
// bootstrap.ProfileExtensions so the mapping has a single source of truth
// shared with (environment '(wile <name>)) dispatch in extensions/eval.
func (p Profile) extensions() []registry.Extension {
	exts, err := bootstrap.ProfileExtensions(p.String())
	if err != nil {
		// Unreachable for valid Profile values: p.String() returns "unknown"
		// for out-of-range, which ProfileExtensions rejects. Out-of-range
		// profiles already yielded nil in the prior implementation; keep
		// that behavior rather than panicking.
		return nil
	}
	return exts
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
// WithExtension/WithExtensions, and records the profile's authorizer
// only if one is defined. An explicit WithAuthorizer always takes
// precedence over the profile's authorizer regardless of option order
// (see resolveAuthorizer), so profile and authorizer options compose
// commutatively.
//
// Across multiple WithProfile calls the last profile that defines an
// authorizer wins; a later profile with no authorizer (e.g. Tiny,
// Small, KitchenSink) does not clear an earlier one. This fails safe —
// a prior restriction is retained rather than silently dropped.
//
// Per-profile envMap behavior (all five current profiles):
//   - Tiny: envMap untouched; profile registers no extensions, so envvars primitives are absent.
//   - Console: allocates empty envMap when unset; sandboxes to the virtual map (no os.Getenv fallthrough).
//   - ConsoleWithLoad: same as Console (empty envMap when unset).
//   - Small: envMap untouched. envvars primitives are registered and fall through to os.Getenv when envMap is nil, gated by the authorizer (if any).
//   - KitchenSink: same fallthrough behavior as Small.
//
// When WithEnv/WithEnvMap is combined with WithProfile(Console*), the
// caller-supplied contents are preserved: WithProfile only allocates an
// empty map when none is set. Option order matters — a later WithEnvMap(nil)
// re-nils the map and opens the sandbox.
func WithProfile(p Profile) EngineOption {
	return namespaceConsumedOption(func(cfg *engineConfig) {
		cfg.profileSet = true
		cfg.extensions = append(cfg.extensions, p.extensions()...)
		auth := p.authorizer()
		if auth != nil {
			cfg.profileAuthorizer = auth
		}
		if (p == Console || p == ConsoleWithLoad) && cfg.envMap == nil {
			cfg.envMap = make(map[string]string)
		}
	})
}
