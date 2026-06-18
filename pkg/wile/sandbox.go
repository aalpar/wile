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

import "github.com/aalpar/wile/pkg/security"

// SandboxOption configures the sandbox modifier.
type SandboxOption func(*sandboxConfig)

type sandboxConfig struct {
	envPrefix string
}

// SandboxEnvPrefix sets the environment variable prefix that the
// sandbox allows reading. Default is "WILE_".
func SandboxEnvPrefix(prefix string) SandboxOption {
	return func(cfg *sandboxConfig) {
		cfg.envPrefix = prefix
	}
}

// WithSandbox layers a restrictive authorizer on top of any profile.
// File reads and stats are allowed; file writes and deletes are denied.
// Environment variable reads are prefix-filtered (default "WILE_").
// Code loading and process execution are denied.
//
// The sandbox layer is always intersected (most-restrictive-wins, via
// security.All) on top of whatever base authorizer the profile and/or
// WithAuthorizer resolve to. This holds regardless of option order — the
// composition is performed once at engine construction by resolveAuthorizer,
// so WithSandbox may appear before or after WithProfile/WithAuthorizer.
//
// Multiple WithSandbox calls accumulate: each layer is intersected with the
// previously recorded sandbox authorizer, so restrictions only ever tighten
// (a second call cannot silently widen the first). Intersection is order-
// independent for the allow/deny decision.
func WithSandbox(opts ...SandboxOption) EngineOption {
	scfg := &sandboxConfig{envPrefix: "WILE_"}
	for _, opt := range opts {
		opt(scfg)
	}

	sandboxAuth := security.SandboxAuthorizer(scfg.envPrefix)

	return func(cfg *engineConfig) {
		if cfg.sandboxAuthorizer != nil {
			cfg.sandboxAuthorizer = security.All(cfg.sandboxAuthorizer, sandboxAuth)
			return
		}
		cfg.sandboxAuthorizer = sandboxAuth
	}
}
