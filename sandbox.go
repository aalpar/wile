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

import "github.com/aalpar/wile/security"

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
// When composed with a profile's built-in authorizer, the result is
// the intersection (most-restrictive-wins) via security.All().
//
// Ordering matters: WithSandbox must appear AFTER WithProfile and any
// WithAuthorizer calls. WithAuthorizer assigns the authorizer rather
// than composing, so placing it after WithSandbox would silently
// overwrite the sandbox restriction.
func WithSandbox(opts ...SandboxOption) EngineOption {
	scfg := &sandboxConfig{envPrefix: "WILE_"}
	for _, opt := range opts {
		opt(scfg)
	}

	sandboxAuth := security.SandboxAuthorizer(scfg.envPrefix)

	return func(cfg *engineConfig) {
		if cfg.authorizer != nil {
			cfg.authorizer = security.All(cfg.authorizer, sandboxAuth)
			return
		}
		cfg.authorizer = sandboxAuth
	}
}
