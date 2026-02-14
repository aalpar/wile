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
	"github.com/aalpar/wile/registry"
)

type engineConfig struct {
	registry     *registry.Registry
	extensions   []registry.Extension
	maxCallDepth uint64
}

// EngineOption configures an Engine.
type EngineOption func(*engineConfig)

// WithRegistry uses a custom registry instead of the default.
// When set, core primitives are NOT automatically added.
func WithRegistry(r *registry.Registry) EngineOption {
	return func(cfg *engineConfig) {
		cfg.registry = r
	}
}

// WithExtension adds an extension to the engine.
func WithExtension(ext registry.Extension) EngineOption {
	return func(cfg *engineConfig) {
		cfg.extensions = append(cfg.extensions, ext)
	}
}

// WithExtensions adds multiple extensions to the engine.
func WithExtensions(exts ...registry.Extension) EngineOption {
	return func(cfg *engineConfig) {
		cfg.extensions = append(cfg.extensions, exts...)
	}
}

// WithMaxCallDepth sets the maximum recursion depth for the VM.
// When the continuation stack exceeds this depth, ErrCallDepthExceeded is returned.
// A value of 0 means unlimited (the default).
func WithMaxCallDepth(n uint64) EngineOption {
	return func(cfg *engineConfig) {
		cfg.maxCallDepth = n
	}
}
