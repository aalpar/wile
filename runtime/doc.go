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

// Package runtime provides the core API for embedding Wile Scheme in Go applications.
//
// This package exposes the essential functions for compiling and executing Scheme code:
//
//   - [Compile] transforms syntax into executable templates
//   - [Run] executes compiled templates
//   - [Load] reads and evaluates Scheme code from an io.Reader
//
// # Basic Usage
//
// To evaluate Scheme code from a string:
//
//	env, _ := bootstrap.NewNamespaceFrameTiny(ctx)
//	reader := strings.NewReader(`(+ 1 2)`)
//	err := runtime.Load(ctx, env, reader, "example.scm")
//
// # Creating Environments
//
// Use [github.com/aalpar/wile/internal/bootstrap.NewNamespaceFrameTiny]
// to create a top-level environment with all standard bindings.
package runtime
