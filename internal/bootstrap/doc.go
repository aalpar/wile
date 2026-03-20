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

// Package bootstrap initializes the top-level Scheme environment.
//
// The package creates and bootstraps complete runtime environments using
// the registry pattern:
//
//	env, err := bootstrap.NewNamespaceFrameTiny(ctx)
//
// # Initialization
//
// [NewNamespaceFrameTiny] performs these steps:
//  1. Creates a registry with core primitives
//  2. Adds all extensions (io, files, math, eval, exceptions, threads, etc.)
//  3. Creates a new Namespace with per-instance symbol interning
//  4. Applies all primitives to the environment
//  5. Registers syntax compilers and primitive expanders
//  6. Loads bootstrap macros
//
// # Library Environments
//
// [NewLibraryEnvironmentFrame] creates environments for R7RS libraries that
// share symbol interning with the caller but isolate bindings:
//
//	libEnv, err := bootstrap.NewLibraryEnvironmentFrame(ctx, callerEnv)
//
// This ensures (eq? 'foo (string->symbol "foo")) returns #t across library
// boundaries per R7RS 6.5.
package bootstrap
