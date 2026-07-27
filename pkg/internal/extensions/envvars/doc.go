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

// Package envvars provides environment variable access primitives.
//
// This extension is split from the system extension because environment
// variable access is a configuration concern, not a system interface.
// Console profile includes this for capability-oriented configuration
// via virtual env maps.
//
// When the caller's Namespace has a non-nil EnvMap, lookups read from
// that virtual map. Otherwise, they fall through to os.LookupEnv/os.Environ
// gated by the authorizer.
//
// Primitives:
//   - get-environment-variable: look up a single variable
//   - get-environment-variables: return alist of all visible variables
package envvars
