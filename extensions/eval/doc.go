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

// Package eval provides evaluation and environment primitives.
//
// # Evaluation (R7RS 6.12)
//
//   - eval: evaluate expression in given environment
//   - load: read and evaluate file contents
//   - current-load-path, current-load-directory, current-load-depth
//
// # Environments
//
//   - environment: create environment from import sets
//   - null-environment: create minimal R5RS environment
//   - scheme-report-environment: create R5RS environment
//
// # Expansion and compilation
//
//   - expand, expand-once: macro-expand a syntax object or datum
//   - compile: compile an expression to a callable thunk
//   - syntax-local-value, syntax-local-value/immediate
//   - make-compile-time-value
//   - syntax-local-introduce, syntax-local-identifier-as-binding
//
// Use [Extension] or [AddToRegistry] to register all primitives.
package eval
