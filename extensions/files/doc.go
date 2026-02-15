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

// Package files provides file I/O primitives.
//
// # Textual File I/O (R7RS 6.13.1)
//
//   - open-input-file, open-output-file
//   - call-with-input-file, call-with-output-file
//   - with-input-from-file, with-output-to-file
//
// # Binary File I/O
//
//   - open-binary-input-file, open-binary-output-file
//
// # File Predicates
//
//   - file-exists?, delete-file
//
// Use [Extension] or [AddToRegistry] to register all primitives.
package files
