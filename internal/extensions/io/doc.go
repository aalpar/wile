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

// Package io provides I/O primitives for reading and writing.
//
// # Textual Read (R7RS 6.13.2)
//
//   - read, read-char, peek-char, read-line, read-string
//   - read-syntax, read-token
//
// # Textual Write (R7RS 6.13.3)
//
//   - write, display, newline, write-char, write-string
//   - write-simple, write-shared
//   - flush-output-port
//
// # Binary I/O
//
//   - read-u8, peek-u8, write-u8
//   - read-bytevector, read-bytevector!, write-bytevector
//
// # Port Predicates
//
//   - port?, input-port?, output-port?
//   - textual-port?, binary-port?
//   - input-port-open?, output-port-open?
//   - eof-object, eof-object?
//
// # String and Bytevector Ports
//
//   - open-input-string, open-output-string, get-output-string
//   - open-input-bytevector, open-output-bytevector, get-output-bytevector
//
// # Port State
//
//   - current-input-port, current-output-port, current-error-port
//   - close-port, close-input-port, close-output-port
//   - call-with-port
//
// Use [Extension] or [AddToRegistry] to register all primitives.
package io
