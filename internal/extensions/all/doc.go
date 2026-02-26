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

// Package all provides additional primitives for records, promises, and extended
// string/character operations.
//
// # Records (SRFI-9 style)
//
//   - make-record-type, record-type?, record?, record-type
//   - record-constructor, record-predicate
//   - record-accessor, record-modifier
//
// # Promises (R7RS 4.2.5)
//
//   - make-promise, promise?, force
//
// # Extended String Operations
//
//   - string-copy!, string-fill!
//   - string-map, string-for-each
//   - string-ci=?, string-ci<?, string-ci>?, string-ci<=?, string-ci>=?
//   - string-upcase, string-downcase, string-foldcase
//
// # Extended Character Operations
//
//   - char-ci=?, char-ci<?, char-ci>?, char-ci<=?, char-ci>=?
//   - char-alphabetic?, char-numeric?, char-whitespace?
//   - char-upper-case?, char-lower-case?
//   - char-upcase, char-downcase, char-foldcase
//   - digit-value
//
// Use [Extension] or [AddToRegistry] to register all primitives.
package all
