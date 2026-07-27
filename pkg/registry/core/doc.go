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

// Package core provides the essential primitives required for Scheme to function.
//
// This package registers primitives that are always included in any Wile engine:
//
// # Primitives
//
//   - Type predicates: null?, pair?, number?, string?, symbol?, etc.
//   - Equality: eq?, eqv?, equal?
//   - Pairs and lists: cons, car, cdr, list, append, reverse
//   - Arithmetic: +, -, *, /, comparisons, min, max, gcd, lcm
//   - Vectors: make-vector, vector-ref, vector-set!, vector->list
//   - Strings: string-length, string-ref, string-append, string comparisons
//   - Characters: char->integer, integer->char, char comparisons
//   - Bytevectors: make-bytevector, bytevector-u8-ref, utf8->string
//   - Control: apply, call/cc, values, call-with-values
//   - Syntax: identifier?, syntax->datum, datum->syntax
//
// # Bootstrap Macros
//
// The package also defines essential derived forms as syntax-rules macros:
// and, or, cond, case, when, unless, do, delay, delay-force, parameterize,
// guard, define-record-type, let-values, let*-values, define-values.
//
// # Registration
//
// Use [Extension] or [AddToRegistry] to register all core primitives:
//
//	reg := registry.NewRegistry()
//	core.AddToRegistry(reg)
package core
