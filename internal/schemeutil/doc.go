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

// Package schemeutil provides conversion utilities between syntax, datum, and Go types.
//
// # Syntax/Datum Conversion
//
//   - [SyntaxValueToDatum]: strip source location and scope info from syntax
//   - [DatumToSyntaxValue]: wrap raw values with source context
//   - [IsSyntaxComment]: check for comment syntax types
//
// # Boolean Conversion
//
// For implementing Scheme predicates and control flow:
//
//   - [BoolToBoolean]: Go bool -> Scheme #t/#f
//   - [BooleanToBool]: Scheme #t/#f -> Go bool
//   - [ValueToBool]: Scheme truthiness (only #f is false)
//   - [ValueToBoolean]: coerce any value to Scheme boolean
//
// # Collection
//
//   - [AsList]: convert Go slice to proper Scheme list
package schemeutil
