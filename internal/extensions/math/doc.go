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

// Package math provides transcendental and advanced mathematical primitives.
//
// # Transcendental Functions (R7RS 6.2.6)
//
//   - sin, cos, tan, asin, acos, atan
//   - exp, log
//   - sqrt, expt
//
// # Rounding
//
//   - floor, ceiling, truncate, round
//
// # Numeric Conversion
//
//   - numerator, denominator
//   - rationalize
//   - exact-integer-sqrt
//
// Use [Extension] or [AddToRegistry] to register all primitives.
package math
