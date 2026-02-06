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

// Package testhelpers provides shared test infrastructure for Scheme primitive tests.
//
// Each helper creates a fresh [wile.Engine] internally, ensuring test isolation:
//
//	result, err := testhelpers.RunSchemeCode(t, "(+ 1 2)")
//	c.Assert(err, qt.IsNil)
//	c.Assert(result, values.SchemeEquals, values.NewInteger(3))
//
// # Helpers
//
//   - [RunSchemeCode]: evaluate Scheme code, return result
//   - [RunSchemeCodeWithTimeout]: evaluate with timeout (prevents hangs)
//   - [RunSchemeCodeWithContext]: evaluate with custom context
//   - [RunSchemeCodeExpectTrue]: assert result is #t
//   - [RunSchemeCodeExpectFalse]: assert result is #f
//   - [RunSchemeCodeExpectError]: assert evaluation raises an error
//
// # Test Case Structs
//
// [SchemeCodeTestCase] and [SchemeCodeErrorTestCase] enable consistent
// table-driven test patterns across primitive test files.
package testhelpers
