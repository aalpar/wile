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

// Package testdata holds fixtures for the singlelinefunclint test. The
// functions below intentionally violate (and conform to) the no-single-line
// rule; line numbers are asserted by main_test.go, so do not reflow this file.
package testdata

func multiLine() int {
	return 1
}

func singleLine() int { return 2 }

func emptyBody() {}

func twoStmtsOneLine() { first(); second() }

func hasNestedLit() {
	fn := func() { inner() }
	_ = fn
}

func okLit() {
	fn := func() {
		inner()
	}
	_ = fn
}

func first()  {}
func second() {}
func inner()  {}
