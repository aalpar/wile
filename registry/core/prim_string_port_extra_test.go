// Copyright 2025 Aaron Alpar
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

package core_test

import (
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// String Port Tests (R7RS §6.13.1)

func TestOpenInputStringReadMultiple(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "a b c")))
			(list (read p) (read p) (read p)))
	`)
	qt.Assert(t, err, qt.IsNil)
	expected := values.List(
		values.NewSymbol("a"),
		values.NewSymbol("b"),
		values.NewSymbol("c"),
	)
	qt.Assert(t, result, values.SchemeEquals, expected)
}

func TestOpenInputStringWithNestedExpressions(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-input-string "(define x 10)")))
			(read p))
	`)
	qt.Assert(t, err, qt.IsNil)
	expected := values.List(
		values.NewSymbol("define"),
		values.NewSymbol("x"),
		values.NewInteger(10),
	)
	qt.Assert(t, result, values.SchemeEquals, expected)
}

func TestOpenOutputStringMultipleWrites(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
			(display "Hello" p)
			(display ", " p)
			(display "World!" p)
			(get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, str.Value, qt.Equals, "Hello, World!")
}

func TestOpenOutputStringWithDifferentTypes(t *testing.T) {
	result, err := runSchemeCode(t, `
		(let ((p (open-output-string)))
			(display "num: " p)
			(display 42 p)
			(display ", bool: " p)
			(display #t p)
			(get-output-string p))
	`)
	qt.Assert(t, err, qt.IsNil)
	str, ok := result.(*values.String)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, str.Value, qt.Equals, "num: 42, bool: #t")
}
