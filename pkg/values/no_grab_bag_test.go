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

package values_test

import (
	"os"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// grabBagStems are the file stems that name no owner. A file under one of them
// is a catch-all whatever it holds; helpers live in the file of the type they
// serve.
var grabBagStems = map[string]bool{
	"util":      true,
	"utils":     true,
	"utility":   true,
	"utilities": true,
	"helper":    true,
	"helpers":   true,
	"misc":      true,
	"common":    true,
}

// isGrabBagName reports whether a file name in pkg/values is a catch-all: its
// stem, lowercased, without ".go", "_test" and the package's own "values_"
// prefix (which names no owner either), is one of grabBagStems. The whole
// stem is compared, not a substring or a token, so an owner-named file such
// as port_helpers.go passes: substring matching was rejected for flagging
// exactly those.
func isGrabBagName(name string) bool {
	stem := strings.ToLower(name)
	stem, ok := strings.CutSuffix(stem, ".go")
	if !ok {
		return false
	}
	stem = strings.TrimSuffix(stem, "_test")
	stem = strings.TrimPrefix(stem, "values_")
	return grabBagStems[stem]
}

// TestValuesHasNoGrabBagFile keeps pkg/values from regrowing a catch-all file,
// test files included. It reads a directory listing, so Utils.go is caught on
// a case-sensitive filesystem too (a bare os.Stat only catches it on a
// case-insensitive one). It is a filename predicate: the split of helpers by
// owner is enforced by review, not by this test.
func TestValuesHasNoGrabBagFile(t *testing.T) {
	c := qt.New(t)
	entries, err := os.ReadDir(".")
	c.Assert(err, qt.IsNil)

	present := make(map[string]bool, len(entries))
	for _, e := range entries {
		present[strings.ToLower(e.Name())] = true
		c.Assert(isGrabBagName(e.Name()), qt.IsFalse, qt.Commentf("pkg/values/%s is a catch-all file name", e.Name()))
	}
	// A wrong working directory would make every absence check above vacuous.
	c.Assert(present["values.go"], qt.IsTrue, qt.Commentf("test is not running in pkg/values"))
}

// TestIsGrabBagName pins the predicate on names, so it is checked without
// creating a file in the package.
func TestIsGrabBagName(t *testing.T) {
	tcs := []struct {
		name string
		want bool
	}{
		{"utils.go", true},
		{"Utils.go", true},
		{"util.go", true},
		{"utility.go", true},
		{"helper.go", true},
		{"helpers.go", true},
		{"misc.go", true},
		{"common.go", true},
		{"values_utils.go", true},
		{"utils_test.go", true},
		{"values_helpers_test.go", true},
		{"values.go", false},
		{"port_helpers.go", false},
		{"ordering_helper_test.go", false},
		{"utils.txt", false},
		{"utilsx.go", false},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, isGrabBagName(tc.name), qt.Equals, tc.want)
		})
	}
}
