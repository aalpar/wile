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

// TestValuesHasNoGrabBagFile keeps pkg/values from regrowing a catch-all file.
// It refuses exactly the five names below, compared case-insensitively against
// a directory listing, so Utils.go is caught on a case-sensitive filesystem
// too (a bare os.Stat only catches it on a case-insensitive one). It is
// a filename predicate: utility.go or values_utils.go pass, and the split of
// helpers by owner is enforced by review, not by this test. Helpers live in the
// file of the type they serve.
func TestValuesHasNoGrabBagFile(t *testing.T) {
	c := qt.New(t)
	entries, err := os.ReadDir(".")
	c.Assert(err, qt.IsNil)

	present := make(map[string]bool, len(entries))
	for _, e := range entries {
		present[strings.ToLower(e.Name())] = true
	}
	// A wrong working directory would make every absence check below vacuous.
	c.Assert(present["values.go"], qt.IsTrue, qt.Commentf("test is not running in pkg/values"))

	names := []string{"utils.go", "util.go", "helpers.go", "misc.go", "common.go"}
	for _, name := range names {
		c.Run(name, func(c *qt.C) {
			c.Assert(present[name], qt.IsFalse, qt.Commentf("pkg/values/%s exists", name))
		})
	}
}
