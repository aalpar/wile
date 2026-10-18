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

package compilation

import (
	"reflect"
	"testing"

	qt "github.com/frankban/quicktest"
)

// Gate (I166-R2): include, include-ci and cond-expand splice at expand time, so
// none of their primitiveExpanderEntries rows may be expandUnchanged. func values
// are not comparable, hence the pointer comparison.
func TestSplicingFormsAreNotExpandUnchanged(t *testing.T) {
	unchanged := reflect.ValueOf((*ExpanderTimeContinuation).expandUnchanged).Pointer()
	for _, name := range []string{"include", "include-ci", "cond-expand"} {
		t.Run(name, func(t *testing.T) {
			found := false
			for _, e := range primitiveExpanderEntries {
				if e.Name != name {
					continue
				}
				found = true
				qt.Assert(t, reflect.ValueOf(e.Fn).Pointer(), qt.Not(qt.Equals), unchanged,
					qt.Commentf("%s is registered as expandUnchanged", name))
			}
			qt.Assert(t, found, qt.IsTrue, qt.Commentf("no primitiveExpanderEntries row for %s", name))
		})
	}
}
