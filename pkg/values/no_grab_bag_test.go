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
	"errors"
	"io/fs"
	"os"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestValuesHasNoGrabBagFile keeps pkg/values from regrowing a catch-all file.
// It is a filename predicate, so it refuses the obvious renames too: moving
// utils.go to misc.go would satisfy a bare os.Stat on utils.go while doing none
// of the work. Helpers live in the file of the type they serve.
func TestValuesHasNoGrabBagFile(t *testing.T) {
	names := []string{"utils.go", "util.go", "helpers.go", "misc.go", "common.go"}
	for _, name := range names {
		t.Run(name, func(t *testing.T) {
			_, err := os.Stat(name)
			qt.Assert(t, errors.Is(err, fs.ErrNotExist), qt.IsTrue,
				qt.Commentf("pkg/values/%s exists (err=%v)", name, err))
		})
	}
}
