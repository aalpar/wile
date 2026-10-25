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
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestSourceContextLocationEscapesFileName pins the render-time escape: a file
// name reaches Location from program data (open-input-file, process pipe
// labels), and a raw newline in it split one diagnostic into two lines, the
// second a forged "file:line:col". Only names holding a control character are
// quoted; a plain name, colons and spaces included, prints as it always did.
func TestSourceContextLocationEscapesFileName(t *testing.T) {
	at := values.NewSourceIndexes(0, 3, 5)
	tcs := []struct {
		name, file, want string
	}{
		{"plain name unchanged", "test.scm", "test.scm:5:3"},
		{"path with spaces and a colon unchanged", "C:/my dir/a.scm", "C:/my dir/a.scm:5:3"},
		{"newline is quoted", "a\nb.scm", `"a\nb.scm":5:3`},
		{"tab is quoted", "a\tb.scm", `"a\tb.scm":5:3`},
		{"escape is quoted", "a\x1b[31mb.scm", `"a\x1b[31mb.scm":5:3`},
		{"nameless keeps the bare position", "", ":5:3"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			sc := values.NewSourceContext("", tc.file, at, at)
			qt.New(t).Assert(sc.Location(), qt.Equals, tc.want)
		})
	}
}
