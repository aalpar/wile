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

package parser

import (
	"context"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/syntax"
)

// TestSetFileIsPathStampsEveryContext pins that the reader's path mark reaches
// every node it builds, nested ones included, and that a parser told nothing
// marks nothing: the zero value is a label, which is what keeps a caller's name
// from steering an include (lexicalIncludeContext).
func TestSetFileIsPathStampsEveryContext(t *testing.T) {
	tcs := []struct {
		name   string
		isPath bool
	}{
		{name: "label (default)", isPath: false},
		{name: "path", isPath: true},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			p := NewParserWithFile(nil, true, strings.NewReader(`(a (b "s") #(c) 'd)`), "dir/f.scm")
			if tc.isPath {
				p.SetFileIsPath(true)
			}
			stx, err := p.ReadSyntax(context.Background())
			qt.Assert(t, err, qt.IsNil)
			seen := 0
			var walk func(v syntax.SyntaxValue)
			walk = func(v syntax.SyntaxValue) {
				sc := v.SourceContext()
				qt.Assert(t, sc, qt.IsNotNil)
				qt.Assert(t, sc.File, qt.Equals, "dir/f.scm")
				qt.Assert(t, sc.FileIsPath, qt.Equals, tc.isPath, qt.Commentf("node %s", v.SchemeString()))
				seen++
				pr, ok := v.(*syntax.SyntaxPair)
				if !ok {
					return
				}
				walk(pr.SyntaxCar())
				cdr := pr.SyntaxCdr()
				if !syntax.IsSyntaxEmptyList(cdr) {
					walk(cdr)
				}
			}
			walk(stx)
			qt.Assert(t, seen > 8, qt.IsTrue, qt.Commentf("walked %d nodes", seen))
		})
	}
}
