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
	"os"
	"regexp"
	"slices"
	"testing"

	"github.com/aalpar/wile/pkg/values"
)

// syntaxFormsSourcePath: this test cannot import pkg/registry/core for
// SyntaxFormsSource — core imports pkg/machine/compilation
// (prim_reflection.go), so the import is a cycle in the test binary — and the
// two names it compares against are unexported here. It reads the .scm through
// the package directory go test runs in instead.
const syntaxFormsSourcePath = "../../registry/core/bootstrap_syntax.scm"

var defineSyntaxName = regexp.MustCompile(`(?m)^\(define-syntax\s+([^\s()]+)`)

// TestSchemeSyntaxFormsCoverExpanderRows (F1, F4): the exclusion set, the Go
// expander table, and bootstrap_syntax.scm name the same forms. A name in the
// slice without a Go row excludes nothing; a Scheme define-syntax with a Go row
// and no exclusion lands in the Primitive-typed slot and is then ignored in
// silence — the compiler dispatches the Go compiler by name anyway; a name in
// the slice the Scheme file does not define falls back to that same Go compiler.
func TestSchemeSyntaxFormsCoverExpanderRows(t *testing.T) {
	raw, err := os.ReadFile(syntaxFormsSourcePath)
	if err != nil {
		t.Fatal(err)
	}
	source := string(raw)
	rows := values.NewStringSet(len(primitiveExpanderEntries))
	for _, e := range primitiveExpanderEntries {
		rows.Set(e.Name)
	}
	defined := values.NewStringSet(8)
	for _, m := range defineSyntaxName.FindAllStringSubmatch(source, -1) {
		defined.Set(m[1])
	}
	for _, name := range schemeSyntaxFormNames {
		if !rows.ContainsOne(name) {
			t.Errorf("%s: in schemeSyntaxFormNames but has no Go expander row — nothing to exclude", name)
		}
		if !defined.ContainsOne(name) {
			t.Errorf("%s: excluded from the Go registration but bootstrap_syntax.scm does not define it", name)
		}
	}
	for _, m := range defineSyntaxName.FindAllStringSubmatch(source, -1) {
		name := m[1]
		if rows.ContainsOne(name) && !slices.Contains(schemeSyntaxFormNames, name) {
			t.Errorf("%s: defined in bootstrap_syntax.scm over a Go expander row but not excluded — the F1 collision", name)
		}
	}
}
