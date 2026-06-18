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

package core_test

import (
	"context"
	"regexp"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/docparse"
	"github.com/aalpar/wile/pkg/internal/bootstrap"
	"github.com/aalpar/wile/pkg/repl"
	"github.com/aalpar/wile/pkg/values"
)

// TestDocEntryNamesResolve verifies that every name with a doc string in the
// core registry (BindingSpecs with Doc and explicit DocEntries) resolves to a
// binding in at least one phase environment. This catches typos in the
// hand-authored name lists in specialforms.go that would otherwise cause
// documentation to silently vanish.
func TestDocEntryNamesResolve(t *testing.T) {
	c := qt.New(t)

	env, reg, err := bootstrap.NewTopLevelWithRegistry(context.Background())
	c.Assert(err, qt.IsNil)

	// Collect all names that ApplyDocs would attempt to inject.
	var names []string
	for _, spec := range reg.BindingSpecs() {
		if spec.Doc != "" {
			names = append(names, spec.Name)
		}
	}
	for _, doc := range reg.Docs() {
		names = append(names, doc.Name)
	}

	topLevel := env.Namespace()
	c.Assert(topLevel, qt.IsNotNil)
	phases := topLevel.Phases()

	for _, name := range names {
		t.Run(name, func(t *testing.T) {
			sym := values.NewSymbol(name)
			for _, phase := range phases.Phases() {
				phaseEnv := phases.Get(phase)
				if phaseEnv == nil {
					continue
				}
				if phaseEnv.GetBinding(sym, nil) != nil {
					return // resolved
				}
			}
			t.Errorf("doc entry %q does not resolve to a binding in any phase", name)
		})
	}
}

// angleParamPattern matches angle-bracket metavariables like <key>, <test>, etc.
// The optional preceding character is captured so we can skip Scheme display
// representations (#<eof>, #<namespace>, etc.) and R7RS record type names (<point>).
var angleParamPattern = regexp.MustCompile(`(.?)<([a-z][-a-z0-9]*)>`)

// checkAngleBracketParams reports angle-bracket metavariables in doc that are
// not Scheme display representations (#<...>) or the R7RS record type <point>.
func checkAngleBracketParams(t *testing.T, name, doc string) {
	t.Helper()
	for _, m := range angleParamPattern.FindAllStringSubmatch(doc, -1) {
		prefix := m[1]
		inner := m[2]
		if prefix == "#" || inner == "point" {
			continue
		}
		t.Errorf("doc string for %q uses angle-bracket param <%s> — use ALL-CAPS instead", name, inner)
	}
}

// TestDocStringsNoAngleBracketParams is a regression test ensuring that
// doc strings use ALL-CAPS metavariables (KEY, TEST, BODY) instead of
// angle-bracket notation (<key>, <test>, <body>).
func TestDocStringsNoAngleBracketParams(t *testing.T) {
	_, reg, err := bootstrap.NewTopLevelWithRegistry(context.Background())
	qt.Assert(t, err, qt.IsNil)

	for _, bs := range reg.BindingSpecs() {
		if bs.Doc == "" {
			continue
		}
		t.Run("binding/"+bs.Name, func(t *testing.T) {
			checkAngleBracketParams(t, bs.Name, bs.Doc)
		})
	}

	for _, de := range reg.Docs() {
		t.Run("doc/"+de.Name, func(t *testing.T) {
			checkAngleBracketParams(t, de.Name, de.Doc)
		})
	}

	for _, pr := range reg.Primitives() {
		if pr.Spec.Doc == "" {
			continue
		}
		t.Run("primitive/"+pr.Spec.Name, func(t *testing.T) {
			checkAngleBracketParams(t, pr.Spec.Name, pr.Spec.Doc)
		})
	}
}

// TestDocStringCategoriesVisible verifies that every Category: declared in
// a doc string actually appears in the RegistryDocProvider's category list.
// This catches the original bug: doc strings declaring "Category: conditionals"
// while ,topics didn't list "conditionals".
func TestDocStringCategoriesVisible(t *testing.T) {
	_, reg, err := bootstrap.NewTopLevelWithRegistry(context.Background())
	qt.Assert(t, err, qt.IsNil)

	provider := repl.NewRegistryDocProvider(reg, nil)
	categories := provider.Categories()
	catSet := make(map[string]bool, len(categories))
	for _, cat := range categories {
		catSet[cat] = true
	}

	// Check binding specs
	for _, bs := range reg.BindingSpecs() {
		if bs.Doc == "" {
			continue
		}
		parsed := docparse.ParseDocstring(bs.Doc)
		if parsed.Category == "" {
			continue
		}
		t.Run("binding/"+bs.Name, func(t *testing.T) {
			qt.Assert(t, catSet[parsed.Category], qt.IsTrue,
				qt.Commentf("category %q from %q not in Categories() list: %v",
					parsed.Category, bs.Name, categories))
		})
	}

	// Check doc entries
	for _, de := range reg.Docs() {
		parsed := docparse.ParseDocstring(de.Doc)
		if parsed.Category == "" {
			continue
		}
		t.Run("doc/"+de.Name, func(t *testing.T) {
			qt.Assert(t, catSet[parsed.Category], qt.IsTrue,
				qt.Commentf("category %q from %q not in Categories() list: %v",
					parsed.Category, de.Name, categories))
		})
	}
}

// TestDocStringCategoriesNonEmpty verifies that every category returned by
// Categories() has at least one entry in ByCategory().
func TestDocStringCategoriesNonEmpty(t *testing.T) {
	_, reg, err := bootstrap.NewTopLevelWithRegistry(context.Background())
	qt.Assert(t, err, qt.IsNil)

	provider := repl.NewRegistryDocProvider(reg, nil)
	for _, cat := range provider.Categories() {
		t.Run(cat, func(t *testing.T) {
			results := provider.ByCategory(cat)
			qt.Assert(t, len(results) > 0, qt.IsTrue,
				qt.Commentf("category %q is empty", cat))
		})
	}
}
