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

package repl

import (
	"context"
	"strings"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

// DocInfo holds documentation for a primitive binding.
type DocInfo struct {
	Doc        string
	Syntax     string // e.g. "(if <test> <consequent> <alternate>)"
	TypeLabel  string // e.g. "special form", "syntax"
	ParamNames []string
	Category   string
	ParamCount int
	IsVariadic bool
	ParamTypes []values.TypeConstraint
	ReturnType values.TypeConstraint
	Keywords   []string
	// Origin is the binding's import-provenance root: the library that DEFINES
	// it and the name it is defined under, invariant to any export/import
	// renaming along the way. Nil for anything not reached by import (a
	// top-level define, a primitive read straight off the registry), which is
	// why it is rendered only when present.
	Origin *environment.OriginRef
}

// DocProvider looks up documentation for named bindings.
type DocProvider interface {
	// LookupDoc returns documentation for the named primitive.
	// Returns found=false if no documentation exists.
	LookupDoc(name string) (info DocInfo, found bool)
}

// StripExamples removes the Examples: section from a docstring.
// Returns the description portion only. If no Examples: section
// exists, returns the original string unchanged.
func StripExamples(doc string) string {
	before, _, found := strings.Cut(doc, "\n\nExamples:\n")
	if !found {
		return doc
	}
	return before
}

// DocSearchProvider extends DocProvider with search and category browsing.
type DocSearchProvider interface {
	DocProvider
	// Search returns entries whose name, doc, or category contains pattern
	// (case-insensitive substring match). Results are sorted by name.
	Search(ctx context.Context, pattern string) []registry.DocSearchResult
	// Categories returns sorted category names.
	Categories() []string
	// ByCategory returns entries in the named category, sorted by name.
	ByCategory(category string) []registry.DocSearchResult
}
