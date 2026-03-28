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
	"testing"

	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
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
				if phaseEnv.GetBinding(sym) != nil {
					return // resolved
				}
			}
			t.Errorf("doc entry %q does not resolve to a binding in any phase", name)
		})
	}
}
