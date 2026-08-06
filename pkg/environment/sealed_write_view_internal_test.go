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

package environment

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

// The sealed axis is now built BY the registry constructor rather than handed to
// it, so "this owner did not build a declared row" is unrepresentable — the
// construction-time panic that used to guard it has nothing left to check. What
// remains worth pinning is the property that panic protected: every owner, of
// every kind, has EVERY row, over its own store.
//
// If a row were missing, SealedWriteViewAt would silently take its ordinary-view
// fallback and land a bootstrap macro or a special-form expander where a user can
// overwrite it in place — surfacing arbitrarily far away as a dead let-syntax,
// with nothing naming the construction bug.
func TestEveryOwnerMintsEverySealedRow(t *testing.T) {
	ns := NewNamespace()
	owners := map[string]*EnvironmentFrame{
		"namespace":   ns.Runtime(),
		"child ns":    ns.NewChildNamespace().Runtime(),
		"report ns":   ns.NewSchemeReportNamespace().Runtime(),
		"library env": ns.NewChildRuntime(),
	}
	for name, owner := range owners {
		t.Run(name, func(t *testing.T) {
			for _, phase := range sealedAxis {
				view, ok := owner.phases.sealedViewAt(phase)
				qt.Assert(t, ok, qt.IsTrue, qt.Commentf("phase %s", phase))
				qt.Assert(t, view, qt.IsNotNil, qt.Commentf("phase %s", phase))
				qt.Assert(t, view.GlobalEnvironment(), qt.Equals, owner.GlobalEnvironment(),
					qt.Commentf("phase %s", phase))
				qt.Assert(t, view.rank, qt.Equals, writeRankSealed, qt.Commentf("phase %s", phase))
			}
		})
	}
}
