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

import "github.com/aalpar/wile/pkg/environment"

// FindLibraryBindingForTest exposes findLibraryBinding to the external test package.
// The function decides a binding's export PHASE by probe order, which is the property
// TestFindLibraryBindingPrefersRuntimeOverExpand pins.
func FindLibraryBindingForTest(lib *CompiledLibrary, internalName string) (*environment.Binding, environment.Phase, bool) {
	return findLibraryBinding(lib, internalName)
}

// SyntaxCompilerNamesForTest exposes the syntaxCompilerEntries names to the
// external test package, so the value-position refusal ratchet cannot drift
// from the registration table: a 21st entry fails the ratchet until it gets a
// refusal row for free.
func SyntaxCompilerNamesForTest() []string {
	q := make([]string, 0, len(syntaxCompilerEntries))
	for _, entry := range syntaxCompilerEntries {
		q = append(q, entry.Name)
	}
	return q
}
