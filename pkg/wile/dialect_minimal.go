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

package wile

import (
	"github.com/aalpar/wile/pkg/internal/forms"
)

// R7RSMinimal is a dialect that derives from the R7RS baseline and removes the
// set! special form, so (set! …) is no longer recognized — an engine built with
// it treats set! as an ordinary (unbound) identifier. It is the second dialect
// in the roadmap, exercising the dialect abstraction with a genuine semantic
// difference from [DefaultDialect].
//
// Boundary: R7RSMinimal removes only the set! special form. The mutation
// primitives (set-car!, set-cdr!, vector-set!, string-set!, list-set!, set-box!,
// hashtable-set!) are procedures in the primitive registry, which the forms-only
// Dialect interface cannot reach, so they remain available; a full "R7RS without
// mutation" additionally needs per-primitive registry filtering (the sandboxing /
// registry-filtering track), not part of this dialect. Removing set! also affects
// any code that uses it: importing a library whose own definitions use set!
// (parts of the stdlib do) fails to compile in an R7RSMinimal engine. Do not treat
// R7RSMinimal as a no-mutation guarantee.
var R7RSMinimal Dialect = r7rsMinimalDialect{}

// r7rsMinimalDialect is the concrete R7RSMinimal — see R7RSMinimal.
type r7rsMinimalDialect struct{}

func (r7rsMinimalDialect) Name() string {
	return "r7rs-minimal"
}

func (r7rsMinimalDialect) InstallForms(fr *forms.FormRegistry) error {
	fr.Remove("set!")
	return nil
}
