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

package syntax

import "github.com/aalpar/wile/values"

// SourceIndexes is the source-position type. It is defined in package
// values so that values.emptyListType can implement values.SyntaxValue
// directly (the empty-list duality merge — see values/syntax_value.go).
type SourceIndexes = values.SourceIndexes

// NewSourceIndexes constructs a SourceIndexes at the given position.
func NewSourceIndexes(index, column, line int) SourceIndexes {
	return values.NewSourceIndexes(index, column, line)
}
