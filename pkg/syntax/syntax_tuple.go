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

import "github.com/aalpar/wile/pkg/values"

// SyntaxForEachFunc is the callback type for iterating over syntax tuples.
type SyntaxForEachFunc = values.SyntaxForEachFunc

// SyntaxTuple is the interface for syntax lists (pairs and vectors).
// Defined in package values so that values.emptyListType can satisfy it.
type SyntaxTuple = values.SyntaxTuple
