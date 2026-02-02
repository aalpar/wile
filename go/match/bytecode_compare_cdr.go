// Copyright 2025 Aaron Alpar
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

package match

import (
	"fmt"

	"github.com/aalpar/wile/go/syntax"
)

// ByteCodeCompareCdr compares the cdr of the current pair with a literal syntax value.
// This is used for improper list patterns where the tail is a literal,
// e.g., (a . b) where b is a literal symbol to match exactly.
type ByteCodeCompareCdr struct {
	Value syntax.SyntaxValue
}

func (p ByteCodeCompareCdr) String() string {
	return fmt.Sprintf("CompareCdr(%v)", p.Value)
}
