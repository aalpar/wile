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

package match

import (
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// testSyntaxInt creates a syntax-wrapped integer for test bytecode.
func testSyntaxInt(v int64) syntax.SyntaxValue {
	return syntax.NewSyntaxObject(values.NewInteger(v), nil)
}

// testSyntaxSym creates a syntax-wrapped symbol for test bytecode.
func testSyntaxSym(key string) syntax.SyntaxValue {
	return syntax.NewSyntaxSymbol(key, nil)
}
