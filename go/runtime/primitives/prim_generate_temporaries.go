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

package primitives

import (
	"context"
	"fmt"
	"sync/atomic"

	"wile/machine"
	"wile/syntax"
	"wile/values"
)

// gensymCounter is used to generate unique symbol names
var gensymCounter uint64

// PrimGenerateTemporaries implements the generate-temporaries procedure (R6RS).
// Takes a list (or syntax list) and returns a list of fresh identifiers
// with the same length. Each identifier is guaranteed to be unique.
//
// (generate-temporaries stx-list) -> list of identifiers
func PrimGenerateTemporaries(_ context.Context, mc *machine.MachineContext) error {
	arg := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()

	// Count the length of the list
	count, err := listLength(arg)
	if err != nil {
		return values.WrapForeignErrorf(err, "generate-temporaries")
	}

	// Generate fresh identifiers
	result := values.EmptyList
	for i := count - 1; i >= 0; i-- {
		id := atomic.AddUint64(&gensymCounter, 1)
		name := fmt.Sprintf("g%d", id)
		sym := syntax.NewSyntaxSymbol(name, nil)
		result = values.NewCons(sym, result)
	}

	mc.SetValue(result)
	return nil
}

// listLength returns the length of a list, handling both regular and syntax lists.
func listLength(v values.Value) (int, error) {
	count := 0
	current := v

	for {
		switch p := current.(type) {
		case *values.Pair:
			if p.IsEmptyList() {
				return count, nil
			}
			count++
			current = p.Cdr()

		case *syntax.SyntaxPair:
			if p.IsEmptyList() {
				return count, nil
			}
			count++
			current = p.Cdr()

		default:
			return 0, values.NewForeignError("expected a proper list")
		}
	}
}
