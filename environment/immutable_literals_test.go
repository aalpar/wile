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

	"github.com/aalpar/wile/values"
)

func TestImmutableLiterals_MarkContains(t *testing.T) {
	set := &ImmutableLiterals{}
	p := values.NewCons(values.NewInteger(1), values.EmptyList)
	q := values.NewCons(values.NewInteger(1), values.EmptyList)

	if set.Contains(p) {
		t.Fatalf("unmarked pair must not be immutable")
	}
	set.Mark(p)
	if !set.Contains(p) {
		t.Fatalf("marked pair must be immutable")
	}
	// Membership is by pointer identity, not equal? — a distinct equal pair is mutable.
	if set.Contains(q) {
		t.Fatalf("distinct equal pair must not be marked (identity, not equal?)")
	}
}
