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

package values

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

// nonPointerValue is a Value with a non-pointer dynamic type — legal Go that no
// in-tree type does, standing in for an embedder that ignores the pointer-shape
// contract. identityHash must degrade to a slow bucket, not fault.
type nonPointerValue struct {
	n int
}

func (nonPointerValue) SchemeString() string {
	return "#<non-pointer>"
}

func (nonPointerValue) IsVoid() bool {
	return false
}

func (p nonPointerValue) EqualTo(o Value) bool {
	q, ok := o.(nonPointerValue)
	return ok && p.n == q.n
}

func TestIdentityHash_NonPointerFallsBackNotFails(t *testing.T) {
	qt.Assert(t, identityHash(nonPointerValue{n: 1}), qt.Equals, identityHashFallback)
	qt.Assert(t, identityHash(nonPointerValue{n: 2}), qt.Equals, identityHashFallback)
	qt.Assert(t, identityHash(NewSymbol("s")), qt.Not(qt.Equals), identityHashFallback)
}
