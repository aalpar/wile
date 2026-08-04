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

package core

import (
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// hashToScheme projects a Go hash onto the exact non-negative integer R6RS
// requires of every hash procedure ("returns an exact non-negative integer").
//
// The >> 1 is load-bearing: int64(uint64) of a hash with the high bit set is
// NEGATIVE, and a negative hash reaching Scheme would break any portable
// (modulo (equal-hash k) n) bucketing. Shifting discards the low bit and keeps
// 63 bits, which is more distribution than any bucket count consumes.
func hashToScheme(h uint64) values.Value {
	return values.NewInteger(int64(h >> 1))
}

// PrimEqualHash implements R6RS equal-hash. Accepts any object, including one
// containing a cycle: values.EqualHash is total.
var PrimEqualHash = helpers.MakeUnaryAccessor(werr.ErrInvalidArgument, "equal-hash",
	func(v values.Value) values.Value {
		return hashToScheme(values.EqualHash(v))
	})

// PrimStringHash implements R6RS string-hash: unary, unbounded.
//
// string-ci-hash is NOT here. It must agree with string-ci=?, which folds case
// with x/text's full Unicode fold, so it is registered beside that procedure in
// pkg/internal/extensions/all and shares the same fold function.
var PrimStringHash = helpers.MakeUnaryAccessor(werr.ErrNotAString, "string-hash",
	func(s *values.String) values.Value {
		return hashToScheme(values.StringHash(s.Value))
	})

// PrimSymbolHash implements R6RS symbol-hash.
var PrimSymbolHash = helpers.MakeUnaryAccessor(werr.ErrNotASymbol, "symbol-hash",
	func(sym *values.Symbol) values.Value {
		return hashToScheme(values.SymbolHash(sym))
	})
