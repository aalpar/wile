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

// PrimOpaqueQ implements the opaque? predicate.
// Returns #t if the argument is an OpaqueValue.
var PrimOpaqueQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.OpaqueValue)
	return ok
})

// PrimOpaqueTag implements the opaque-tag primitive.
// Returns the tag of an opaque value as a symbol.
var PrimOpaqueTag = helpers.MakeUnaryAccessor(werr.ErrNotAnOpaqueValue, "opaque-tag", func(o *values.OpaqueValue) values.Value {
	return values.NewSymbol(o.OpaqueTag())
})
