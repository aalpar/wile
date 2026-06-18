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
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// PrimBox implements the box primitive.
// Creates a new box containing the given value.
func PrimBox(mc machine.CallContext) error {
	mc.SetValue(values.NewBox(mc.Arg(0)))
	return nil
}

// PrimBoxQ implements the box? predicate.
// Returns #t if the argument is a box, #f otherwise.
var PrimBoxQ = helpers.MakeTypePredicate(func(o values.Value) bool {
	_, ok := o.(*values.Box)
	return ok
})

// PrimUnbox implements the unbox primitive.
// Returns the value contained in a box.
var PrimUnbox = helpers.MakeUnaryAccessor(werr.ErrNotABox, "unbox", func(b *values.Box) values.Value {
	return b.Unbox()
})

// PrimSetBox implements the set-box! primitive.
// Sets the value contained in a box.
var PrimSetBox = helpers.MakeBinarySetter(werr.ErrNotABox, "set-box!", func(b *values.Box, val values.Value) {
	b.Value = val
})
