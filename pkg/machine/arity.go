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

package machine

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// checkArity validates that argCount satisfies the arity requirements.
// For non-variadic: argCount must equal paramCount.
// For variadic: argCount must be >= paramCount-1.
func checkArity(paramCount int, isVariadic bool, argCount int) error {
	if !isVariadic {
		if argCount != paramCount {
			return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
				"expected %d arguments, got %d", paramCount, argCount)
		}
		return nil
	}
	if argCount < paramCount-1 {
		return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
			"expected at least %d arguments, got %d", paramCount-1, argCount)
	}
	return nil
}

// bindArgs binds arguments to environment bindings.
// For variadic closures, the rest args (from index paramCount-1 onward)
// are collected using the provided restArgFn. If restArgFn is nil,
// values.List is used as the default.
func bindArgs(
	bnds []environment.Binding,
	vs []values.Value,
	paramCount int,
	isVariadic bool,
	restArgFn func([]values.Value, int) values.Tuple,
) {
	if !isVariadic {
		for i := range bnds[:paramCount] {
			bnds[i].SetValue(vs[i])
		}
		return
	}
	for i := range bnds[:paramCount-1] {
		bnds[i].SetValue(vs[i])
	}
	if restArgFn != nil {
		bnds[paramCount-1].SetValue(restArgFn(vs, paramCount-1))
	} else {
		bnds[paramCount-1].SetValue(values.List(vs[paramCount-1:]...))
	}
}
