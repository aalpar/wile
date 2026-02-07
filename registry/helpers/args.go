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

package helpers

import "github.com/aalpar/wile/values"

// ParseOptionalStartEnd extracts optional [start [end]] integer arguments from a rest list.
// defaultEnd is the value used if end is not provided.
// name is used in error messages (e.g., "bytevector-copy").
func ParseOptionalStartEnd(rest values.Value, defaultEnd int64, name string) (int64, int64, error) {
	start := int64(0)
	end := defaultEnd

	if values.IsEmptyList(rest) {
		return start, end, nil
	}

	tuple, ok := rest.(values.Tuple)
	if !ok {
		return 0, 0, values.WrapForeignErrorf(values.ErrNotAList, "%s: improper argument list", name)
	}

	startVal, ok := tuple.Car().(*values.Integer)
	if !ok {
		return 0, 0, values.WrapForeignErrorf(values.ErrNotAnInteger, "%s: start must be an integer but got %T", name, tuple.Car())
	}
	start = startVal.Value

	cdr := tuple.Cdr()
	if !values.IsEmptyList(cdr) {
		tuple2, ok := cdr.(values.Tuple)
		if !ok {
			return 0, 0, values.WrapForeignErrorf(values.ErrNotAList, "%s: improper argument list", name)
		}
		endVal, ok := tuple2.Car().(*values.Integer)
		if !ok {
			return 0, 0, values.WrapForeignErrorf(values.ErrNotAnInteger, "%s: end must be an integer but got %T", name, tuple2.Car())
		}
		end = endVal.Value
	}

	return start, end, nil
}
