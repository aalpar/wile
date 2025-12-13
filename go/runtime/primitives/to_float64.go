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

import "wile/values"

func ToFloat64(v values.Value) (float64, error) {
	switch n := v.(type) {
	case *values.Integer:
		return float64(n.Value), nil
	case *values.Float:
		return n.Value, nil
	case *values.Rational:
		f, _ := n.Rat().Float64()
		return f, nil
	default:
		return 0, values.WrapForeignErrorf(values.ErrNotANumber, "expected a real number but got %T", v)
	}
}
