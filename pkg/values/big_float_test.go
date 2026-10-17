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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestDecimalLiteralPrecision(t *testing.T) {
	tcs := []struct {
		in   string
		want uint
	}{
		{"1.5", DefaultBigFloatPrecision},
		{"1e400", DefaultBigFloatPrecision},
		{"0." + strings.Repeat("0", 200) + "1", DefaultBigFloatPrecision},
		{strings.Repeat("7", 100), 333},
		{"-1." + strings.Repeat("7", 99) + "e5", 333},
	}
	for _, tc := range tcs {
		c := qt.New(t)
		c.Assert(decimalLiteralPrecision(tc.in), qt.Equals, tc.want, qt.Commentf("%.20s", tc.in))
	}
}
