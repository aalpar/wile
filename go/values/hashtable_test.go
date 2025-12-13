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

package values

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestHashtable_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 Value
		in1 Value
		out bool
	}{
		{
			in0: NewHashtable(map[string]Value{
				"key1": NewInteger(1),
			}),
			in1: NewHashtable(map[string]Value{
				"key2": NewInteger(1),
			}),
			out: false,
		},
		{
			in0: NewHashtable(map[string]Value{
				"key1": NewInteger(1),
			}),
			in1: NewHashtable(map[string]Value{
				"key1": NewInteger(1),
			}),
			out: true,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}
