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

package values_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

func TestByte_SchemeString(t *testing.T) {
	tcs := []struct {
		in  values.Value
		out string
	}{
		{
			in:  values.NewByte(10),
			out: "10",
		},
		{
			in:  values.NewByte(20),
			out: "20",
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in.SchemeString(), qt.Equals, tc.out)
		})
	}
}

func TestByte_EqualTo(t *testing.T) {
	tcs := []struct {
		in0 values.Value
		in1 values.Value
		out bool
	}{
		{
			in0: values.NewByte(1),
			in1: values.NewByte(1),
			out: true,
		},
		{
			in0: values.NewByte(1),
			in1: values.NewByte(0),
			out: false,
		},
	}
	for _, tc := range tcs {
		t.Run("", func(t *testing.T) {
			qt.Assert(t, tc.in0.EqualTo(tc.in1), qt.Equals, tc.out)
		})
	}
}
