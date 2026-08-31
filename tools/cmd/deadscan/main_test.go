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

package main

import (
	"testing"

	ds "github.com/aalpar/wile/tools/deadscan"
)

// TestRenderConsumers covers the formatting half of the per-module split. The
// counting is the library's and is tested there; this is only how the row reads.
func TestRenderConsumers(t *testing.T) {
	const goast = "github.com/aalpar/wile-goast"
	const example = "github.com/aalpar/wile-extension-example"
	tcs := []struct {
		name string
		in   []ds.ConsumerCount
		want string
	}{
		{"none", nil, ""},
		{"one", []ds.ConsumerCount{{Module: goast, Syms: 95}}, goast + " 95"},
		{
			"two, in the order given",
			[]ds.ConsumerCount{{Module: goast, Syms: 95}, {Module: example, Syms: 37}},
			goast + " 95, " + example + " 37",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := renderConsumers(tc.in)
			if got != tc.want {
				t.Errorf("renderConsumers = %q, want %q", got, tc.want)
			}
		})
	}
}
