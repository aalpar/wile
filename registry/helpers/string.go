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

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// StringCompareVariadic is a helper for variadic string comparison primitives.
// It extracts strings from the variadic args and applies the comparator pairwise.
func StringCompareVariadic(mc machine.CallContext, name string, cmp func(a, b string) bool) error {
	return CompareVariadic(mc, name, werr.ErrNotAString,
		func(s *values.String) string {
			return s.Value
		}, cmp)
}
