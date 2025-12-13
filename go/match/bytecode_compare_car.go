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

package match

import (
	"fmt"

	"wile/values"
)

// ByteCodeCompareCar compares the current car position with a literal value.
type ByteCodeCompareCar struct {
	Value values.Value
}

func (p ByteCodeCompareCar) String() string {
	return fmt.Sprintf("CompareCar(%s)", p.Value.SchemeString())
}
