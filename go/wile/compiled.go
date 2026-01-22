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

package wile

import (
	"wile/environment"
	"wile/machine"
)

// CompiledCode represents compiled Scheme code ready for execution.
type CompiledCode struct {
	template *machine.NativeTemplate
	env      *environment.EnvironmentFrame
}

// String returns a string representation of the compiled code.
func (c *CompiledCode) String() string {
	return "#<compiled-code>"
}
