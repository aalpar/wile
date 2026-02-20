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
	"github.com/aalpar/wile/values"
)

// OperationDrop removes the top value from the eval stack without
// affecting the value register. This is used when we need to clean
// up the stack but preserve the current result.
type OperationDrop struct {
	OperationBase
}

func NewOperationDrop() *OperationDrop {
	return &OperationDrop{
		OperationBase: NewOperationBase("machine-operation-drop"),
	}
}

func (p *OperationDrop) EqualTo(o values.Value) bool {
	v, ok := o.(*OperationDrop)
	return sameType(p, v, ok)
}
