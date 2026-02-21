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

// Operation is the base interface for all bytecode operations. Every operation
// is also a values.Value so it can appear in the literals pool and be printed.
// Operations that are inlined into the Run() switch loop only need this base
// interface. Operations dispatched through the side table (OpComplex) must
// additionally implement InlinedOperation.
type Operation interface {
	values.Value
}

// InlinedOperation is the interface for operations dispatched through the
// side table via OpComplex. These operations carry their own Apply method
// because the Run() loop delegates to them rather than inlining the logic.
type InlinedOperation interface {
	Operation
	Apply(mc *MachineContext) (*MachineContext, error)
}
