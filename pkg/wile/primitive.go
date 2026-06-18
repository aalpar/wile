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
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry"
)

// PrimitiveSpec defines a primitive to be registered.
// This is a re-export of registry.PrimitiveSpec for convenience.
type PrimitiveSpec = registry.PrimitiveSpec

// CallContext is the extension-facing subset of MachineContext.
// This is a re-export of machine.CallContext for convenience.
type CallContext = machine.CallContext

// ForeignFunction is the signature for primitive implementations.
// This is a re-export of machine.ForeignFunction for convenience.
type ForeignFunction = machine.ForeignFunction

// MachineContext provides access to the VM during primitive execution.
// This is a re-export of machine.MachineContext for convenience.
type MachineContext = machine.MachineContext
