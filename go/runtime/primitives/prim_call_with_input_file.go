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

package primitives

import (
	"context"
	"os"

	"wile/machine"
	"wile/values"
)

// PrimCallWithInputFile implements the call-with-input-file primitive.
func PrimCallWithInputFile(ctx context.Context, mc *machine.MachineContext) error {
	return callWithFile(ctx, mc, "call-with-input-file", os.Open,
		func(f *os.File) values.Value { return values.NewCharacterInputPortFromReader(f) })
}
