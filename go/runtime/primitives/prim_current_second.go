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
	"time"

	"wile/machine"
	"wile/values"
)

// PrimCurrentSecond implements the (current-second) primitive.
// Returns current time in seconds since Unix epoch.
func PrimCurrentSecond(_ context.Context, mc *machine.MachineContext) error {
	now := time.Now()
	secs := float64(now.Unix()) + float64(now.Nanosecond())/1e9
	mc.SetValue(values.NewFloat(secs))
	return nil
}
