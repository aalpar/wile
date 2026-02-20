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

// BarrierToken is an opaque identity for a with-continuation-barrier scope.
// Each call to call-with-continuation-barrier creates a fresh token. Barrier
// crossing is detected by pointer identity comparison: if the capture-time
// token differs from the invocation-time token, the continuation would cross
// a barrier boundary.
//
// nil means "not inside any barrier."
type BarrierToken struct{}

// NewBarrierToken creates a fresh barrier identity token.
func NewBarrierToken() *BarrierToken {
	return &BarrierToken{}
}
