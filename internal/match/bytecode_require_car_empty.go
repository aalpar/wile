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

// ByteCodeRequireCarEmpty verifies that the car at the current position is an empty list.
//
// Problem: Pattern () should only match input (). Without this check,
// VisitCar + Done would match any list, because Done only checks that CDR is empty.
//
// Solution: Generate this instruction instead of VisitCar when the pattern
// element is (). It verifies the input car is also empty before proceeding.
type ByteCodeRequireCarEmpty struct{}

func (p ByteCodeRequireCarEmpty) String() string {
	return "RequireCarEmpty"
}
