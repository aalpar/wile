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

package match

// ByteCodeVisitCar navigates into the car of the current pair.
type ByteCodeVisitCar struct{}

func (ByteCodeVisitCar) String() string {
	return "VisitCar"
}

// ByteCodeVisitCdr navigates to the cdr of the current pair.
type ByteCodeVisitCdr struct{}

func (ByteCodeVisitCdr) String() string {
	return "VisitCdr"
}

// ByteCodeDone signals completion of the current subtree.
type ByteCodeDone struct{}

func (ByteCodeDone) String() string {
	return "Done"
}

// ByteCodeVisitCarAsVector checks that the car of the current pair is a
// SyntaxVector, converts its elements to a SyntaxPair chain, and pushes
// the chain onto the syntax stack. This enables pair-based matching of
// vector pattern contents per R7RS §4.3.2.
type ByteCodeVisitCarAsVector struct{}

func (ByteCodeVisitCarAsVector) String() string {
	return "VisitCarAsVector"
}

// ByteCodeRequireCarEmptyVector verifies that the car at the current position is an
// empty SyntaxVector. Used for empty vector patterns #().
type ByteCodeRequireCarEmptyVector struct{}

func (ByteCodeRequireCarEmptyVector) String() string {
	return "RequireCarEmptyVector"
}
