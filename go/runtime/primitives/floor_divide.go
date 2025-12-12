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

func FloorDivide(n0, n1 int64) (q, r int64) {
	q = n0 / n1
	r = n0 % n1
	// Floor division: quotient rounds toward negative infinity
	// If remainder is non-zero and signs differ, adjust
	if r != 0 && (n0 < 0) != (n1 < 0) {
		q--
		r += n1
	}
	return q, r
}

func GcdInt(a, b int64) int64 {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}
