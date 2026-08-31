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

package main

import "testing"

// TestDensityFloorSuppressesRatherThanDrops confirms the two ranking floors
// exist and are reported. A one-function file topping a per-function ranking
// would be the metric's most obvious failure mode.
func TestDensityFloors(t *testing.T) {
	if minFuncsPerFile < 2 {
		t.Errorf("minFuncsPerFile = %d: a floor below 2 cannot suppress a sample of one", minFuncsPerFile)
	}
	if minLOCPerPackage < 1 {
		t.Errorf("minLOCPerPackage = %d, want a positive floor", minLOCPerPackage)
	}

	small := &group{Key: "tiny", Cognitive: 20, Funcs: 1, Lines: 30}
	large := &group{Key: "big", Cognitive: 200, Funcs: 40, Lines: 2000}

	if density(small, false) <= density(large, false) {
		t.Errorf("fixture assumption broken: the one-function group should out-score the large one per function")
	}
	if density(large, true) >= density(small, true) {
		t.Errorf("fixture assumption broken: per-LOC density should also favour the small group")
	}
}
