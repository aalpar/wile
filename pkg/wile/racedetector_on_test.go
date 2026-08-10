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

//go:build race

package wile_test

// raceDetectorEnabled reports whether this test binary was built with -race.
// The shared-aggregate pins need it because they come in two arms that are
// mutually exclusive by construction: one asserts the detector fires, the
// other asserts the process survives the same unsynchronised shape.
const raceDetectorEnabled = true
