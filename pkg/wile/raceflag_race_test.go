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

// raceEnabled reports whether the test binary was built with the race detector
// (-race). The detector inflates per-Go-frame stack cost several-fold, so tests
// that legitimately nest deep on the Go stack (e.g. ctak's ~40k live
// continuation re-invocation frames) can exceed Go's 1 GB goroutine-stack limit
// under -race even though they fit comfortably without it. Such single-threaded
// tests gate on this flag to skip under -race, where they add no race-detection
// value anyway. Mirrors pkg/registry/core's raceflag pair.
const raceEnabled = true
