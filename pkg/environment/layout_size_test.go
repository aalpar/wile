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

package environment

import (
	"testing"
	"unsafe"
)

// TestEnvironmentFrameLayout records the in-memory size of the hot VM structs.
// It is a baseline guard for memory/2026-06-02-environment-frame-hot-cold-layout.md:
// the EnvironmentFrame is allocated/reset on every closure application, so its
// size directly drives apply-path allocation cost. When the hot/cold layout
// change lands, these expected sizes will flip and this test must be updated to
// the new (smaller) numbers — the diff makes the win visible.
func TestEnvironmentFrameLayout(t *testing.T) {
	sizes := []struct {
		name string
		got  uintptr
	}{
		{"EnvironmentFrame", unsafe.Sizeof(EnvironmentFrame{})},
		{"LocalEnvironmentFrame", unsafe.Sizeof(LocalEnvironmentFrame{})},
		{"GlobalEnvironmentFrame", unsafe.Sizeof(GlobalEnvironmentFrame{})},
		{"Binding", unsafe.Sizeof(Binding{})},
	}
	for _, s := range sizes {
		t.Logf("sizeof(%s) = %d bytes", s.name, s.got)
	}
	// Hard-assert only the frame whose size the layout change targets, so the
	// change is forced to update this baseline deliberately. GlobalEnvironmentFrame
	// and Binding carry want=0 (log-only) because they are not the subject here.
	ef := unsafe.Sizeof(EnvironmentFrame{})
	if ef != 80 {
		t.Errorf("sizeof(EnvironmentFrame) = %d, baseline expected 80; "+
			"if this is the intended slimming change, update the baseline and the plan", ef)
	}
}
