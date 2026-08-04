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

package wile

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestWithStrictNamespaceSetsFlag is a white-box unit test confirming the option
// raises engineConfig.strictLevel to strictLevelCore and that the default is off.
// Behavior driven by the level is exercised in the external strict-namespace suite.
func TestWithStrictNamespaceSetsFlag(t *testing.T) {
	c := qt.New(t)

	cfg := newEngineConfig()
	c.Assert(cfg.strictLevel, qt.Equals, strictLevelOff, qt.Commentf("default must be non-strict"))

	WithStrictNamespace()(cfg)
	c.Assert(cfg.strictLevel, qt.Equals, strictLevelCore, qt.Commentf("WithStrictNamespace must select the core level"))
}
