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

package integration_test

import (
	"testing"
	"time"
)

func TestERMacro_Basic(t *testing.T) {
	runSchemeTest(t, "er_macro_basic.scm", 15*time.Second, "ER macro basic")
}

func TestERMacro_Hygiene(t *testing.T) {
	runSchemeTest(t, "er_macro_hygiene.scm", 15*time.Second, "ER macro hygiene")
}

func TestERMacro_Compare(t *testing.T) {
	runSchemeTest(t, "er_macro_compare.scm", 15*time.Second, "ER macro compare")
}
