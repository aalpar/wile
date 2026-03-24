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

package values_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func TestProcess(t *testing.T) {
	c := qt.New(t)

	t.Run("SchemeString includes command", func(t *testing.T) {
		p := values.NewProcess("ls", nil, nil, nil, nil)
		c.Assert(p.SchemeString(), qt.Matches, `#<process "ls".*>`)
	})

	t.Run("IsVoid is false for non-nil", func(t *testing.T) {
		p := values.NewProcess("ls", nil, nil, nil, nil)
		c.Assert(p.IsVoid(), qt.IsFalse)
	})

	t.Run("IsVoid is true for nil", func(t *testing.T) {
		var p *values.Process
		c.Assert(p.IsVoid(), qt.IsTrue)
	})

	t.Run("EqualTo is identity", func(t *testing.T) {
		p := values.NewProcess("ls", nil, nil, nil, nil)
		c.Assert(p.EqualTo(p), qt.IsTrue)

		q := values.NewProcess("ls", nil, nil, nil, nil)
		c.Assert(p.EqualTo(q), qt.IsFalse)
	})

	t.Run("EqualTo false for non-process", func(t *testing.T) {
		p := values.NewProcess("ls", nil, nil, nil, nil)
		c.Assert(p.EqualTo(values.NewString("ls")), qt.IsFalse)
	})

	t.Run("Command returns command name", func(t *testing.T) {
		p := values.NewProcess("grep", nil, nil, nil, nil)
		c.Assert(p.Command(), qt.Equals, "grep")
	})
}
