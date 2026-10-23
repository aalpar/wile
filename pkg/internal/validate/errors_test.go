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

package validate

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/syntax"
)

func TestValidationErrorOriginChain(t *testing.T) {
	at := func(line int) *syntax.SourceContext {
		return syntax.NewSourceContext("x", "f.scm", syntax.NewSourceIndexes(0, 0, line), syntax.NewSourceIndexes(1, 1, line))
	}
	chain := &syntax.OriginInfo{Identifier: "inner", Location: at(2),
		Parent: &syntax.OriginInfo{Identifier: "mid", Location: at(3),
			Parent: &syntax.OriginInfo{Identifier: "outer", Location: at(4)}}}
	t.Run("position-less node still prints its chain", func(t *testing.T) {
		e := ValidationError{Source: &syntax.SourceContext{Origin: chain}, Message: "bad", Form: "let"}
		qt.Assert(t, e.Error(), qt.Equals,
			"bad in let form\n  expanded from 'inner' at "+at(2).Location()+
				"\n  expanded from 'mid' at "+at(3).Location()+
				"\n  expanded from 'outer' at "+at(4).Location())
	})
	t.Run("max depth truncates", func(t *testing.T) {
		e := ValidationError{Source: at(1).WithOrigin(chain), Message: "bad", Form: "let"}
		qt.Assert(t, e.ErrorWithMaxOriginDepth(1), qt.Equals,
			at(1).Location()+": bad in let form\n  expanded from 'inner' at "+at(2).Location()+"\n  ... and 2 more expansion(s)")
	})
}
