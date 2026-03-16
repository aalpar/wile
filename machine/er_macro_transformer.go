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

package machine

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
)

var _ values.Value = (*ERMacroTransformer)(nil)

// ERMacroTransformer wraps a 3-arg MachineClosure to identify it as an
// explicit-renaming transformer in expandMacroInvocation. The defEnv
// captures the expand-time environment at the macro definition site,
// used by the rename closure to resolve definition-site bindings.
type ERMacroTransformer struct {
	closure *MachineClosure
	defEnv  *environment.EnvironmentFrame
}

func NewERMacroTransformer(closure *MachineClosure, defEnv *environment.EnvironmentFrame) *ERMacroTransformer {
	return &ERMacroTransformer{
		closure: closure,
		defEnv:  defEnv,
	}
}

func (p *ERMacroTransformer) Closure() *MachineClosure {
	return p.closure
}

func (p *ERMacroTransformer) DefEnv() *environment.EnvironmentFrame {
	return p.defEnv
}

func (p *ERMacroTransformer) IsVoid() bool {
	return p == nil
}

func (p *ERMacroTransformer) SchemeString() string {
	return "#<er-macro-transformer>"
}

func (p *ERMacroTransformer) EqualTo(o values.Value) bool {
	v, ok := o.(*ERMacroTransformer)
	if !ok {
		return false
	}
	return p == v
}
