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

package compilation

import (
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

var _ values.Value = (*ERMacroTransformer)(nil)

// ERMacroTransformer wraps a 3-arg machine.MachineClosure to identify it as an
// explicit-renaming transformer in expandMacroInvocation. defEnv captures the
// frame the transformer's right-hand side compiled in, which identifies the
// definition-site STORE (a library's own environment, or the namespace's).
//
// Its PHASE is not the one a rename reads at: the right-hand side compiles one
// rung up the tower, and the output lands one rung down. invokeERTransformer
// re-phases it to the expansion under way before handing it to the rename
// closure.
//
// defScopes is the scope set of the er-macro-transformer keyword as written,
// the Go spelling of the Scheme shim's #'k: it is the definition site's lexical
// context, so a rename carries it and reaches what the name means there,
// including a body-local macro, which the store keys by the body's scopes and
// which no frame lookup by name can tell from another body's.
type ERMacroTransformer struct {
	closure   *machine.MachineClosure
	defEnv    *environment.EnvironmentFrame
	defScopes syntax.Scopes
}

func NewERMacroTransformer(closure *machine.MachineClosure, defEnv *environment.EnvironmentFrame, defScopes syntax.Scopes) *ERMacroTransformer {
	return &ERMacroTransformer{
		closure:   closure,
		defEnv:    defEnv,
		defScopes: defScopes,
	}
}

func (p *ERMacroTransformer) Closure() *machine.MachineClosure {
	return p.closure
}

func (p *ERMacroTransformer) DefEnv() *environment.EnvironmentFrame {
	return p.defEnv
}

// DefScopes is the er-macro-transformer keyword's scope set (see the type doc).
func (p *ERMacroTransformer) DefScopes() syntax.Scopes {
	return p.defScopes
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
