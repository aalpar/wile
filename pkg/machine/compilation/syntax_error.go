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
	"github.com/aalpar/wile/pkg/values"
)

// syntaxError decorates a (syntax-error message irritant ...) failure with its
// irritants as Scheme values.
//
// R7RS §4.3.1 names the irritants, and error-object-irritants is a registered
// primitive with a waiting consumer, so they have to travel as values: joining
// them into the message left error-object-irritants answering () while the
// irritants themselves existed only inside a string.
//
// It adds no text. Error forwards the cause, which is the same wrap the join
// always produced, so the diagnostic an embedder reads is byte-identical and the
// format lives in one place — the raise site, not here.
type syntaxError struct {
	irritants []values.Value
	cause     error
}

func (p *syntaxError) Error() string {
	return p.cause.Error()
}

func (p *syntaxError) Unwrap() error {
	return p.cause
}

// ErrorIrritants returns the irritants as Scheme values. It exists so that
// pkg/machine can recognise this type structurally: this package imports
// pkg/machine, so the concrete type cannot be named from that side of the edge,
// and an errors.As target there has to be an interface.
//
// The name is deliberately not Irritants: *values.NativeError already has that
// method, and an interface keyed on it would match a condition buried in the
// chain and harvest its irritants onto the one being built — the same hazard
// SourceContext avoids for the location.
func (p *syntaxError) ErrorIrritants() []values.Value {
	return p.irritants
}
