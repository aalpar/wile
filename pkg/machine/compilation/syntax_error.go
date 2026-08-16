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

// syntaxError decorates a (syntax-error message irritant ...) failure with the two
// facts the form named, as Scheme values.
//
// R7RS §4.3.3 defines syntax-error by delegation — "behaves similarly to error
// (6.11)" — so the operands ARE error's message and irritants, and §6.11's two
// accessors are what a program reads them back with. Joining them into the wrap
// text left error-object-irritants answering () and error-object-message answering
// the whole Go chain, when both values were sitting right here at the raise site.
//
// It adds no text. Error forwards the cause, which is the same wrap the join
// always produced, so the diagnostic an embedder reads is byte-identical and the
// format lives in one place — the raise site, not here. The declared message is
// therefore NOT the rendered one: the rendering keeps its "syntax-error: " prefix
// and its joined irritants, while the accessor answers the message alone, exactly
// as it does for (error "expected a pair" 'hello 42).
type syntaxError struct {
	message   string
	irritants []values.Value
	cause     error
}

func (p *syntaxError) Error() string {
	return p.cause.Error()
}

func (p *syntaxError) Unwrap() error {
	return p.cause
}

// ErrorMessage returns the message the form named, without the "syntax-error: "
// prefix the rendering adds. See ErrorIrritants for why both names carry the Error
// prefix.
func (p *syntaxError) ErrorMessage() string {
	return p.message
}

// ErrorIrritants returns the irritants as Scheme values. Together with
// ErrorMessage it exists so that pkg/machine can recognise this type
// structurally: this package imports pkg/machine, so the concrete type cannot be
// named from that side of the edge, and an errors.As target there has to be an
// interface.
//
// The names are deliberately not Message and Irritants: *values.NativeError
// already has both, and an interface keyed on them would match a condition buried
// in the chain and harvest its message and irritants onto the one being built —
// the same hazard SourceContext avoids for the location.
func (p *syntaxError) ErrorIrritants() []values.Value {
	return p.irritants
}
