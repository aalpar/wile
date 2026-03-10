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
	"github.com/aalpar/wile/internal/syntax"
)

// quasiKeywords holds the keyword names that distinguish quasiquote expansion
// from quasisyntax expansion. Both share the same structural logic; only the
// keyword strings (and whether dotted-pair unquote is supported) differ.
type quasiKeywords struct {
	unquote             string // "unquote" or "unsyntax"
	splicing            string // "unquote-splicing" or "unsyntax-splicing"
	nesting             string // "quasiquote" or "quasisyntax"
	quoting             string // "quote" or "syntax"
	handleDottedUnquote bool   // true for quasiquote (R7RS §4.2.8), false for quasisyntax
}

var quasiquoteKW = quasiKeywords{
	unquote:             "unquote",
	splicing:            "unquote-splicing",
	nesting:             "quasiquote",
	quoting:             "quote",
	handleDottedUnquote: true,
}

var quasisyntaxKW = quasiKeywords{
	unquote:             "unsyntax",
	splicing:            "unsyntax-splicing",
	nesting:             "quasisyntax",
	quoting:             "syntax",
	handleDottedUnquote: false,
}

// Compile-time assertions: these vars are consumed by subsequent tasks
// that unify expandQuasiquote/expandQuasisyntax into a shared dispatcher.
var (
	_ = quasiquoteKW
	_ = quasisyntaxKW
)

// buildQuasiSyntaxList creates a proper list from syntax elements.
func (p *CompileTimeContinuation) buildQuasiSyntaxList(srcCtx *syntax.SourceContext, elems ...syntax.SyntaxValue) syntax.SyntaxValue {
	var result syntax.SyntaxValue = syntax.SyntaxEmptyList
	for i := len(elems) - 1; i >= 0; i-- {
		result = syntax.NewSyntaxCons(elems[i], result, srcCtx)
	}
	return result
}

// getSymbolName returns the symbol name if the value is a symbol
func (p *CompileTimeContinuation) getSymbolName(v syntax.SyntaxValue) (string, bool) {
	s, ok := v.(*syntax.SyntaxSymbol)
	if ok {
		return s.Sym.Key, true
	}
	return "", false
}
