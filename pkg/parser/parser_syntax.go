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

package parser

import (
	"strings"

	"github.com/aalpar/wile/pkg/internal/tokenizer"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// newSourceContext creates a SourceContext from a token.
func (p *Parser) newSourceContext(t tokenizer.Token) *syntax.SourceContext {
	return syntax.NewSourceContext(t.String(), p.file, t.Start(), t.End())
}

func (p *Parser) wrapSyntax(o values.Value, t tokenizer.Token) *syntax.SyntaxObject {
	return syntax.NewSyntaxObject(o, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxSymbol(o string, t tokenizer.Token) *syntax.SyntaxSymbol {
	// R7RS §2.1: Apply fold-case if directive is active
	if p.foldCase {
		o = strings.ToLower(o)
	}
	sym := values.NewSymbol(o)
	return syntax.NewSyntaxSymbolForSymbol(sym, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxVector(os []values.Value, t tokenizer.Token) *syntax.SyntaxVector {
	sc := p.newSourceContext(t)
	svs := make([]syntax.SyntaxValue, len(os))
	for i, v := range os {
		svs[i] = p.wrapSyntax(v, t)
	}
	return syntax.NewSyntaxVector(sc, svs...)
}

func (p *Parser) wrapSyntaxEmptyList(_ tokenizer.Token) syntax.SyntaxTuple {
	return syntax.SyntaxEmptyList
}

func (p *Parser) wrapSyntaxPair(v0, v1 syntax.SyntaxValue, t tokenizer.Token) *syntax.SyntaxPair {
	return syntax.NewSyntaxCons(v0, v1, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxComment(v0 string, t tokenizer.Token) *syntax.SyntaxComment {
	return syntax.NewSyntaxComment(v0, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxDatumComment(v0 string, v1 syntax.SyntaxValue, t tokenizer.Token) *syntax.SyntaxDatumComment {
	return syntax.NewSyntaxDatumComment(v0, v1, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxBox(v0 syntax.SyntaxValue, t tokenizer.Token) *syntax.SyntaxBox {
	return syntax.NewSyntaxBox(v0, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxDatumLabelAssignment(v0 int, v1 syntax.SyntaxValue, t tokenizer.Token) *syntax.SyntaxDatumLabelAssignment {
	return syntax.NewSyntaxDatumLabelAssignment(v0, v1, p.newSourceContext(t))
}

func (p *Parser) wrapSyntaxDirective(v0 string, t tokenizer.Token) *syntax.SyntaxDirective {
	return syntax.NewSyntaxDirective(v0, p.newSourceContext(t))
}

// processFoldCaseDirective handles R7RS §2.1 fold-case directives.
// #!fold-case causes subsequent identifiers to be case-folded to lowercase.
// #!no-fold-case restores case-sensitive reading.
func (p *Parser) processFoldCaseDirective(d *syntax.SyntaxDirective) {
	if strings.EqualFold(d.Name, "fold-case") {
		p.foldCase = true
	} else if strings.EqualFold(d.Name, "no-fold-case") {
		p.foldCase = false
	}
}

func (p *Parser) listSyntax(t tokenizer.Token, os ...syntax.SyntaxValue) syntax.SyntaxValue {
	l := len(os)
	switch l {
	case 0:
		return p.wrapSyntaxEmptyList(t)
	case 1:
		return p.wrapSyntaxPair(os[0], p.wrapSyntaxEmptyList(t), t)
	}
	q := p.wrapSyntaxPair(os[0], p.wrapSyntaxPair(nil, nil, t), t)
	curr := q
	for _, v := range os[1:] {
		curr = curr.Cdr().(*syntax.SyntaxPair)
		curr.SetCar(v)
		curr.SetCdr(p.wrapSyntaxPair(nil, nil, t))
	}
	curr.SetCdr(p.wrapSyntaxEmptyList(t))
	return q
}

// rewrapSyntax wraps a new value with the same syntax context as the original.
func (p *Parser) rewrapSyntax(orig syntax.SyntaxValue, newVal values.Value) syntax.SyntaxValue {
	// Get the source context from the original syntax value
	var sctx *syntax.SourceContext
	so, ok := orig.(*syntax.SyntaxObject)
	if ok {
		sctx = so.SourceContext()
	}
	return syntax.NewSyntaxObject(newVal, sctx)
}
