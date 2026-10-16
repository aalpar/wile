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
	"slices"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// The operands a quasi template fires, a syntax-case input and a with-syntax
// right-hand side are ordinary expressions, so they expand HERE, in the expander
// pass, and not at compile time. Three things depend on the timing:
//
//   - validation runs after this pass, so a set! a macro template introduces is
//     a form the opaque scan sees (validate/opaque_subtree.go);
//   - a let-syntax or letrec-syntax keyword is in scope only here; the compiler's
//     environment no longer holds its frame;
//   - the operand shares this expansion's depth guard, so a macro recursing
//     through an operand raises ErrExpandDepthExceeded instead of overflowing the
//     Go stack.
//
// Each expander returns the form with only those positions replaced, so the
// compiler's own walks (expandQuasi, quasiNeedsRuntime, CompileSyntaxCase) read
// the same template they always did.

// expandQuasiquoteForm expands the operands (quasiquote template) fires.
func (p *ExpanderTimeContinuation) expandQuasiquoteForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expandQuasiTemplateForm(sym, expr, quasiquoteKW)
}

// expandQuasisyntaxForm expands the operands (quasisyntax template) fires.
func (p *ExpanderTimeContinuation) expandQuasisyntaxForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	return p.expandQuasiTemplateForm(sym, expr, quasisyntaxKW)
}

func (p *ExpanderTimeContinuation) expandQuasiTemplateForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue, kw quasiKeywords) (syntax.SyntaxValue, error) {
	// Only a proper one-operand form is rewritten: the rebuilt form drops any
	// improper tail, which would hide it from the compiler's own shape check.
	args, ok := expr.(*syntax.SyntaxPair)
	if !ok || !hasSyntaxArity(args, 1) {
		return formUnchanged(sym, expr)
	}
	template, err := p.expandQuasiOperands(args.SyntaxCar(), 1, kw.resolvedBy(p.env))
	if err != nil {
		return nil, err
	}
	if template == args.SyntaxCar() {
		return formUnchanged(sym, expr)
	}
	return syntax.NewSyntaxCons(sym, syntax.SyntaxList(sym.SourceContext(), template), sym.SourceContext()), nil
}

// expandQuasiOperands returns stx with every operand that fires at depth
// expanded and everything else as written. Which operands fire is
// expandQuasi's rule, position for position: an unquote at depth 1, a splice
// that is a list element at depth 1, and the dotted-tail cell read as a form. A
// bare splice does not fire, and a malformed marker is data. It returns stx
// itself when nothing changed, so a template without an operand keeps its
// identity for the compiler's literal path.
func (p *ExpanderTimeContinuation) expandQuasiOperands(stx syntax.SyntaxValue, depth int, kw quasiKeywords) (syntax.SyntaxValue, error) {
	exceeded := p.depthGuard.enter()
	defer p.depthGuard.leave()
	if exceeded {
		return nil, wrapSourcedError(stx.SourceContext(), werr.WrapForeignErrorf(werr.ErrExpandDepthExceeded,
			"%s: nesting depth exceeds maximum of %d", kw.nesting, p.depthGuard.max))
	}

	switch v := stx.(type) {
	case *syntax.SyntaxPair:
		name, ok := kw.headName(v.SyntaxCar())
		if ok && hasSyntaxArity(v, 2) {
			switch name {
			case kw.unquote:
				if depth == 1 {
					return p.expandMarkerOperand(v, kw)
				}
				return p.expandMarkerArg(v, depth-1, kw)
			case kw.splicing:
				if depth == 1 {
					return v, nil
				}
				return p.expandMarkerArg(v, depth-1, kw)
			case kw.nesting:
				return p.expandMarkerArg(v, depth+1, kw)
			}
		}
		if ok && (name == kw.unquote || name == kw.splicing || name == kw.nesting) {
			return v, nil
		}
		return p.expandQuasiListOperands(v, depth, kw, spineFromPair)

	case *syntax.SyntaxVector:
		if len(v.Values) == 0 {
			return v, nil
		}
		spine := buildQuasiSyntaxList(v.SourceContext(), v.Values...).(*syntax.SyntaxPair)
		walked, err := p.expandQuasiListOperands(spine, depth, kw, spineFromVector)
		if err != nil {
			return nil, err
		}
		if walked == spine {
			return v, nil
		}
		var elems []syntax.SyntaxValue
		for cell := range syntax.Spine(walked.(*syntax.SyntaxPair)) {
			elems = append(elems, cell.SyntaxCar())
		}
		return syntax.NewSyntaxVector(v.SourceContext(), elems...), nil

	default:
		return stx, nil
	}
}

// expandQuasiListOperands is expandQuasiOperands over a list template's spine:
// each element at depth, a depth-1 splice element's operand expanded, the
// dotted-tail cell (pair spines only) handed back whole as a form, and an
// improper tail walked as a template.
func (p *ExpanderTimeContinuation) expandQuasiListOperands(pair *syntax.SyntaxPair, depth int, kw quasiKeywords, spine quasiSpine) (syntax.SyntaxValue, error) {
	var cells []*syntax.SyntaxPair
	var cars []syntax.SyntaxValue
	changed := false
	var tail syntax.SyntaxValue = syntax.SyntaxEmptyList
	var end syntax.SpineEnd
	for cell, e := range syntax.Spine(pair) {
		end = e
		if spine == spineFromPair && dottedTailCell(cell, kw) {
			walked, err := p.expandQuasiOperands(cell, depth, kw)
			if err != nil {
				return nil, err
			}
			changed = changed || walked != syntax.SyntaxValue(cell)
			tail = walked
			end = syntax.SpineEnd{}
			break
		}
		walked, err := p.expandQuasiElement(cell.SyntaxCar(), depth, kw)
		if err != nil {
			return nil, err
		}
		changed = changed || walked != cell.SyntaxCar()
		cells = append(cells, cell)
		cars = append(cars, walked)
	}
	if end.Improper() {
		walked, err := p.expandQuasiOperands(end.Tail, depth, kw)
		if err != nil {
			return nil, err
		}
		changed = changed || walked != end.Tail
		tail = walked
	}
	if !changed {
		return pair, nil
	}
	q := tail
	for i, car := range slices.Backward(cars) {
		q = syntax.NewSyntaxCons(car, q, cells[i].SourceContext())
	}
	return q, nil
}

// expandQuasiElement walks one list element: a depth-1 splice fires its
// operand, and anything else is a template at depth.
func (p *ExpanderTimeContinuation) expandQuasiElement(car syntax.SyntaxValue, depth int, kw quasiKeywords) (syntax.SyntaxValue, error) {
	carPair, ok := car.(*syntax.SyntaxPair)
	if ok && depth == 1 && hasSyntaxArity(carPair, 2) {
		name, ok := kw.headName(carPair.SyntaxCar())
		if ok && name == kw.splicing {
			return p.expandMarkerOperand(carPair, kw)
		}
	}
	return p.expandQuasiOperands(car, depth, kw)
}

// expandMarkerOperand rebuilds (marker operand) with the operand expanded as an
// expression.
func (p *ExpanderTimeContinuation) expandMarkerOperand(v *syntax.SyntaxPair, kw quasiKeywords) (syntax.SyntaxValue, error) {
	arg := v.SyntaxCdr().(*syntax.SyntaxPair)
	expanded, err := p.expandSub(v, arg.SyntaxCar(), kw.nesting+": failed to expand operand")
	if err != nil {
		return nil, err
	}
	return rebuildMarker(v, arg, expanded), nil
}

// expandMarkerArg rebuilds (marker arg) with arg walked as a template at depth.
func (p *ExpanderTimeContinuation) expandMarkerArg(v *syntax.SyntaxPair, depth int, kw quasiKeywords) (syntax.SyntaxValue, error) {
	arg := v.SyntaxCdr().(*syntax.SyntaxPair)
	walked, err := p.expandQuasiOperands(arg.SyntaxCar(), depth, kw)
	if err != nil {
		return nil, err
	}
	if walked == arg.SyntaxCar() {
		return v, nil
	}
	return rebuildMarker(v, arg, walked), nil
}

// rebuildMarker returns (marker x) with x replaced, keeping both cells' source
// contexts.
func rebuildMarker(v, arg *syntax.SyntaxPair, x syntax.SyntaxValue) syntax.SyntaxValue {
	return syntax.NewSyntaxCons(v.SyntaxCar(), syntax.NewSyntaxCons(x, arg.SyntaxCdr(), arg.SourceContext()), v.SourceContext())
}

// expandSyntaxCaseForm expands (syntax-case input (literal ...) clause ...)'s
// input. The clauses are templates and patterns; CompileSyntaxCase expands each
// clause body itself, once the pattern variables are bound.
func (p *ExpanderTimeContinuation) expandSyntaxCaseForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := asNonEmptyPair(expr)
	if !ok {
		return formUnchanged(sym, expr)
	}
	input, err := p.expandSub(expr, pair.SyntaxCar(), "syntax-case: failed to expand input expression")
	if err != nil {
		return nil, err
	}
	return syntax.NewSyntaxCons(sym, syntax.NewSyntaxCons(input, pair.SyntaxCdr(), pair.SourceContext()), sym.SourceContext()), nil
}

// expandWithSyntaxForm expands each right-hand side of (with-syntax ((pattern
// expr) ...) body ...). A malformed binding list is left for CompileWithSyntax
// to report.
func (p *ExpanderTimeContinuation) expandWithSyntaxForm(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue) (syntax.SyntaxValue, error) {
	pair, ok := asNonEmptyPair(expr)
	if !ok {
		return formUnchanged(sym, expr)
	}
	bindings, ok := pair.SyntaxCar().(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(bindings) {
		return formUnchanged(sym, expr)
	}
	var cells []*syntax.SyntaxPair
	var rebuilt []syntax.SyntaxValue
	for cell, end := range syntax.Spine(bindings) {
		if end.Improper() {
			return formUnchanged(sym, expr)
		}
		binding, ok := cell.SyntaxCar().(*syntax.SyntaxPair)
		if !ok || !hasSyntaxArity(binding, 2) {
			return formUnchanged(sym, expr)
		}
		rhs := binding.SyntaxCdr().(*syntax.SyntaxPair)
		expanded, err := p.expandSub(expr, rhs.SyntaxCar(), "with-syntax: failed to expand binding expression")
		if err != nil {
			return nil, err
		}
		cells = append(cells, cell)
		rebuilt = append(rebuilt, rebuildMarker(binding, rhs, expanded))
	}
	// Each spine cell keeps its own source position, as expandQuasiListOperands does.
	var list syntax.SyntaxValue = syntax.SyntaxEmptyList
	for i, b := range slices.Backward(rebuilt) {
		list = syntax.NewSyntaxCons(b, list, cells[i].SourceContext())
	}
	out := syntax.NewSyntaxCons(list, pair.SyntaxCdr(), pair.SourceContext())
	return syntax.NewSyntaxCons(sym, out, sym.SourceContext()), nil
}
