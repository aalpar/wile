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

package core

import (
	"fmt"
	"slices"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// The one-level syntax accessors (design §2.4). All Unwrap-based, never
// UnwrapAll: elements stay syntax, which is what lets the Scheme syntax-case
// walk a form without losing hygiene.

// requireSyntaxPair returns Arg(0) as a non-empty syntax pair.
func requireSyntaxPair(mc machine.CallContext, name string) (*syntax.SyntaxPair, error) {
	sp, ok := mc.Arg(0).(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(sp) {
		return nil, werr.WrapForeignErrorf(werr.ErrNotASyntaxPair, "%s: expected a syntax pair, got %s", name, mc.Arg(0).SchemeString())
	}
	return sp, nil
}

// PrimSyntaxPairQ implements (syntax-pair? obj): a non-empty syntax pair.
func PrimSyntaxPairQ(mc machine.CallContext) error {
	sp, ok := mc.Arg(0).(*syntax.SyntaxPair)
	mc.SetValue(values.BoolToBoolean(ok && !syntax.IsSyntaxEmptyList(sp)))
	return nil
}

// PrimSyntaxNullQ implements (syntax-null? obj): the syntax empty list, or the
// plain empty list a syntax-cdr can answer for an improper spine's end.
func PrimSyntaxNullQ(mc machine.CallContext) error {
	v := mc.Arg(0)
	sv, ok := v.(syntax.SyntaxValue)
	if ok {
		mc.SetValue(values.BoolToBoolean(syntax.IsSyntaxEmptyList(sv)))
		return nil
	}
	mc.SetValue(values.BoolToBoolean(values.IsEmptyList(v)))
	return nil
}

// PrimSyntaxCar implements (syntax-car stx).
func PrimSyntaxCar(mc machine.CallContext) error {
	sp, err := requireSyntaxPair(mc, "syntax-car")
	if err != nil {
		return err
	}
	mc.SetValue(sp.SyntaxCar())
	return nil
}

// PrimSyntaxCdr implements (syntax-cdr stx).
func PrimSyntaxCdr(mc machine.CallContext) error {
	sp, err := requireSyntaxPair(mc, "syntax-cdr")
	if err != nil {
		return err
	}
	mc.SetValue(sp.SyntaxCdr())
	return nil
}

// PrimSyntaxVectorQ implements (syntax-vector? obj).
func PrimSyntaxVectorQ(mc machine.CallContext) error {
	_, ok := mc.Arg(0).(*syntax.SyntaxVector)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}

// PrimSyntaxVectorToList implements (syntax-vector->list stx): the elements as
// a SYNTAX list whose spine carries the vector's source context, so a vector
// pattern reduces to the list matcher.
func PrimSyntaxVectorToList(mc machine.CallContext) error {
	sv, ok := mc.Arg(0).(*syntax.SyntaxVector)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotASyntaxObject, "syntax-vector->list: expected a syntax vector, got %s", mc.Arg(0).SchemeString())
	}
	var q syntax.SyntaxValue = syntax.SyntaxEmptyList
	for _, e := range slices.Backward(sv.Values) {
		q = syntax.NewSyntaxCons(e, q, sv.SourceContext())
	}
	mc.SetValue(q)
	return nil
}

// PrimSyntaxSpine implements (%syntax-spine stx): pairs and vectors unwrapped to
// Scheme pairs and vectors all the way down, identifier leaves kept as syntax,
// other atoms unwrapped (design §3.5, Q6). The ER shim hands its proc this
// shape, so (identifier? x) is the test and compare the equality; (symbol? x) on
// a form element is #f. compare is the re-homed free-identifier=?, which requires
// both arguments to be identifiers, so (compare 'sym x) raises where Chibi's
// identifier=? would accept the quoted symbol.
func PrimSyntaxSpine(mc machine.CallContext) error {
	mc.SetValue(syntaxSpine(mc.Arg(0)))
	return nil
}

func syntaxSpine(v values.Value) values.Value {
	switch s := v.(type) {
	case *syntax.SyntaxSymbol:
		return s
	case *syntax.SyntaxPair:
		return syntaxSpineList(s)
	case *syntax.SyntaxVector:
		elems := make([]values.Value, len(s.Values))
		for i, e := range s.Values {
			elems[i] = syntaxSpine(e)
		}
		return values.NewVector(elems...)
	case syntax.SyntaxValue:
		return s.Unwrap()
	default:
		return v
	}
}

// syntaxSpineList walks the cdr chain iteratively: list length must not become
// Go stack depth (the datum->syntax lesson in prim_syntax.go). Nesting still
// recurses, bounded by the reader's and datum->syntax's depth limits.
func syntaxSpineList(sp *syntax.SyntaxPair) values.Value {
	var head, tail *values.Pair
	var cur syntax.SyntaxValue = sp
	for {
		p, ok := cur.(*syntax.SyntaxPair)
		if !ok || syntax.IsSyntaxEmptyList(p) {
			break
		}
		cell := values.NewCons(syntaxSpine(p.SyntaxCar()), values.EmptyList)
		if head == nil {
			head = cell
		} else {
			tail.SetCdr(cell)
		}
		tail = cell
		cur = p.SyntaxCdr()
	}
	if head == nil {
		return values.EmptyList
	}
	if !syntax.IsSyntaxEmptyList(cur) {
		tail.SetCdr(syntaxSpine(cur))
	}
	return head
}

// syntaxViolation carries the offending form's source as a real chain member.
// pkg/machine reads it structurally (foreign_closure.go's sourcedError) when it
// renders an uncaught error, and errors.As reaches it from Go. registry/core
// cannot use compilation.SourcedError (the two are peers), so this is the same
// shape declared here.
type syntaxViolation struct {
	src   *syntax.SourceContext
	cause error
}

func (p *syntaxViolation) Error() string {
	if p.src != nil {
		loc := p.src.Location()
		if loc != "" {
			return loc + ": " + p.cause.Error()
		}
	}
	return p.cause.Error()
}

func (p *syntaxViolation) Unwrap() error {
	return p.cause
}

func (p *syntaxViolation) SourceContext() *syntax.SourceContext {
	return p.src
}

// PrimSyntaxViolation implements (%syntax-violation who message stx): raises a
// catchable error object whose message is "who: message", whose irritant is the
// datum of stx, and whose cause carries stx's source location (design §6
// Diagnostics). The Scheme matcher's one raise helper.
func PrimSyntaxViolation(cc machine.CallContext) error {
	mc, err := machine.RequireMachineContext(cc, "%syntax-violation")
	if err != nil {
		return err
	}
	who := mc.Arg(0)
	msg, ok := mc.Arg(1).(*values.String)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAString, "%%syntax-violation: message must be a string, got %s", mc.Arg(1).SchemeString())
	}
	stx := mc.Arg(2)
	irritant := stx
	var src *syntax.SourceContext
	sv, ok := stx.(syntax.SyntaxValue)
	if ok {
		src = firstLocatedContext(sv)
		irritant = sv.UnwrapAll()
	}
	text := fmt.Sprintf("%s: %s", who.SchemeString(), msg.Value)
	cause := &syntaxViolation{src: src, cause: werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s", text)}
	return machine.RaiseInPlace(mc, values.NewErrorObjectWithCause(text, cause, irritant), false)
}

// firstLocatedContext returns stx's own source context, or — when the node was
// synthesized and carries none — the first one found walking the spine.
//
// A synthesized SPINE over located leaves is the ordinary shape here: the Go
// syntax template producer builds its output pairs with a nil context (measured:
// #'(x y) yields a SyntaxPair whose SourceContext is nil over symbols located at
// the template's own columns), and generated code assembles lists the same way.
// Reporting "no location" for a form every element of which knows where it came
// from is the wrong answer, and design §6 Diagnostics asks this primitive for the
// form's location specifically. quote-syntax templates, which are read rather
// than built, hit the first branch.
//
// The walk is depth-first, car before cdr, so the reported position is the
// earliest element of the form rather than an arbitrary one. It descends only
// through pairs and vectors — an atom either has a context or has none.
func firstLocatedContext(stx syntax.SyntaxValue) *syntax.SourceContext {
	if stx == nil {
		return nil
	}
	src := stx.SourceContext()
	if src != nil {
		return src
	}
	switch s := stx.(type) {
	case *syntax.SyntaxPair:
		if syntax.IsSyntaxEmptyList(s) {
			return nil
		}
		car := firstLocatedContext(s.SyntaxCar())
		if car != nil {
			return car
		}
		return firstLocatedContext(s.SyntaxCdr())
	case *syntax.SyntaxVector:
		for _, e := range s.Values {
			q := firstLocatedContext(e)
			if q != nil {
				return q
			}
		}
	}
	return nil
}
