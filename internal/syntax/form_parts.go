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

package syntax

import (
	"context"
	"fmt"

	"github.com/aalpar/wile/werr"
)

// FormArityError reports that a form is a proper list of the wrong length.
// It is the one FormParts failure that carries structured data — the bounds
// and the actual count — because callers re-render it in their own vocabulary:
// compilation reports keyword-inclusive "element" counts (FormParts counts the
// form keyword at index 0), while the validator restates them as keyword-
// exclusive "argument" counts. Structural failures (not a list, improper list)
// carry no such data and are returned as plain ErrInvalidSyntax wraps, not as
// this type. FormArityError Unwraps to werr.ErrInvalidSyntax so callers keep
// matching with errors.Is regardless of the failure mode.
type FormArityError struct {
	Name string
	// Min and Max are the element-count bounds (keyword included); Max < 0
	// means unbounded. Got is the actual element count.
	Min, Max, Got int
}

var _ error = (*FormArityError)(nil)

func (e *FormArityError) Error() string {
	if e.Min == e.Max && e.Max >= 0 {
		return fmt.Sprintf("%s: expected exactly %d element(s), got %d", e.Name, e.Min, e.Got)
	}
	if e.Got < e.Min {
		return fmt.Sprintf("%s: expected at least %d element(s), got %d", e.Name, e.Min, e.Got)
	}
	return fmt.Sprintf("%s: expected at most %d element(s), got %d", e.Name, e.Max, e.Got)
}

// Unwrap reports ErrInvalidSyntax so an arity failure matches with
// errors.Is(err, werr.ErrInvalidSyntax), like the structural failures.
func (*FormArityError) Unwrap() error {
	return werr.ErrInvalidSyntax
}

// FormParts destructures a fixed-shape syntax form into its positional
// elements, validating that the form is a proper list of acceptable length.
//
// It replaces the recurring hand-rolled "assert *SyntaxPair, take car, assert
// cdr, ..." chains used by special-form compilers and expanders. name is the
// form name used in error messages. minLen and maxLen bound the number of
// elements (set maxLen to -1 for an unbounded upper limit; set minLen == maxLen
// for an exact count). All elements are counted — callers that keep the form
// keyword at element 0 should include it in the bounds.
//
// On success it returns the elements in source order. On a structural failure
// (not a list, improper list) it returns a nil slice and an ErrInvalidSyntax
// wrap naming the form. On an arity mismatch it returns a nil slice and a
// *FormArityError carrying the bounds and actual count; callers add source
// context. Both error forms match errors.Is(err, werr.ErrInvalidSyntax).
func FormParts(form SyntaxValue, name string, minLen, maxLen int) ([]SyntaxValue, error) {
	if IsSyntaxEmptyList(form) {
		return checkFormArity(nil, name, minLen, maxLen)
	}
	pair, ok := form.(*SyntaxPair)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: form must be a list", name)
	}
	var elements []SyntaxValue
	tail, err := pair.SyntaxForEach(context.Background(),
		func(_ context.Context, _ int, _ bool, v SyntaxValue) error {
			elements = append(elements, v)
			return nil
		})
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "%s: error traversing form", name)
	}
	if !IsSyntaxEmptyList(tail) {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidSyntax, "%s: form must be a proper list", name)
	}
	return checkFormArity(elements, name, minLen, maxLen)
}

// checkFormArity validates the element count against [minLen, maxLen] bounds and
// returns the elements unchanged on success, or a *FormArityError on mismatch.
func checkFormArity(elements []SyntaxValue, name string, minLen, maxLen int) ([]SyntaxValue, error) {
	n := len(elements)
	if minLen == maxLen && maxLen >= 0 {
		if n != minLen {
			return nil, &FormArityError{Name: name, Min: minLen, Max: maxLen, Got: n}
		}
		return elements, nil
	}
	if n < minLen {
		return nil, &FormArityError{Name: name, Min: minLen, Max: maxLen, Got: n}
	}
	if maxLen >= 0 && n > maxLen {
		return nil, &FormArityError{Name: name, Min: minLen, Max: maxLen, Got: n}
	}
	return elements, nil
}
