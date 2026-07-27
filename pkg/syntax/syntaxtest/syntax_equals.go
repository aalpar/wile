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

// Package syntaxtest provides test helpers for the syntax package.
package syntaxtest

import (
	"errors"
	"fmt"
	"reflect"

	"github.com/aalpar/wile/pkg/syntax"

	qt "github.com/frankban/quicktest"
)

// SyntaxEquals is a quicktest checker that compares syntax values.
var SyntaxEquals qt.Checker = &syntaxEqualsChecker{}

type syntaxEqualsChecker struct{}

func (p *syntaxEqualsChecker) ArgNames() []string {
	return []string{"got", "want"}
}

// Check implements Checker.Check by comparing got and args[0] as syntax values:
// their source contexts must be EqualTo, then their UnwrapAll results must be
// EqualTo.
func (p *syntaxEqualsChecker) Check(got any, args []any, note func(key string, value any)) (err error) {
	defer func() {
		// A panic is raised when the provided args are not comparable.
		r := recover()
		if r != nil {
			err = fmt.Errorf("%v", r) //nolint:gocritic // test helper, not Scheme runtime
		}
	}()

	want := args[0]

	// Unreachable: got.(error) succeeds only for a non-nil interface.
	_, ok := got.(error)
	if ok && got == nil {
		return errors.New("got non-nil error") //nolint:gocritic // test helper, not Scheme runtime
	}

	// Show error types when comparing errors with different types.
	gotErr, ok := got.(error)
	if ok {
		wantErr, ok := want.(error)
		if ok {
			gotType := reflect.TypeOf(gotErr)
			wantType := reflect.TypeOf(wantErr)
			if gotType != wantType {
				note("got type", qt.Unquoted(gotType.String()))
				note("want type", qt.Unquoted(wantType.String()))
			}
		}
		return errors.New("values are not equal") //nolint:gocritic // test helper, not Scheme runtime
	}

	gotSyntaxValue, ok0 := got.(syntax.SyntaxValue)
	wantSyntaxValue, ok1 := want.(syntax.SyntaxValue)
	if !ok0 || !ok1 {
		return errors.New("got and want must be of type Datum") //nolint:gocritic // test helper, not Scheme runtime
	}

	if !gotSyntaxValue.SourceContext().EqualTo(wantSyntaxValue.SourceContext()) {
		return errors.New("source contexts are not equal") //nolint:gocritic // test helper, not Scheme runtime
	}
	if !gotSyntaxValue.UnwrapAll().EqualTo(wantSyntaxValue.UnwrapAll()) {
		return errors.New("values are not equal") //nolint:gocritic // test helper, not Scheme runtime
	}

	return nil
}
