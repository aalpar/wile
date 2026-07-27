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

// Package valuestest provides test helpers for the values package.
package valuestest

import (
	"errors"
	"fmt"
	"reflect"

	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// SchemeEquals is a quicktest checker for Scheme value equality.
var SchemeEquals qt.Checker = &schemeEqualsChecker{} //nolint:gocritic // test helper, not Scheme runtime

type schemeEqualsChecker struct{}

func (p *schemeEqualsChecker) ArgNames() []string {
	return []string{"got", "want"}
}

// Check implements Checker.Check by comparing got and args[0] with
// values.EqualTo.
func (p *schemeEqualsChecker) Check(got any, args []any, note func(key string, value any)) (err error) {
	defer func() {
		// A panic is raised when the provided args are not comparable.
		r := recover()
		if r != nil {
			err = fmt.Errorf("%s", r) //nolint:gocritic // test helper, not Scheme runtime
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

	gotValue, ok0 := got.(values.Value)
	wantValue, ok1 := want.(values.Value)
	if !ok0 || !ok1 {
		return errors.New("got and want must be of type Datum") //nolint:gocritic // test helper, not Scheme runtime
	}

	if values.EqualTo(gotValue, wantValue) {
		return nil
	}

	return errors.New("values are not equal") //nolint:gocritic // test helper, not Scheme runtime
}
