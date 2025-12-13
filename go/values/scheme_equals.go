// Copyright 2025 Aaron Alpar
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

package values

import (
	"errors"
	"fmt"
	"reflect"

	qt "github.com/frankban/quicktest"
)

var SchemeEquals qt.Checker = &schemeEqualsChecker{}

type schemeEqualsChecker struct {
}

func (c *schemeEqualsChecker) ArgNames() []string {
	return []string{"got", "want"}
}

// Check implements Checker.Check by checking that got == args[0].
func (c *schemeEqualsChecker) Check(got interface{}, args []interface{}, note func(key string, value interface{})) (err error) {
	defer func() {
		// A panic is raised when the provided args are not comparable.
		if r := recover(); r != nil {
			err = fmt.Errorf("%s", r)
		}
	}()

	want := args[0]

	// Customize error message for non-nil errors.
	_, ok := got.(error)
	if ok && got == nil {
		return errors.New("got non-nil error")
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
		return errors.New("values are not equal")
	}

	gotValue, ok0 := got.(Value)
	wantValue, ok1 := want.(Value)
	if !ok0 || !ok1 {
		return errors.New("got and want must be of type Datum")
	}

	// Binding(nil).(Binding) == false so check
	if EqualTo(gotValue, wantValue) {
		return nil
	}

	return errors.New("values are not equal")
}
