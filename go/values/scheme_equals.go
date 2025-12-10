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
