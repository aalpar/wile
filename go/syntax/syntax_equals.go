package syntax

import (
	"errors"
	"fmt"
	"reflect"

	qt "github.com/frankban/quicktest"
)

var SyntaxEquals qt.Checker = &syntaxEqualsChecker{}

type syntaxEqualsChecker struct {
}

func (c *syntaxEqualsChecker) ArgNames() []string {
	return []string{"got", "want"}
}

// Check implements Checker.Check by checking that got == args[0].
func (c *syntaxEqualsChecker) Check(got interface{}, args []interface{}, note func(key string, value interface{})) (err error) {
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

	gotSyntaxValue, ok0 := got.(SyntaxValue)
	wantSyntaxValue, ok1 := want.(SyntaxValue)
	if !ok0 || !ok1 {
		return errors.New("got and want must be of type Datum")
	}

	if !gotSyntaxValue.SourceContext().EqualTo(wantSyntaxValue.SourceContext()) {
		return errors.New("source context are not equal")
	}
	if !gotSyntaxValue.UnwrapAll().EqualTo(wantSyntaxValue.UnwrapAll()) {
		return errors.New("values are not equal")
	}

	return nil
}
