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

package machine_test

import (
	"context"
	"io"
	"strings"
	"testing"

	"wile/machine"
	"wile/parser"
	schemertime "wile/runtime"
	"wile/values"

	qt "github.com/frankban/quicktest"
)

// evalScheme evaluates a Scheme expression string and returns the result
func evalScheme(t *testing.T, code string) values.Value {
	t.Helper()
	env, err := schemertime.NewTopLevelEnvironmentFrameTiny()
	qt.Assert(t, err, qt.IsNil)

	ctx := context.Background()
	rdr := strings.NewReader(code)
	p := parser.NewParser(env, rdr)

	var lastValue values.Value = values.Void

	for {
		stx, err := p.ReadSyntax(nil)
		if err == io.EOF {
			break
		}
		qt.Assert(t, err, qt.IsNil)

		// Expand
		ectx := machine.NewExpandTimeCallContext()
		expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
		qt.Assert(t, err, qt.IsNil)

		// Compile
		tpl := machine.NewNativeTemplate(0, 0, false)
		cctx := machine.NewCompileTimeCallContext(false, true, env)
		err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
		qt.Assert(t, err, qt.IsNil)

		// Run
		cont := machine.NewMachineContinuation(nil, tpl, env)
		mc := machine.NewMachineContext(cont)
		err = mc.Run(ctx)
		qt.Assert(t, err, qt.IsNil)

		lastValue = mc.GetValue()
	}

	return lastValue
}

func TestDefineRecordTypeBasic(t *testing.T) {
	c := qt.New(t)

	// Define a simple point record and test basic operations
	result := evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x)
			(y point-y))
		(make-point 3 4)
	`)

	// Result should be a record
	rec, ok := result.(*values.Record)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected Record, got %T", result))
	c.Assert(rec.RecordType().Name().SchemeString(), qt.Equals, ":point")
}

func TestRecordPredicate(t *testing.T) {
	c := qt.New(t)

	// Test predicate returns true for correct type
	result := evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x)
			(y point-y))
		(point? (make-point 3 4))
	`)
	c.Assert(result, qt.Equals, values.TrueValue)

	// Test predicate returns false for wrong type
	result = evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x)
			(y point-y))
		(point? 42)
	`)
	c.Assert(result, qt.Equals, values.FalseValue)

	// Test predicate returns false for different record type
	result = evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x)
			(y point-y))
		(define-record-type :circle
			(make-circle r)
			circle?
			(r circle-r))
		(point? (make-circle 5))
	`)
	c.Assert(result, qt.Equals, values.FalseValue)
}

func TestRecordAccessors(t *testing.T) {
	c := qt.New(t)

	// Test field accessors
	result := evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x)
			(y point-y))
		(point-x (make-point 3 4))
	`)
	c.Assert(result, values.SchemeEquals, values.NewInteger(3))

	result = evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x)
			(y point-y))
		(point-y (make-point 3 4))
	`)
	c.Assert(result, values.SchemeEquals, values.NewInteger(4))
}

func TestRecordModifiers(t *testing.T) {
	c := qt.New(t)

	// Test field modifier
	result := evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x set-point-x!)
			(y point-y))
		(let ((p (make-point 3 4)))
			(set-point-x! p 10)
			(point-x p))
	`)
	c.Assert(result, values.SchemeEquals, values.NewInteger(10))
}

func TestRecordPartialConstructor(t *testing.T) {
	c := qt.New(t)

	// Test constructor with subset of fields
	result := evalScheme(t, `
		(define-record-type :person
			(make-person name)
			person?
			(name person-name)
			(age person-age set-person-age!))
		(let ((p (make-person "Alice")))
			(set-person-age! p 30)
			(person-age p))
	`)
	c.Assert(result, values.SchemeEquals, values.NewInteger(30))
}

func TestRecordEquality(t *testing.T) {
	c := qt.New(t)

	// Test equal? on records with same type and values
	result := evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x)
			(y point-y))
		(equal? (make-point 3 4) (make-point 3 4))
	`)
	c.Assert(result, qt.Equals, values.TrueValue)

	// Test equal? on records with different values
	result = evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x)
			(y point-y))
		(equal? (make-point 3 4) (make-point 5 6))
	`)
	c.Assert(result, qt.Equals, values.FalseValue)

	// Test eq? on same record instance
	result = evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x)
			(y point-y))
		(let ((p (make-point 3 4)))
			(eq? p p))
	`)
	c.Assert(result, qt.Equals, values.TrueValue)

	// Test eq? on different record instances
	result = evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x)
			(y point-y))
		(eq? (make-point 3 4) (make-point 3 4))
	`)
	c.Assert(result, qt.Equals, values.FalseValue)
}

func TestRecordTypeQ(t *testing.T) {
	c := qt.New(t)

	// Test record-type? on a record type
	result := evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x)
			(y point-y))
		(record-type? :point)
	`)
	c.Assert(result, qt.Equals, values.TrueValue)

	// Test record-type? on a non-record-type
	result = evalScheme(t, `
		(record-type? 42)
	`)
	c.Assert(result, qt.Equals, values.FalseValue)
}

func TestRecordQ(t *testing.T) {
	c := qt.New(t)

	// Test record? on a record
	result := evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x)
			(y point-y))
		(record? (make-point 3 4))
	`)
	c.Assert(result, qt.Equals, values.TrueValue)

	// Test record? on a non-record
	result = evalScheme(t, `
		(record? '(1 2 3))
	`)
	c.Assert(result, qt.Equals, values.FalseValue)
}

func TestNestedRecords(t *testing.T) {
	c := qt.New(t)

	// Test records containing other records
	result := evalScheme(t, `
		(define-record-type :point
			(make-point x y)
			point?
			(x point-x)
			(y point-y))
		(define-record-type :line
			(make-line start end)
			line?
			(start line-start)
			(end line-end))
		(let ((l (make-line (make-point 0 0) (make-point 10 10))))
			(point-x (line-end l)))
	`)
	c.Assert(result, values.SchemeEquals, values.NewInteger(10))
}
