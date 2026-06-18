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

package machine_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestMachineContextWinding(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "dynamic-wind execution order",
			Code: `(let ((log '()))
                     (dynamic-wind
                       (lambda () (set! log (cons 'before log)))
                       (lambda () (set! log (cons 'during log)))
                       (lambda () (set! log (cons 'after log))))
                     (reverse log))`,
			Expected: values.List(
				values.NewSymbol("before"),
				values.NewSymbol("during"),
				values.NewSymbol("after"),
			),
		},
		{
			Name: "dynamic-wind thunk return value",
			Code: `(dynamic-wind
                     (lambda () #f)
                     (lambda () 42)
                     (lambda () #f))`,
			Expected: values.NewInteger(42),
		},
		{
			Name: "dynamic-wind with call/cc escape",
			Code: `(let ((log '()))
                     (let ((k (call/cc
                                (lambda (escape)
                                  (dynamic-wind
                                    (lambda () (set! log (cons 'before log)))
                                    (lambda () (escape (call/cc (lambda (c) c))))
                                    (lambda () (set! log (cons 'after log))))))))
                       (if (procedure? k)
                           (begin
                             (k 'done))
                           (reverse log))))`,
			Expected: values.List(
				values.NewSymbol("before"),
				values.NewSymbol("after"),
				values.NewSymbol("before"),
				values.NewSymbol("after"),
			),
		},
		{
			Name: "nested dynamic-wind",
			Code: `(let ((log '()))
                     (dynamic-wind
                       (lambda () (set! log (cons 'outer-before log)))
                       (lambda ()
                         (dynamic-wind
                           (lambda () (set! log (cons 'inner-before log)))
                           (lambda () (set! log (cons 'inner-during log)))
                           (lambda () (set! log (cons 'inner-after log)))))
                       (lambda () (set! log (cons 'outer-after log))))
                     (reverse log))`,
			Expected: values.List(
				values.NewSymbol("outer-before"),
				values.NewSymbol("inner-before"),
				values.NewSymbol("inner-during"),
				values.NewSymbol("inner-after"),
				values.NewSymbol("outer-after"),
			),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
