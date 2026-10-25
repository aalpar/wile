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

package wile_test

import (
	"context"
	"errors"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// TestPromotedVectorRefClassifiesIndexLikeTheCall pins that vector-ref says the
// same thing about a bad index whether the call is promoted to OpVectorRef
// (inside a procedure on a real engine) or goes through the registered
// primitive (a top-level call). Before, a bignum index was a type error with
// promoted-only wording ("expected exact integer index, got"), so one
// primitive classified one mistake two ways depending on where it was called.
func TestPromotedVectorRefClassifiesIndexLikeTheCall(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name     string
		index    string
		sentinel error
		want     string
	}{
		{"bignum", "100000000000000000000", werr.ErrIndexOutOfRange,
			"vector-ref: index 100000000000000000000 out of range"},
		{"negative", "-1", werr.ErrIndexOutOfRange, "vector-ref: index -1 must be non-negative"},
		{"past the end", "1", werr.ErrIndexOutOfRange, "vector-ref: index 1 out of bounds for length 1"},
		{"symbol", "'x", werr.ErrNotAnInteger, "vector-ref: expected an exact integer index but got x"},
		{"integral rational", "4/2", werr.ErrIndexOutOfRange, "vector-ref: index 2 out of bounds for length 1"},
	}
	ctx := context.Background()
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			eng, err := wile.NewEngine(ctx)
			qt.Assert(t, err, qt.IsNil)
			t.Cleanup(func() {
				_ = eng.Close()
			})
			_, err = eng.EvalMultiple(ctx, `(define (f v i) (vector-ref v i))`)
			qt.Assert(t, err, qt.IsNil)
			f, ok := eng.Get("f")
			qt.Assert(t, ok, qt.IsTrue)
			dis, err := eng.DisassembleValue(f)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, strings.Contains(dis, "VectorRef"), qt.IsTrue,
				qt.Commentf("the in-procedure arm must take the promoted op: %s", dis))

			for _, code := range []string{
				`(f (vector 1) ` + tc.index + `)`,
				`(vector-ref (vector 1) ` + tc.index + `)`,
			} {
				_, err = eng.EvalMultiple(ctx, code)
				qt.Assert(t, err, qt.IsNotNil, qt.Commentf("%s", code))
				qt.Check(t, errors.Is(err, tc.sentinel), qt.IsTrue, qt.Commentf("%s: %v", code, err))
				qt.Check(t, err.Error(), qt.Contains, tc.want, qt.Commentf("%s", code))
			}
		})
	}
}
