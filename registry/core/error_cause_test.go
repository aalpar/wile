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

package core_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/werr"
)

// TestVMErrorsCarrySentinelCause pins R9: VM and expander errors built via
// MachineContext.Error carried no Cause, so errors.Is against the failure's
// sentinel always returned false — the error looked typed but matched nothing.
// Routing them through WrapError(sentinel, msg) makes the sentinel matchable.
//
// Only the user-reachable sites are exercised here; the remaining causeless
// sites are can't-happen invariants (ErrInternal) not triggerable from Scheme.
func TestVMErrorsCarrySentinelCause(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		sentinel error
	}{
		{
			name:     "dynamic-wind before not a procedure",
			code:     `(dynamic-wind 1 (lambda () 1) (lambda () 2))`,
			sentinel: werr.ErrNotAProcedure,
		},
		{
			name:     "dynamic-wind after not a procedure",
			code:     `(dynamic-wind (lambda () 1) (lambda () 2) 3)`,
			sentinel: werr.ErrNotAProcedure,
		},
		{
			name:     "parameter applied to too many arguments",
			code:     `((make-parameter 5) 1 2)`,
			sentinel: werr.ErrWrongNumberOfArguments,
		},
		{
			name:     "syntax-rules no matching clause",
			code:     `(begin (define-syntax foo (syntax-rules () ((foo a) a))) (foo 1 2))`,
			sentinel: werr.ErrInvalidSyntax,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, err, qt.ErrorIs, tc.sentinel)
		})
	}
}
