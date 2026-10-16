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

// The expander dispatches a special form in an (environment <import-set>)
// namespace, as the compiler already did (I171).
//
// RED on a23c4543. A NewChildNamespace store is empty: the import copies the
// phase-0 KEYWORD row of begin, quote, define-syntax, … but not the phase-1
// HANDLER row the expander dispatches through. So the expander read every such
// form as a procedure call and expanded its operands, while the compiler,
// dispatching by the keyword's denoted form, still compiled it correctly. Only
// two things were observable: a define-syntax inside a begin or a body did not
// bind before the forms after it expanded ("no such binding m"), and quote did
// not shield its datum, so (quote (m)) expanded the macro call and answered 42.
//
// Oracles: racket's make-base-namespace answers 42 for the begin row; R7RS
// §4.1.2, racket and petite answer (m) for the quote row.

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
)

// notDefinedHere is CompileDefineSyntax's refusal of a define-syntax the
// expander did not reach as one.
const notDefinedHere = `(?s).*define-syntax: m was not defined: the expander did not reach this form as define-syntax.*`

func TestImportSetNamespaceDispatchesSpecialForms(t *testing.T) {
	tests := []struct {
		name string
		env  string
		// program is evaluated with env bound to the namespace under test.
		program string
		want    string
		// wantErr, when set, is the error the program must raise instead.
		wantErr string
	}{
		{
			name:    "define-syntax in a begin binds for the forms after it",
			env:     "(environment '(scheme base))",
			program: `(eval '(begin (define-syntax m (syntax-rules () ((_) 42))) (m)) env)`,
			want:    "42",
		},
		{
			name:    "define-syntax in a body binds for the forms after it",
			env:     "(environment '(scheme base))",
			program: `(eval '(let () (define-syntax m (syntax-rules () ((_) 42))) (m)) env)`,
			want:    "42",
		},
		{
			name: "quote shields its datum",
			env:  "(environment '(scheme base))",
			program: `(eval '(define-syntax m (syntax-rules () ((_) 42))) env)
			          (eval '(quote (m)) env)`,
			want: "(m)",
		},
		// The probe looks up the form the head DENOTES, not the spelling the user
		// wrote: q-begin and q-quote have no root row of their own.
		{
			name:    "prefixed define-syntax in a begin binds for the forms after it",
			env:     "(environment '(prefix (scheme base) q-))",
			program: `(eval '(q-begin (q-define-syntax m (q-syntax-rules () ((_) 42))) (m)) env)`,
			want:    "42",
		},
		{
			name: "prefixed quote shields its datum",
			env:  "(environment '(prefix (scheme base) q-))",
			program: `(eval '(q-define-syntax m (q-syntax-rules () ((_) 42))) env)
			          (eval '(q-quote (m)) env)`,
			want: "(m)",
		},
		// The root probe fires only for a head that denotes a form. define-syntax
		// is not in (only (scheme base) car), so DenotedForm is "" and the expander
		// reads it as a call and binds nothing. The compiler dispatches the same
		// head by spelling and reaches CompileDefineSyntax, which binds nothing
		// since I138 moved the right-hand side into the expander, so it refuses
		// rather than accept the definition as a no-op. A root probe by spelling
		// for such a head would bind m and answer 42 in the begin row.
		{
			name:    "an unbound head does not reach the root probe",
			env:     "(environment '(only (scheme base) car))",
			program: `(eval '(begin (define-syntax m (syntax-rules () ((_) 42))) (m)) env)`,
			wantErr: notDefinedHere,
		},
		{
			name:    "a lone define-syntax the expander did not reach is refused",
			env:     "(environment '(only (scheme base) car))",
			program: `(eval '(define-syntax m (syntax-rules () ((_) 42))) env)`,
			wantErr: notDefinedHere,
		},
		// Controls, green on a23c4543: these namespaces carry the handler rows in
		// their own store and never reach the root probe.
		{
			name:    "control: (wile small)",
			env:     "(environment '(wile small))",
			program: `(eval '(begin (define-syntax m (syntax-rules () ((_) 42))) (m)) env)`,
			want:    "42",
		},
		{
			name:    "control: (wile kitchen-sink)",
			env:     "(environment '(wile kitchen-sink))",
			program: `(eval '(begin (define-syntax m (syntax-rules () ((_) 42))) (m)) env)`,
			want:    "42",
		},
		{
			name:    "control: scheme-report-environment 5",
			env:     "(scheme-report-environment 5)",
			program: `(eval '(begin (define-syntax m (syntax-rules () ((_) 42))) (m)) env)`,
			want:    "42",
		},
		{
			name:    "control: scheme-report-environment 7",
			env:     "(scheme-report-environment 7)",
			program: `(eval '(begin (define-syntax m (syntax-rules () ((_) 42))) (m)) env)`,
			want:    "42",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			eng := newPhaseRowsEngine(t)
			v, err := eng.EvalMultiple(context.Background(),
				`(let ((env `+tt.env+`)) `+tt.program+`)`)
			if tt.wantErr != "" {
				qt.Assert(t, tt.want, qt.Equals, "", qt.Commentf("a row sets want or wantErr, not both"))
				qt.Assert(t, err, qt.ErrorMatches, tt.wantErr)
				return
			}
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, tt.want)
		})
	}
}
