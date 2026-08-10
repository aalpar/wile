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

// Audit harness for the rest slot of variadic PrimitiveSpec.ParamTypes.
//
// The declaration language has one entry per slot, and validateVariadic
// (pkg/registry/contract.go) applies the LAST entry to EVERY element of the
// rest list. A primitive whose optional arguments are not all the same type
// therefore cannot be described by a non-TypeAny rest constraint, and
// declaring one makes WithContractEnforcement() reject calls the
// implementation accepts. Nothing called each ParamTypes-declaring spec
// against its implementation before this file.
//
// The rows are calls that supply the primitive's optional arguments. The
// assertion is differential: enforcement must not change whether a call is
// accepted. That needs no expected value per row, and it is exactly the
// property the defect breaks.

package wile

import (
	"context"
	"sort"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// restExercise is one call per variadic primitive, supplying as many rest
// arguments as the primitive accepts. Where the maximal form differs in type
// across rest positions, that is the case that catches a too-narrow
// declaration.
var restExercise = map[string]string{
	// Numeric — homogeneous rests.
	"*":    `(* 1 2 3)`,
	"+":    `(+ 1 2 3)`,
	"-":    `(- 10 3 2)`,
	"/":    `(/ 12 3 2)`,
	"<":    `(< 1 2 3)`,
	"<=":   `(<= 1 2 3)`,
	"=":    `(= 1 1 1)`,
	">":    `(> 3 2 1)`,
	">=":   `(>= 3 2 1)`,
	"atan": `(atan 1 1)`,
	"gcd":  `(gcd 12 18 24)`,
	"lcm":  `(lcm 2 3 4)`,
	"log":  `(log 8 2)`,
	"max":  `(max 1 2 3)`,
	"min":  `(min 1 2 3)`,

	// Characters and strings — homogeneous rests.
	"char-ci<=?":    `(char-ci<=? #\a #\b #\c)`,
	"char-ci<?":     `(char-ci<? #\a #\b #\c)`,
	"char-ci=?":     `(char-ci=? #\a #\a #\a)`,
	"char-ci>=?":    `(char-ci>=? #\c #\b #\a)`,
	"char-ci>?":     `(char-ci>? #\c #\b #\a)`,
	"char<=?":       `(char<=? #\a #\b #\c)`,
	"char<?":        `(char<? #\a #\b #\c)`,
	"char=?":        `(char=? #\a #\a #\a)`,
	"char>=?":       `(char>=? #\c #\b #\a)`,
	"char>?":        `(char>? #\c #\b #\a)`,
	"string":        `(string #\a #\b)`,
	"string-append": `(string-append "a" "b" "c")`,
	"string-ci<=?":  `(string-ci<=? "a" "b" "c")`,
	"string-ci<?":   `(string-ci<? "a" "b" "c")`,
	"string-ci=?":   `(string-ci=? "a" "a" "a")`,
	"string-ci>=?":  `(string-ci>=? "c" "b" "a")`,
	"string-ci>?":   `(string-ci>? "c" "b" "a")`,
	"string<=?":     `(string<=? "a" "b" "c")`,
	"string<?":      `(string<? "a" "b" "c")`,
	"string=?":      `(string=? "a" "a" "a")`,
	"string>=?":     `(string>=? "c" "b" "a")`,
	"string>?":      `(string>? "c" "b" "a")`,

	// start/end pairs living in the rest slot behind a typed fixed prefix.
	"bytevector-copy":  `(bytevector-copy #u8(1 2 3) 1 3)`,
	"bytevector-copy!": `(let ((bv (make-bytevector 4 0))) (bytevector-copy! bv 0 #u8(1 2 3) 1 3) bv)`,
	"string->list":     `(string->list "abc" 1 3)`,
	"string->utf8":     `(string->utf8 "abc" 1 3)`,
	"string->vector":   `(string->vector "abc" 1 3)`,
	"string-copy":      `(string-copy "abc" 1 3)`,
	"string-copy!":     `(let ((s (string-copy "xxxx"))) (string-copy! s 0 "abc" 1 3) s)`,
	"string-fill!":     `(let ((s (string-copy "abc"))) (string-fill! s #\z 1 3) s)`,
	"utf8->string":     `(utf8->string #u8(97 98 99) 1 3)`,
	"vector->list":     `(vector->list #(1 2 3) 1 3)`,
	"vector->string":   `(vector->string (vector #\a #\b #\c) 1 3)`,
	"vector-copy":      `(vector-copy #(1 2 3) 1 3)`,
	"vector-fill!":     `(let ((v (vector 1 2 3))) (vector-fill! v 0 1 3) v)`,

	// Heterogeneous rests: a container or port first, then integer indices.
	"vector-copy!":     `(let ((v (vector 1 2 3))) (vector-copy! v 0 (vector 7 8 9) 0 2) v)`,
	"write-string":     `(let ((p (open-output-string))) (write-string "hello" p 1 3) (get-output-string p))`,
	"write-bytevector": `(let ((p (open-output-bytevector))) (write-bytevector #u8(1 2 3) p 1 3) (get-output-bytevector p))`,
	"read-bytevector!": `(let ((bv (make-bytevector 3 0))) (read-bytevector! bv (open-input-bytevector #u8(1 2)) 0 2) bv)`,
	// A symbol first, then a default of any type.
	"namespace-ref": `(namespace-ref (make-namespace) 'missing 42)`,

	// Homogeneous container rests.
	"bytevector":        `(bytevector 1 2 3)`,
	"bytevector-append": `(bytevector-append #u8(1) #u8(2))`,
	"vector-append":     `(vector-append #(1) #(2))`,
	"make-bytevector":   `(make-bytevector 3 7)`,
	"make-string":       `(make-string 3 #\a)`,
	"number->string":    `(number->string 255 16)`,
	"string->number":    `(string->number "ff" 16)`,

	// Ports in the rest slot.
	"char-ready?":       `(char-ready? (open-input-string "a"))`,
	"display":           `(display 1 (open-output-string))`,
	"flush-output-port": `(flush-output-port (open-output-string))`,
	"newline":           `(newline (open-output-string))`,
	"peek-char":         `(peek-char (open-input-string "a"))`,
	"peek-u8":           `(peek-u8 (open-input-bytevector #u8(1)))`,
	"read":              `(read (open-input-string "1"))`,
	"read-bytevector":   `(read-bytevector 2 (open-input-bytevector #u8(1 2 3)))`,
	"read-char":         `(read-char (open-input-string "a"))`,
	"read-line":         `(read-line (open-input-string "a"))`,
	"read-string":       `(read-string 1 (open-input-string "ab"))`,
	"read-syntax":       `(read-syntax (open-input-string "1"))`,
	"read-token":        `(read-token (open-input-string "1"))`,
	"read-u8":           `(read-u8 (open-input-bytevector #u8(1)))`,
	"u8-ready?":         `(u8-ready? (open-input-bytevector #u8(1)))`,
	"write":             `(write 1 (open-output-string))`,
	"write-char":        `(write-char #\a (open-output-string))`,
	"write-shared":      `(write-shared 1 (open-output-string))`,
	"write-simple":      `(write-simple 1 (open-output-string))`,
	"write-u8":          `(let ((p (open-output-bytevector))) (write-u8 65 p) (get-output-bytevector p))`,

	// Miscellaneous constrained rests.
	// A profile name rather than a library, so the row does not depend on
	// library-path configuration; the rest slot sees a list either way.
	"environment":                  `(environment '(wile tiny))`,
	"make-continuation-prompt-tag": `(make-continuation-prompt-tag 'tag)`,
	"make-parameter":               `(make-parameter 1 (lambda (x) x))`,
}

// restExerciseSkip lists constrained-rest primitives with no safe in-process
// call, mapped to why. A skip is a debt, not an exemption: it must name the
// hazard, not the effort.
var restExerciseSkip = map[string]string{
	"process-spawn": "spawns a real OS process; the rest is a homogeneous string argv",
}

// TestParamTypesRestSlotAcceptsWhatTheImplementationDoes calls every variadic
// ParamTypes-declaring primitive whose rest constraint can reject something,
// with and without WithContractEnforcement(). Enforcement must not change
// whether the call is accepted.
func TestParamTypesRestSlotAcceptsWhatTheImplementationDoes(t *testing.T) {
	ctx := context.Background()

	plain, err := NewEngine(ctx, WithProfile(KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer plain.Close()

	strict, err := NewEngine(ctx, WithProfile(KitchenSink), WithContractEnforcement())
	qt.Assert(t, err, qt.IsNil)
	defer strict.Close()

	names := make([]string, 0, len(restExercise))
	for name := range restExercise {
		names = append(names, name)
	}
	sort.Strings(names)

	for _, name := range names {
		t.Run(name, func(t *testing.T) {
			c := qt.New(t)
			code := restExercise[name]

			_, plainErr := plain.EvalMultiple(ctx, code)
			c.Assert(plainErr, qt.IsNil,
				qt.Commentf("fixture %s does not evaluate with enforcement off; fix the row, not the assertion", code))

			_, strictErr := strict.EvalMultiple(ctx, code)
			c.Assert(strictErr, qt.IsNil,
				qt.Commentf("WithContractEnforcement() rejected a call the implementation accepts: %s\n"+
					"the last ParamTypes entry constrains EVERY rest element, so a heterogeneous "+
					"rest must declare values.TypeAny", code))
		})
	}
}

// TestParamTypesRestSlotCensusIsComplete is the ratchet. A variadic
// ParamTypes-declaring spec whose rest constraint is neither nil nor TypeAny
// can reject an argument, so it must be exercised above or explicitly skipped
// with a hazard. A new primitive with a constrained rest fails here until it
// has a row.
//
// The reverse direction is deliberately looser: a row survives its primitive
// widening to TypeAny, because those rows are exactly the regression guard for
// the specs this audit already corrected. Only a row naming something that is
// no longer a variadic ParamTypes-declaring primitive at all is stale.
func TestParamTypesRestSlotCensusIsComplete(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer eng.Close()

	var missing []string
	declaring := map[string]bool{}
	for _, pr := range eng.Registry().Primitives() {
		spec := pr.Spec
		if !spec.IsVariadic || len(spec.ParamTypes) == 0 {
			continue
		}
		declaring[spec.Name] = true
		rest := spec.ParamTypes[len(spec.ParamTypes)-1]
		if rest == nil || rest == values.TypeAny {
			continue
		}
		_, hasRow := restExercise[spec.Name]
		_, skipped := restExerciseSkip[spec.Name]
		if !hasRow && !skipped {
			missing = append(missing, spec.Name+" (rest: "+rest.Name()+")")
		}
	}
	sort.Strings(missing)
	qt.Assert(t, missing, qt.HasLen, 0,
		qt.Commentf("variadic primitives with a constrained rest and no row:\n  %s",
			strings.Join(missing, "\n  ")))

	var stale []string
	for name := range restExercise {
		if !declaring[name] {
			stale = append(stale, name)
		}
	}
	for name := range restExerciseSkip {
		if !declaring[name] {
			stale = append(stale, name)
		}
	}
	sort.Strings(stale)
	qt.Assert(t, stale, qt.HasLen, 0,
		qt.Commentf("rows for primitives that are no longer variadic ParamTypes-declaring:\n  %s",
			strings.Join(stale, "\n  ")))
}
