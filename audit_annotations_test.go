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

// Audit harness for PrimitiveSpec ReturnType annotations.
//
// See plans/2026-04-19-primitive-annotation-audit.md for scope and design.
//
// Phase 1 (axis A): docstring-example ↔ annotation.
// Report-only — never fails the build. After the first run we triage
// findings and decide which categories to promote to hard failures.

package wile

import (
	"context"
	"fmt"
	"regexp"
	"sort"
	"strings"
	"testing"

	"github.com/aalpar/wile/values"
)

// exampleRe matches `  (call ...)  => expected` lines in docstrings.
// Group 1: call form. Group 2: expected-result literal.
var exampleRe = regexp.MustCompile(`(?m)^\s*(\(.+?\))\s+=>\s+(\S[^\n]*?)\s*$`)

// callHeadRe extracts the head symbol of a parenthesized call.
var callHeadRe = regexp.MustCompile(`^\(\s*([^\s()]+)`)

type auditFinding struct {
	Primitive string
	Kind      string // type-mismatch | value-mismatch | eval-error | expected-unparseable
	Call      string
	Expected  string
	Declared  string
	Actual    string
	Category  string
}

func stripInlineComment(s string) string {
	q := s
	i := strings.Index(q, " ;")
	if i >= 0 {
		q = q[:i]
	}
	return strings.TrimSpace(q)
}

func renderType(t values.TypeConstraint) string {
	if t == nil {
		return "<nil>"
	}
	return t.Name()
}

func schemeRepr(v values.Value) string {
	if v == nil {
		return "<nil>"
	}
	return fmt.Sprintf("%T:%s", v, v.SchemeString())
}

// evalIsolated evaluates a single example in a fresh Engine so that
// top-level mutations (define, set!, parameterize on current-*-port)
// cannot leak into later examples. A full engine is required — both
// NewSchemeReportNamespace (runtime-only copy) and NewChildNamespace
// (empty runtime) drop the compile/expand-phase bindings that bootstrap
// macros (delay, guard, quote handling for dotted pairs) rely on.
func evalIsolated(ctx context.Context, call string, tag string) (values.Value, error) {
	source := "<audit:" + tag + ">"
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths())
	if err != nil {
		return nil, err
	}
	wrapped, err := eng.EvalMultipleWithSource(ctx, call, source)
	if err != nil {
		return nil, err
	}
	return wrapped.Internal(), nil
}

func TestAuditPrimitiveAnnotations(t *testing.T) {
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths())
	if err != nil {
		t.Fatalf("new engine: %v", err)
	}

	prims := eng.Registry().Primitives()

	var (
		totalPrims        int
		primsWithExamples int
		exampleCount      int
		selfCallExamples  int
		verified          int
		findings          []auditFinding
	)

	for _, pr := range prims {
		spec := pr.Spec
		totalPrims++
		if spec.ReturnType == nil {
			continue
		}
		matches := exampleRe.FindAllStringSubmatch(spec.Doc, -1)
		if len(matches) == 0 {
			continue
		}
		primsWithExamples++

		for _, m := range matches {
			exampleCount++
			call := strings.TrimSpace(m[1])
			expected := stripInlineComment(m[2])

			head := ""
			hm := callHeadRe.FindStringSubmatch(call)
			if len(hm) > 1 {
				head = hm[1]
			}
			if head != spec.Name {
				continue
			}
			selfCallExamples++

			// Isolate each example in a fresh Namespace snapshotted from
			// the engine's base. R7RS §6.12 semantics: all primitives are
			// still bound, but top-level mutations (define, set!) in one
			// example do not leak into the next.
			actual, evalErr := evalIsolated(ctx, call, spec.Name)
			if evalErr != nil {
				findings = append(findings, auditFinding{
					Primitive: spec.Name, Kind: "eval-error",
					Call: call, Expected: expected,
					Declared: renderType(spec.ReturnType),
					Actual:   evalErr.Error(), Category: spec.Category,
				})
				continue
			}

			_, ok, checkErr := spec.ReturnType.Check(actual)
			if !ok {
				detail := ""
				if checkErr != nil {
					detail = checkErr.Error()
				}
				findings = append(findings, auditFinding{
					Primitive: spec.Name, Kind: "type-mismatch",
					Call: call, Expected: expected,
					Declared: renderType(spec.ReturnType),
					Actual:   schemeRepr(actual) + " / " + detail,
					Category: spec.Category,
				})
				continue
			}

			// `#<...>` is the R7RS-style external representation for
			// opaque values (ports, records, namespaces, syntax objects).
			// It doesn't round-trip through the reader by design, so skip
			// the value-equality check and trust the type check we did
			// above.
			if strings.HasPrefix(expected, "#<") {
				verified++
				continue
			}
			expectedVal, expErr := evalIsolated(ctx,
				"(quote "+expected+")",
				"expected:"+spec.Name)
			if expErr != nil {
				findings = append(findings, auditFinding{
					Primitive: spec.Name, Kind: "expected-unparseable",
					Call: call, Expected: expected,
					Declared: renderType(spec.ReturnType),
					Actual:   expErr.Error(), Category: spec.Category,
				})
				continue
			}
			if !actual.EqualTo(expectedVal) {
				findings = append(findings, auditFinding{
					Primitive: spec.Name, Kind: "value-mismatch",
					Call: call, Expected: expected,
					Declared: renderType(spec.ReturnType),
					Actual:   schemeRepr(actual), Category: spec.Category,
				})
				continue
			}
			verified++
		}
	}

	byKind := map[string]int{}
	for _, f := range findings {
		byKind[f.Kind]++
	}

	t.Logf("audit summary: prims=%d with-examples=%d examples=%d self-call=%d",
		totalPrims, primsWithExamples, exampleCount, selfCallExamples)
	t.Logf("  verified:             %d", verified)
	t.Logf("  type-mismatch:        %d", byKind["type-mismatch"])
	t.Logf("  value-mismatch:       %d", byKind["value-mismatch"])
	t.Logf("  eval-error:           %d", byKind["eval-error"])
	t.Logf("  expected-unparseable: %d", byKind["expected-unparseable"])

	sort.SliceStable(findings, func(i, j int) bool {
		if findings[i].Kind != findings[j].Kind {
			return findings[i].Kind < findings[j].Kind
		}
		return findings[i].Primitive < findings[j].Primitive
	})

	if len(findings) > 0 {
		t.Logf("\n--- findings (%d) ---", len(findings))
	}
	for _, f := range findings {
		t.Logf("[%s] %s (category=%s)", f.Kind, f.Primitive, f.Category)
		t.Logf("    call:     %s", f.Call)
		t.Logf("    expected: %s", f.Expected)
		t.Logf("    declared: %s", f.Declared)
		t.Logf("    actual:   %s", f.Actual)
	}
}
