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

package core

import (
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/values"
)

//nolint:govet
func addContMarks(r *registry.PrimitiveRegistry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		// Continuation-mark values are opaque; there is no ValueType enum for
		// mark sets or prompt tags, so ReturnType is TypeAny.
		{Name: "current-continuation-marks", ParamCount: 1, IsVariadic: true, Impl: PrimCurrentContinuationMarks,
			Doc: "Returns the set of continuation marks on the current continuation up to the nearest prompt matching PROMPT-TAG.\n\nExamples:\n  (continuation-mark-set? (current-continuation-marks))  => #t", ParamNames: []string{"prompt-tag"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "continuation-mark-set->list", ParamCount: 2, Impl: PrimContinuationMarkSetToList,
			Doc: "Returns a list of all values associated with KEY in MARK-SET, from innermost to outermost frame.\n\nExamples:\n  (with-continuation-mark 'k 1\n    (continuation-mark-set->list (current-continuation-marks) 'k))  => (1)", ParamNames: []string{"mark-set", "key"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny}, ReturnType: values.TypeList},
		{Name: "continuation-mark-set-first", ParamCount: 3, IsVariadic: true, Impl: PrimContinuationMarkSetFirst,
			Doc: "Returns the first (innermost) value for KEY in MARK-SET, or DEFAULT if no mark with KEY exists.\n\nExamples:\n  (with-continuation-mark 'k 42\n    (continuation-mark-set-first (current-continuation-marks) 'k #f))  => 42", ParamNames: []string{"mark-set", "key", "default"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny, values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "continuation-mark-set?", ParamCount: 1, Impl: PrimContinuationMarkSetQ,
			Doc: "Returns #t if OBJ is a continuation mark set.\n\nExamples:\n  (continuation-mark-set? (current-continuation-marks))  => #t\n  (continuation-mark-set? 42)  => #f", ParamNames: []string{"obj"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "call-with-immediate-continuation-mark", InvokesProcedure: true, ParamCount: 3, IsVariadic: true, Impl: PrimCallWithImmediateContMark,
			Doc: "Looks up KEY in the marks of the current frame (not parent frames), then calls PROC with the value or DEFAULT.\n\nExamples:\n  (with-continuation-mark 'k 42\n    (call-with-immediate-continuation-mark 'k values #f))  => 42", ParamNames: []string{"key", "proc", "default"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeProcedure, values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "continuation-marks", ParamCount: 2, IsVariadic: true, Impl: PrimContinuationMarks,
			Doc: "Returns the continuation mark set from CONT, optionally limited by PROMPT-TAG.\n\nExamples:\n  ;; (continuation-marks (call/cc values))  => mark set from captured continuation", ParamNames: []string{"cont", "prompt-tag"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeAny}, ReturnType: values.TypeAny},
		{Name: "continuation?", ParamCount: 1, Impl: PrimContinuationQ,
			Doc: "Returns #t if OBJ is a continuation captured by call/cc. Composable continuations from call-with-composable-continuation are not continuation?.\n\nExamples:\n  (call/cc (lambda (k) (continuation? k)))  => #t\n  (continuation? car)  => #f\n  ;; (call-with-composable-continuation (lambda (k) (continuation? k)) TAG)  => #f", ParamNames: []string{"obj"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "continuation-mark-set->list*", ParamCount: 3, IsVariadic: true, Impl: PrimContinuationMarkSetToListStar,
			Doc: "Like continuation-mark-set->list but for multiple keys. Returns a list of vectors from MARK-SET, one per frame containing any key in KEY-LIST.\n\nExamples:\n  ;; See `continuation-mark-set->iterator' in (wile control) for usage.", ParamNames: []string{"mark-set", "key-list", "none-v"}, Category: "continuations",
			ParamTypes: []values.TypeConstraint{values.TypeAny, values.TypeList, values.TypeAny}, ReturnType: values.TypeList},
	}, registry.PhaseSetRuntime)

	return nil
}
