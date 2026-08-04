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

func addHashtables(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "make-hashtable", ParamCount: 3, IsVariadic: true, Impl: PrimMakeHashtable,
			Doc: "Returns a new empty mutable hashtable using HASH and EQUIV. Only the built-in pair (equal-hash, equal?) is supported; any other pair, including user-written procedures, raises. The optional size hint K is accepted and ignored.\n\nFor the other two R6RS key equivalences use make-eq-hashtable or make-eqv-hashtable; make-equal-hashtable is the shorter spelling of this one.\n\nExamples:\n  (hashtable-size (make-hashtable equal-hash equal?))  => 0", ParamNames: []string{"hash", "equiv", "k"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeProcedure, values.TypeProcedure, values.TypeAny},
			ReturnType: values.TypeHashtable,
			Keywords:   []string{"hash map", "dictionary", "map", "associative array", "hash table", "R6RS"}},
		// The three fixed-kind constructors. ParamCount 1 + IsVariadic is "0
		// required plus a rest slot" per AcceptsArity's n >= paramCount-1;
		// ParamCount 0 with IsVariadic panics, so 1 is the minimum.
		{Name: "make-eq-hashtable", ParamCount: 1, IsVariadic: true, Impl: PrimMakeEqHashtable,
			Doc:        "Returns a new empty mutable hashtable whose keys are compared with eq?. The optional size hint K is accepted and ignored.\n\nExamples:\n  (hashtable-size (make-eq-hashtable))  => 0",
			ParamNames: []string{"k"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeHashtable,
			Keywords:   []string{"hash map", "dictionary", "eq", "R6RS"}},
		{Name: "make-eqv-hashtable", ParamCount: 1, IsVariadic: true, Impl: PrimMakeEqvHashtable,
			Doc:        "Returns a new empty mutable hashtable whose keys are compared with eqv?. The optional size hint K is accepted and ignored.\n\nExamples:\n  (hashtable-size (make-eqv-hashtable))  => 0",
			ParamNames: []string{"k"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeHashtable,
			Keywords:   []string{"hash map", "dictionary", "eqv", "R6RS"}},
		{Name: "make-equal-hashtable", ParamCount: 1, IsVariadic: true, Impl: PrimMakeEqualHashtable,
			Doc:        "Returns a new empty mutable hashtable whose keys are compared with equal?, so lists, vectors and strings work as keys by structure. The optional size hint K is accepted and ignored.\n\nNOT R6RS: this is the Chez / Larceny / Vicare / Ypsilon extension. The portable R6RS spelling is (make-hashtable equal-hash equal?), which Wile also accepts.\n\nPrefer make-eq-hashtable when the keys are objects whose equal? IS identity — a record type, a port, a procedure. Those all hash to one bucket here and scan linearly, where an eq table hashes them by identity.\n\nExamples:\n  (let ((h (make-equal-hashtable))) (hashtable-set! h (list 1 2) 'v) (hashtable-ref h (list 1 2) #f))  => v",
			ParamNames: []string{"k"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeAny},
			ReturnType: values.TypeHashtable,
			Keywords:   []string{"hash map", "dictionary", "equal", "structural"}},
		{Name: "hashtable?", ParamCount: 1, Impl: PrimHashtableQ,
			Doc: "Returns #t if OBJ is a hashtable.\n\nExamples:\n  (hashtable? (make-hashtable))  => #t\n  (hashtable? '())               => #f", ParamNames: []string{"obj"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeAny}, ReturnType: values.TypeBoolean},
		{Name: "hashtable-ref", ParamCount: 3, Impl: PrimHashtableRef,
			Doc: "Returns the value associated with KEY in HT, or DEFAULT if KEY is absent. DEFAULT is REQUIRED — R6RS has no two-argument form, and there is no missing-key error.\n\nExamples:\n  (let ((ht (make-equal-hashtable))) (hashtable-set! ht 'a 1) (hashtable-ref ht 'a #f))  => 1\n  (hashtable-ref (make-equal-hashtable) 'x 42)  => 42", ParamNames: []string{"ht", "key", "default"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeHashtable, values.TypeAny, values.TypeAny}, ReturnType: values.TypeAny,
			Keywords: []string{"lookup", "get", "retrieve", "dictionary lookup"}},
		{Name: "hashtable-set!", ParamCount: 3, Impl: PrimHashtableSet,
			Doc: "Associates KEY with VALUE in HT, replacing any existing entry for KEY.\n\nExamples:\n  (let ((ht (make-hashtable))) (hashtable-set! ht 'a 1) (hashtable-ref ht 'a #f))  => 1", ParamNames: []string{"ht", "key", "value"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeHashtable, values.TypeAny, values.TypeAny}, ReturnType: values.TypeVoid,
			Keywords: []string{"insert", "put", "store", "dictionary insert"}},
		{Name: "hashtable-delete!", ParamCount: 2, Impl: PrimHashtableDelete,
			Doc: "Removes the entry for KEY from HT. Does nothing if KEY is not present.\n\nExamples:\n  (let ((ht (make-hashtable))) (hashtable-set! ht 'a 1) (hashtable-delete! ht 'a) (hashtable-size ht))  => 0", ParamNames: []string{"ht", "key"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeHashtable, values.TypeAny}, ReturnType: values.TypeVoid,
			Keywords: []string{"remove", "erase", "dictionary remove"}},
		{Name: "hashtable-keys", ParamCount: 1, Impl: PrimHashtableKeys,
			Doc: "Returns a VECTOR of all keys in HT. The order is unspecified. Wile previously returned a list.\n\nUse hashtable-entries to get keys and values together and index-aligned; there is no hashtable-values.\n\nExamples:\n  (let ((ht (make-equal-hashtable))) (hashtable-set! ht 'a 1) (vector-length (hashtable-keys ht)))  => 1", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeHashtable}, ReturnType: values.TypeVector},
		{Name: "hashtable-size", ParamCount: 1, Impl: PrimHashtableSize,
			Doc: "Returns the number of key-value pairs in HT.\n\nExamples:\n  (hashtable-size (make-hashtable))  => 0", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeHashtable}, ReturnType: values.TypeInteger},
		{Name: "hashtable-copy", ParamCount: 2, IsVariadic: true, Impl: PrimHashtableCopy,
			Doc: "Returns a shallow copy of HT. Keys and values are shared with the original.\n\nIf MUTABLE is supplied and not #f the copy is mutable; OTHERWISE IT IS IMMUTABLE, per R6RS. This reverses Wile's earlier one-argument behaviour, so code that copies and then mutates must now pass #t.\n\nExamples:\n  (hashtable-mutable? (hashtable-copy (make-equal-hashtable)))     => #f\n  (hashtable-mutable? (hashtable-copy (make-equal-hashtable) #t))  => #t", ParamNames: []string{"ht", "mutable"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeHashtable, values.TypeAny}, ReturnType: values.TypeHashtable},
		{Name: "hashtable-mutable?", ParamCount: 1, Impl: PrimHashtableMutableQ,
			Doc: "Returns #t if HT accepts hashtable-set!, hashtable-delete! and hashtable-clear!. Only hashtable-copy without a true MUTABLE argument produces an immutable table.\n\nExamples:\n  (hashtable-mutable? (make-equal-hashtable))  => #t", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeHashtable}, ReturnType: values.TypeBoolean,
			Keywords: []string{"immutable", "R6RS"}},
		{Name: "hashtable-clear!", ParamCount: 2, IsVariadic: true, Impl: PrimHashtableClear,
			Doc: "Removes all entries from HT, leaving it empty. The optional size hint K is accepted and IGNORED: R6RS calls it a hint implementations are free to ignore, and the backing store has no capacity knob.\n\nExamples:\n  (let ((ht (make-hashtable))) (hashtable-set! ht 'a 1) (hashtable-clear! ht) (hashtable-size ht))  => 0", ParamNames: []string{"ht", "k"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeHashtable, values.TypeAny}, ReturnType: values.TypeVoid},
		{Name: "hashtable-contains?", ParamCount: 2, Impl: PrimHashtableContainsQ,
			Doc: "Returns #t if HT has an entry for KEY.\n\nExamples:\n  (let ((h (make-equal-hashtable))) (hashtable-set! h 'a 1) (hashtable-contains? h 'a))  => #t", ParamNames: []string{"ht", "key"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeHashtable, values.TypeAny}, ReturnType: values.TypeBoolean,
			Keywords: []string{"member", "has key", "R6RS"}},
		{Name: "hashtable-entries", ParamCount: 1, Impl: PrimHashtableEntries,
			Doc: "Returns TWO values: a vector of HT's keys and an index-aligned vector of its values, so the ith element of the second is the value of the ith element of the first. The order is unspecified but consistent between the two.\n\nExamples:\n  (call-with-values (lambda () (hashtable-entries h)) (lambda (ks vs) (vector-length ks)))", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeHashtable}, ReturnType: values.TypeAny,
			Keywords: []string{"items", "pairs", "R6RS"}},
		{Name: "hashtable-equivalence-function", ParamCount: 1, Impl: PrimHashtableEquivalenceFunction,
			Doc: "Returns the procedure HT compares keys with: eq?, eqv? or equal?. The result is this namespace's own binding, so (eq? (hashtable-equivalence-function (make-eq-hashtable)) eq?) is #t.\n\nExamples:\n  (eq? (hashtable-equivalence-function (make-equal-hashtable)) equal?)  => #t", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeHashtable}, ReturnType: values.TypeProcedure,
			Keywords: []string{"R6RS"}},
		{Name: "hashtable-hash-function", ParamCount: 1, Impl: PrimHashtableHashFunction,
			Doc: "Returns equal-hash for an equal?-keyed table, and #f — not a procedure — for eq and eqv tables, which hash by identity. That #f is R6RS, not a shortcut.\n\nExamples:\n  (eq? (hashtable-hash-function (make-equal-hashtable)) equal-hash)  => #t\n  (hashtable-hash-function (make-eq-hashtable))                      => #f", ParamNames: []string{"ht"}, Category: "hashtables",
			ParamTypes: []values.TypeConstraint{values.TypeHashtable}, ReturnType: values.TypeAny,
			Keywords: []string{"R6RS"}},
	}, registry.PhaseSetRuntime|registry.PhaseSetExpand)

	return nil
}
