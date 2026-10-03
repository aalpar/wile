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

package wile

import (
	"context"
	"errors"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// TestMakeHashtableRecognitionSurvivesImports pins that (make-hashtable
// equal-hash equal?) keeps working after an import — including (import (scheme
// base)), which every conforming R7RS program begins with.
//
// A library environment is a flat island: environment/namespace.go NewChildRuntime
// gives it parent nil, so the library env factory re-applies the whole registry
// into it and every primitive is minted a SECOND *machine.ForeignClosure there.
// (scheme base) exports equal?, so after that import the caller's equal? is the
// library's closure while the sealed base still holds the engine's. Recognition by
// closure POINTER therefore saw two different objects for one primitive and
// refused the pair.
//
// The identity token is what makes these cases agree: every closure built from one
// PrimitiveSpec carries the same *machine.PrimitiveIdentity, whichever environment
// minted it, so recognition asks "is this the registered equal-hash?" rather than
// "is this the sealed base's copy of it?".
func TestMakeHashtableRecognitionSurvivesImports(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		expr string
	}{
		{"before any import",
			`(hashtable? (make-hashtable equal-hash equal?))`},
		// The regression case: (scheme base) exports equal?, which is enough on
		// its own — equal-hash need not be exported by anything.
		{"after (import (scheme base))",
			`(begin (import (scheme base)) (hashtable? (make-hashtable equal-hash equal?)))`},
		{"after (import (rnrs hashtables))",
			`(begin (import (rnrs hashtables)) (hashtable? (make-hashtable equal-hash equal?)))`},
		{"after (import (srfi 13))",
			`(begin (import (srfi 13)) (hashtable? (make-hashtable equal-hash equal?)))`},
		{"after both",
			`(begin (import (scheme base)) (import (rnrs hashtables)) (hashtable? (make-hashtable equal-hash equal?)))`},
		// The table is genuinely equal?-keyed, not merely a hashtable: a fresh
		// list must find the entry a structurally-equal list stored.
		{"and the table is equal?-keyed after the import",
			`(begin (import (scheme base)) (let ((h (make-hashtable equal-hash equal?))) (hashtable-set! h (list 1 2) 'x) (eq? (hashtable-ref h (list 1 2) #f) 'x)))`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			// A FRESH engine per case: an import mutates the top level, so
			// reusing one engine would let an earlier case decide a later one.
			got := evalSRFI(c, newSRFITestEngine(t), tc.expr)
			c.Assert(got, qt.Equals, `#t`)
		})
	}
}

// TestMakeHashtableRejectsUnregisteredProcedures is the fail-closed half. The
// token must not be forgeable from Scheme: a user procedure named equal-hash, or
// any other Scheme closure, carries no identity and is refused. Widening
// recognition to a name compare would accept these and silently hash with the
// wrong function.
func TestMakeHashtableRejectsUnregisteredProcedures(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		expr string
	}{
		{"a lambda in the hash position",
			`(let ((eh (lambda (x) 0))) (make-hashtable eh equal?))`},
		{"a lambda in the equivalence position",
			`(let ((eq2 (lambda (a b) #t))) (make-hashtable equal-hash eq2))`},
		{"a different registered primitive in the hash position",
			`(make-hashtable string-hash equal?)`},
		{"a different registered primitive in the equivalence position",
			`(make-hashtable equal-hash eqv?)`},
		{"a non-procedure",
			`(make-hashtable 1 2)`},
		{"the pair in the wrong order",
			`(make-hashtable equal? equal-hash)`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			eng := newSRFITestEngine(t)
			_, err := eng.EvalMultiple(t.Context(), tc.expr)
			c.Assert(err, qt.IsNotNil)
		})
	}
}

// TestHashtableAccessorsRoundTripAfterImport pins the accessors against the same
// split. R6RS lets a program read a table's (hash, equivalence) pair back off it
// and build another table from them, so whatever hashtable-hash-function returns
// must be something make-hashtable accepts — before and after an import.
//
// The eq? cases are the stronger claim: the accessor hands back the binding the
// CALLER holds, not the sealed base's copy, so the pair a program reads off a
// table is the pair it wrote.
func TestHashtableAccessorsRoundTripAfterImport(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		expr string
	}{
		{"round-trip without an import",
			`(let ((h (make-equal-hashtable))) (hashtable? (make-hashtable (hashtable-hash-function h) (hashtable-equivalence-function h))))`},
		{"round-trip after (import (scheme base))",
			`(begin (import (scheme base)) (let ((h (make-equal-hashtable))) (hashtable? (make-hashtable (hashtable-hash-function h) (hashtable-equivalence-function h)))))`},
		{"hash-function is eq? to the caller's equal-hash",
			`(begin (import (scheme base)) (eq? (hashtable-hash-function (make-equal-hashtable)) equal-hash))`},
		{"equivalence-function is eq? to the caller's equal?",
			`(begin (import (scheme base)) (eq? (hashtable-equivalence-function (make-equal-hashtable)) equal?))`},
		{"eqv table's equivalence-function is eq? to the caller's eqv?",
			`(begin (import (scheme base)) (eq? (hashtable-equivalence-function (make-eqv-hashtable)) eqv?))`},
		{"eq table's equivalence-function is eq? to the caller's eq?",
			`(begin (import (scheme base)) (eq? (hashtable-equivalence-function (make-eq-hashtable)) eq?))`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			got := evalSRFI(c, newSRFITestEngine(t), tc.expr)
			c.Assert(got, qt.Equals, `#t`)
		})
	}
}

// TestHashtableAccessorsSurviveAUserShadow pins the SEALED-tier fallback, which
// is the branch reached only when the mutable tier holds something that is not
// this primitive.
//
// name only LOCATES a candidate; identity DECIDES. A user (define equal-hash …)
// wins the ranked probe from the runtime view and then fails the identity check,
// and the accessor must ask the sealed tiers directly rather than conclude the
// primitive is unregistered. Nothing else exercises that branch: without a
// shadow the mutable tier is either empty (so the probe falls through to the
// sealed slot on its own) or holds the real primitive after an import.
func TestHashtableAccessorsSurviveAUserShadow(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name string
		expr string
	}{
		{"shadowed equal-hash",
			`(begin (define equal-hash 42)
				(procedure? (hashtable-hash-function (make-equal-hashtable))))`},
		{"shadowed equal?",
			`(begin (define equal? 42)
				(procedure? (hashtable-equivalence-function (make-equal-hashtable))))`},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			got := evalSRFI(c, newSRFITestEngine(t), tc.expr)
			c.Assert(got, qt.Equals, `#t`)
		})
	}
}

// hashtableSecondCopyLibFS mints a SECOND copy of equal-hash reachable from
// Scheme under a name of its own.
//
// A library need not EXPORT equal-hash for a second copy to exist, which is the
// correction that makes the hash side of I106 pinnable at all. A library
// environment is a flat island — the factory re-applies the whole registry into
// it — so `(define my-hash equal-hash)` inside the library binds the island's
// own closure, and exporting THAT binding hands the program an object distinct
// from the sealed base's. The export list of (rnrs hashtables) is irrelevant.
//
// It works because equal-hash is AMBIENT. The same trick on equal? does NOT
// mint a second copy: a `(define my-equal equal?)` here would resolve through
// this library's own (import (scheme base)), and a library INSTANCE is shared
// across namespaces, so the program's (import (scheme base)) hands it the same
// object — measured `(eq? my-equal equal?)` => #t. The equivalence side needs a
// rename instead, which is what its own row uses.
func hashtableSecondCopyLibFS() fstest.MapFS {
	return fstest.MapFS{
		"hashlib.scm": &fstest.MapFile{Data: []byte(`(define-library (hashlib)
  (export my-hash)
  (import (scheme base))
  (begin
    (define my-hash equal-hash)))
`)},
	}
}

func newHashtableSecondCopyEngine(t testing.TB) *Engine {
	t.Helper()
	eng, err := NewEngine(context.Background(),
		WithProfile(KitchenSink),
		WithSourceFS(hashtableSecondCopyLibFS()),
		WithSourceFS(stdlib.FS),
		WithLibraryPaths("."))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// TestHashtableRemembersTheProceduresItWasMadeWith pins I106: the accessors hand
// back the procedure OBJECT the table was made with, and hashtable-copy keeps
// it.
//
// That is Chez's answer, not a conformance requirement — R6RS §13.3 says only
// "the equivalence function used by hashtable to compare keys" and never reaches
// object identity. The discriminating oracle is `petite -q --script` on a table
// built from USER procedures, `(define (my-eq a b) (equal? a b))`, which answers
// #t for the table and its copy on both accessors; the aliasing form
// `(define my-eq equal?)` answers #t under either semantics and so proves
// nothing. Wile defers user-supplied pairs (see PrimMakeHashtable's doc), so the
// oracle form is not runnable here and the rows below reach the same question
// through a second copy of a registered primitive instead.
//
// The name probe in setRecognizedPrimitive cannot answer this. It finds whichever
// copy the program can NAME, so the two agree only while the caller holds the
// primitive under its registered spelling; the rows below each arrange for the
// caller to hold a different copy, by rename or by library island.
//
// Both elements of every row are load-bearing. (*values.Hashtable).Copy is an
// explicit field list rather than a struct copy, so a field Copy forgets
// regresses to the probe on the copy while the first element still passes.
func TestHashtableRemembersTheProceduresItWasMadeWith(t *testing.T) {
	c := qt.New(t)

	cases := []struct {
		name    string
		expr    string
		want    string
		needLib bool
	}{
		// The equivalence gate: my-eq is the (scheme base) closure under a name
		// the probe will never look up.
		{name: "a renamed equivalence procedure survives the table and its copy",
			expr: `(begin (import (rnrs hashtables)
					(only (rename (scheme base) (equal? my-eq)) my-eq))
				(let ((h (make-hashtable equal-hash my-eq)))
					(list (eq? (hashtable-equivalence-function h) my-eq)
						(eq? (hashtable-equivalence-function (hashtable-copy h)) my-eq))))`,
			want: `(#t #t)`},
		// The hash gate, and the reason it needs a library: equal-hash is
		// ambient, so it cannot be renamed through an import. (hashlib)'s
		// my-hash is the island's own copy of it — a distinct object, which the
		// third element proves rather than assumes.
		{name: "a second copy of the hash procedure survives the table and its copy",
			expr: `(begin (import (hashlib) (rnrs hashtables) (scheme base))
				(let ((h (make-hashtable my-hash equal?)))
					(list (eq? (hashtable-hash-function h) my-hash)
						(eq? (hashtable-hash-function (hashtable-copy h)) my-hash)
						(eq? my-hash equal-hash))))`,
			want: `(#t #t #f)`, needLib: true},
		// Both sides at once — island copy for the hash, rename for the
		// equivalence — so neither accessor can be answering from the probe in
		// the same run. Through an immutable copy, since that is the shape
		// hashtable-copy's one-argument form produces.
		{name: "both procedures survive together, through a copy",
			expr: `(begin (import (hashlib) (rnrs hashtables)
					(only (rename (scheme base) (equal? my-eq)) my-eq))
				(let ((h (hashtable-copy (make-hashtable my-hash my-eq))))
					(list (eq? (hashtable-hash-function h) my-hash)
						(eq? (hashtable-equivalence-function h) my-eq)
						(hashtable-mutable? h))))`,
			want: `(#t #t #f)`, needLib: true},
		// The control: with (scheme base) imported plainly the probe and the
		// remembered value agree, and must keep agreeing.
		{name: "the plain-import control is unchanged",
			expr: `(begin (import (rnrs hashtables) (scheme base))
				(let ((h (make-hashtable equal-hash equal?)))
					(list (eq? (hashtable-equivalence-function h) equal?)
						(eq? (hashtable-equivalence-function (hashtable-copy h)) equal?)
						(eq? (hashtable-hash-function h) equal-hash))))`,
			want: `(#t #t #t)`},
		// A fixed-kind constructor takes no procedures, so there is nothing to
		// remember and the probe stays the only answer. This is the residual
		// I069 depends on, pinned so the fix cannot be mistaken for deleting it.
		{name: "a fixed-kind table still answers from the probe",
			expr: `(begin (import (rnrs hashtables) (scheme base))
				(let ((h (make-equal-hashtable)))
					(list (eq? (hashtable-equivalence-function h) equal?)
						(eq? (hashtable-hash-function h) equal-hash))))`,
			want: `(#t #t)`},
		// The pair a program reads off a table is a pair it can write back,
		// which is what the identity gate in rememberedEqualPair must not
		// break: a second copy carries the same PrimitiveIdentity.
		{name: "the remembered pair round-trips through make-hashtable",
			expr: `(begin (import (hashlib) (rnrs hashtables)
					(only (rename (scheme base) (equal? my-eq)) my-eq))
				(let* ((h (make-hashtable my-hash my-eq))
						(g (make-hashtable (hashtable-hash-function h)
								(hashtable-equivalence-function h))))
					(list (eq? (hashtable-hash-function g) my-hash)
						(eq? (hashtable-equivalence-function g) my-eq))))`,
			want: `(#t #t)`, needLib: true},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			// A FRESH engine per case: an import mutates the top level. Built
			// inside the branch — constructing one and discarding it would keep
			// it alive until the parent test's cleanup runs.
			newEngine := newSRFITestEngine
			if tc.needLib {
				newEngine = newHashtableSecondCopyEngine
			}
			got := evalSRFI(c, newEngine(c), tc.expr)
			c.Assert(got, qt.Equals, tc.want)
		})
	}
}

// TestHashtableAccessorsRefuseAnUnknownKind pins the refusal both accessors owe
// an out-of-range kind, which is reachable only from Go: values.NewHashtable is
// exported and validates nothing.
//
// equivIdentity is a switch rather than an array precisely so an unknown kind
// refuses explicitly instead of panicking (see its doc), and hashtable-hash-function
// owes the same refusal, where it previously answered #f for anything that was
// not the equal kind — a complement only accidentally equivalent to "eq or eqv".
//
// This pins the REFUSAL and not the accessors' check order. The order is
// unobservable, because a remembering table is always the equal kind: a table
// with an out-of-range kind comes from values.NewHashtable, which never sets the
// remembered slot. Swapping the two checks fails no test, measured — and saying
// otherwise here is a claim this file cannot back.
func TestHashtableAccessorsRefuseAnUnknownKind(t *testing.T) {
	c := qt.New(t)

	for _, name := range []string{"hashtable-equivalence-function", "hashtable-hash-function"} {
		c.Run(name, func(c *qt.C) {
			eng := newSRFITestEngine(t)
			err := eng.Define("odd-table", WrapValue(values.NewHashtable(values.HashtableKind(values.HashtableKindCount))))
			c.Assert(err, qt.IsNil)
			_, err = eng.EvalMultiple(context.Background(), "("+name+" odd-table)")
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrUnsupportedHashtableKind), qt.IsTrue,
				qt.Commentf("must match the sentinel with errors.Is, got %v", err))
		})
	}
}

// typedNilCallable is a POINTER type implementing values.Callable, so a nil of
// it can be stored in a table's remembered slot while `!= nil` on the interface
// still answers true. Nothing in Wile produces one; it exists to reach the
// hazard an embedder can reach.
type typedNilCallable struct{}

func (*typedNilCallable) SchemeString() string {
	return "#<typed-nil>"
}

func (*typedNilCallable) IsVoid() bool {
	return false
}

func (*typedNilCallable) EqualTo(values.Value) bool {
	return false
}

func (*typedNilCallable) AcceptsArity(int) bool {
	return true
}

// TestHashtableAccessorsRejectAnUnregisteredRememberedValue pins the gate in
// rememberedEqualPair, which is the only thing keeping the remembered slot from
// being a hole in the accessors' contract.
//
// values.NewEqualHashtableWithFunctions is exported and deliberately draws no
// conclusion about which procedures it is handed — recognition needs
// machine.PrimitiveIdentity, which pkg/values cannot see. Without the gate an
// embedder could make hashtable-equivalence-function answer a value the registry
// never registered, under its declared ReturnType: TypeProcedure, which nothing
// at runtime enforces.
//
// Three cases, and each pins a different half of the predicate:
//
//   - A TYPED NIL of a non-closure type. `remembered != nil` is an interface
//     test, which a typed nil passes, so a bare nil check would let it escape.
//   - A typed nil of *machine.ForeignClosure, the one type the slot is BUILT to
//     hold. This is the sharp case: IdentityOf asserts to that type and reads a
//     field off it, so before IdentityOf was made total this panicked inside the
//     gate rather than being rejected by it — reintroducing the opaque
//     ErrPanicRecovery that equivIdentity's switch exists to avoid.
//   - A REGISTERED primitive bearing the WRONG token: equal? in the hash slot
//     and equal-hash in the equivalence slot, the transposition an embedder
//     would actually make. Without this row the test cannot tell `== identity`
//     from the weaker `IdentityOf(...) != nil`, and weakening the gate that way
//     leaves every package green.
//
// The gate must fail OPEN to the name probe, not refuse: the table is still a
// perfectly good equal?-keyed table, and the probe's answer is the one a
// fixed-kind table would get.
func TestHashtableAccessorsRejectAnUnregisteredRememberedValue(t *testing.T) {
	c := qt.New(t)

	// The registry's own equal? and equal-hash closures, reached the way a
	// program reaches them, so the transposed row holds genuinely registered
	// primitives rather than look-alikes.
	registered := func(c *qt.C, eng *Engine, expr string) values.Callable {
		v, err := eng.EvalMultiple(context.Background(), expr)
		c.Assert(err, qt.IsNil)
		q, ok := v.Internal().(values.Callable)
		c.Assert(ok, qt.IsTrue)
		return q
	}

	cases := []struct {
		name string
		pair func(c *qt.C, eng *Engine) (values.Callable, values.Callable)
	}{
		{"a typed nil of a non-closure type",
			func(_ *qt.C, _ *Engine) (values.Callable, values.Callable) {
				return (*typedNilCallable)(nil), (*typedNilCallable)(nil)
			}},
		{"a typed nil of the closure type the slot holds",
			func(_ *qt.C, _ *Engine) (values.Callable, values.Callable) {
				return (*machine.ForeignClosure)(nil), (*machine.ForeignClosure)(nil)
			}},
		{"a procedure the registry never registered",
			func(_ *qt.C, _ *Engine) (values.Callable, values.Callable) {
				return &typedNilCallable{}, &typedNilCallable{}
			}},
		{"registered primitives in transposed slots",
			func(c *qt.C, eng *Engine) (values.Callable, values.Callable) {
				return registered(c, eng, "equal?"), registered(c, eng, "equal-hash")
			}},
		// One good member and one bad one: the gate is ATOMIC, so this must
		// answer from the probe on BOTH sides rather than mixing provenance.
		{"one registered member and one unregistered",
			func(c *qt.C, eng *Engine) (values.Callable, values.Callable) {
				return registered(c, eng, "equal-hash"), &typedNilCallable{}
			}},
	}
	for _, tc := range cases {
		c.Run(tc.name, func(c *qt.C) {
			eng := newSRFITestEngine(c)
			hashFn, equivFn := tc.pair(c, eng)
			ht := values.NewEqualHashtableWithFunctions(hashFn, equivFn)
			c.Assert(eng.Define("odd-table", WrapValue(ht)), qt.IsNil)
			got := evalSRFI(c, eng, `(list (eq? (hashtable-equivalence-function odd-table) equal?)
				(eq? (hashtable-hash-function odd-table) equal-hash))`)
			c.Assert(got, qt.Equals, `(#t #t)`)
		})
	}
}

// TestHashtableHalfRememberedPairIsNotRemembered pins that a HALF pair degrades
// to remembering nothing, at the constructor.
//
// pkg/values cannot ask whether a member is the registered primitive, so the
// claim it can hold is narrower: the two accessors answer NONE together. One
// nil member would break that on its own, before the registry's gate ever runs,
// and the two accessors would then read from different sources.
//
// Fail OPEN, like the gate: both accessors answer from the name probe, which is
// what a fixed-kind table gets.
func TestHashtableHalfRememberedPairIsNotRemembered(t *testing.T) {
	c := qt.New(t)

	eng := newSRFITestEngine(c)
	equalHash, err := eng.EvalMultiple(context.Background(), "equal-hash")
	c.Assert(err, qt.IsNil)
	hashFn, ok := equalHash.Internal().(values.Callable)
	c.Assert(ok, qt.IsTrue)

	for _, tc := range []struct {
		name            string
		hashFn, equivFn values.Callable
	}{
		{"a remembered hash with no equivalence", hashFn, nil},
		{"a remembered equivalence with no hash", nil, hashFn},
		{"neither", nil, nil},
	} {
		c.Run(tc.name, func(c *qt.C) {
			ht := values.NewEqualHashtableWithFunctions(tc.hashFn, tc.equivFn)
			// Asserted at the pkg/values layer too, because this is that
			// layer's claim: a half pair leaves the slot empty.
			c.Assert(ht.HashFunction(), qt.Equals, nil)
			c.Assert(ht.EquivalenceFunction(), qt.Equals, nil)

			probe := newSRFITestEngine(c)
			c.Assert(probe.Define("half-table", WrapValue(ht)), qt.IsNil)
			got := evalSRFI(c, probe, `(list (eq? (hashtable-equivalence-function half-table) equal?)
				(eq? (hashtable-hash-function half-table) equal-hash))`)
			c.Assert(got, qt.Equals, `(#t #t)`)
		})
	}
}
