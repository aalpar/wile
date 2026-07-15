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

package compilation

import (
	"fmt"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// Top-level binder hygiene (R7RS §4.3.2).
//
// Wile's global bindings are keyed by name, not by scope set (unlike locals,
// which are scope-aware; see environment.LocalEnvironmentFrame vs
// GlobalEnvironmentFrame, and the deliberate name-symbolic global arm of
// BindingRef). That asymmetry breaks hygiene for a macro-introduced TOP-LEVEL
// binder two ways:
//
//   - Collision: two expansions of a macro that introduces a top-level define
//     (e.g. a helper temporary) both key the same global name, so the second
//     redefine is rejected under the immutable top level and, even when allowed,
//     the two expansions alias one binding. This blocked a set!-free
//     define-values.
//   - Leak: a macro-introduced top-level binding resolves by bare name at the
//     use site, so (define-syntax m (syntax-rules () ((_) (define x 0)))) (m) x
//     wrongly sees the macro's x. R7RS §4.3 requires it to be hidden.
//
// The fix restores hygiene the way a name-keyed global namespace must: give each
// macro-introduced top-level (define …) binder a fresh unique name and rewrite
// every reference that resolves to it. A binder is "macro-introduced" iff it
// carries a non-empty scope set — a user-written top-level binder, and a binder
// that came from a pattern variable (the user's own identifier, e.g.
// define-record-type's type/constructor/accessor names), both carry empty scopes
// at the top level and are left untouched. Only genuinely template-introduced
// identifiers (which the expander stamped with an intro scope) are renamed.
//
// The fresh name must be UNGUESSABLE: it enters the same name-keyed global layer
// as user code, so a predictable spelling (e.g. "tmp.1") is a legal identifier a
// program could collide with by writing it, which reopens the collision and
// makes the "hidden from the use site" guarantee false. It is therefore built
// from values.NewTemporaryVariableName (128 bits of crypto randomness) — a
// reader-legal but not feasibly guessable spelling — the codebase's
// disjoint-by-construction temporary-name mechanism.
//
// The pass runs between expansion and compilation (ExpandTopLevelExpression), on
// the fully-expanded top-level form. It is a no-op for any form that is not a
// top-level define / begin-of-defines (an expression, a lambda transformer
// body), so applying it unconditionally is safe.
//
// Scope: only (define …) value/function binders are renamed. A macro-introduced
// top-level (define-syntax …) is intentionally left alone — a macro-introduced
// top-level macro is resolved across phases by GetGlobalIndexAcrossPhases (the
// macro-generating-macro path), and renaming it here would cut that.

// introducedBinder is one macro-introduced top-level define binder: its original
// name, the scope set it was introduced with, and the fresh unique name assigned
// to it. scopes aliases the binder symbol's live backing slice (SyntaxSymbol.Scopes
// returns it directly); safe because syntax objects are immutable, though the
// struct outlives the collecting walk.
type introducedBinder struct {
	key    string
	scopes []*syntax.Scope
	fresh  string
}

// renameMacroIntroducedTopLevelBinders gives every macro-introduced top-level
// define binder in stx a fresh unique name and rewrites the references that
// resolve to it. Returns stx unchanged when there are none (the common case:
// expressions, user defines, transformer bodies).
func renameMacroIntroducedTopLevelBinders(stx syntax.SyntaxValue) syntax.SyntaxValue {
	binders := collectTopLevelIntroducedBinders(stx, nil)
	if len(binders) == 0 {
		return stx
	}
	return renameReferences(stx, binders, 0)
}

// collectTopLevelIntroducedBinders walks the top-level structure of stx —
// descending through top-level begin splicing only, never into a lambda/let or
// any other binding form — and records each (define <id> ...) whose binder is
// macro-introduced (carries a non-empty scope set). Local (internal) defines are
// left to the scope-aware local environment, which already handles them.
func collectTopLevelIntroducedBinders(stx syntax.SyntaxValue, acc []introducedBinder) []introducedBinder {
	beginForm, ok := asSyntaxFormWithKeyword(stx, "begin")
	if ok {
		rest := beginForm.SyntaxCdr()
		for {
			restPair, ok := rest.(*syntax.SyntaxPair)
			if !ok || syntax.IsSyntaxEmptyList(restPair) {
				break
			}
			acc = collectTopLevelIntroducedBinders(restPair.SyntaxCar(), acc)
			rest = restPair.SyntaxCdr()
		}
		return acc
	}
	// extractDefineName matches (define name …) / (define (name . args) …) and
	// returns nil otherwise, so it doubles as the "is this a define" test. A
	// future valid-but-unrecognized define shape returning nil here would be a
	// silent hygiene hole — extractDefineName must recognize every define shape
	// the validator accepts.
	binder := extractDefineName(stx)
	if binder != nil && len(binder.Scopes()) > 0 {
		acc = appendBinder(acc, binder)
	}
	return acc
}

// appendBinder records sym as a macro-introduced binder with a fresh unique
// name, deduplicating against a binder already collected with the same name and
// the same scope set (so one binder reached twice shares one fresh name).
func appendBinder(acc []introducedBinder, sym *syntax.SyntaxSymbol) []introducedBinder {
	key := sym.Key()
	scopes := sym.Scopes()
	for i := range acc {
		if acc[i].key == key && sameScopeSet(acc[i].scopes, scopes) {
			return acc
		}
	}
	// key prefix for debuggability; the crypto-random tail is the actual
	// uniqueness-and-unforgeability guarantee (see the file header).
	fresh := fmt.Sprintf("%s.%s", key, values.NewTemporaryVariableName().Key)
	return append(acc, introducedBinder{key: key, scopes: scopes, fresh: fresh})
}

// renameReferences rewrites each reference to a collected binder's fresh name,
// but never a symbol in DATUM position: inside (quote …) nothing is renamed, and
// inside (quasiquote …) only the evaluated parts reached through a matching
// (unquote …)/(unquote-splicing …) are. A quoted symbol is data, not an
// identifier (R7RS §4.1.2); a literal vector's elements are likewise data. quasi
// is the quasiquote nesting depth (0 = an ordinary evaluated position). A blanket
// walk over every symbol would rewrite quoted data and silently corrupt it.
func renameReferences(stx syntax.SyntaxValue, binders []introducedBinder, quasi int) syntax.SyntaxValue {
	switch s := stx.(type) {
	case *syntax.SyntaxSymbol:
		if quasi > 0 {
			return s
		}
		fresh, ok := maximalBinderMatch(s, binders)
		if !ok {
			return s
		}
		// Preserve the original source context (scopes + location); only the
		// name changes. Every occurrence maps through the same binder, so the
		// binder and its references stay linked under the fresh name.
		return syntax.NewSyntaxSymbolForSymbol(values.NewSymbol(fresh), s.SourceContext())
	case *syntax.SyntaxPair:
		return renamePair(s, binders, quasi)
	case *syntax.SyntaxVector:
		if quasi == 0 {
			// A literal vector: its elements are data, not references.
			return s
		}
		return renameVectorElements(s, binders, quasi)
	default:
		return stx
	}
}

// renamePair handles quote/quasiquote/unquote specially, then recurses.
func renamePair(p *syntax.SyntaxPair, binders []introducedBinder, quasi int) syntax.SyntaxValue {
	if syntax.IsSyntaxEmptyList(p) {
		return p
	}
	// Keywords are recognized by name. That is sound for a fully-expanded
	// top-level form, whose surviving quote/quasiquote/unquote/begin/define are
	// the core forms — EXCEPT the pathological case of a program that lexically
	// shadows one of these names and passes a renamed macro temp through it (e.g.
	// (let ((quote (lambda (x) x))) (quote tmp))); the pass would misread the
	// shadowed head as the special form. Detecting that needs the binding
	// environment, which this post-expansion syntax pass does not carry; it is
	// left as a known limitation, consistent with extractDefineName/
	// asSyntaxFormWithKeyword, which also match define/begin by name.
	head, ok := p.SyntaxCar().(*syntax.SyntaxSymbol)
	if ok {
		switch head.Key() {
		case "quote":
			if quasi == 0 {
				// A real quote form: pure datum, nothing inside is an identifier.
				return p
			}
			// Inside a quasiquote, `quote` is an ordinary datum symbol whose
			// nested unquotes stay live (R7RS §4.2.6). Fall through to recurse at
			// the same depth — matching the compiler's own quasiquote walk, which
			// does not special-case quote.
		case "quasiquote":
			return renameArgsKeepHead(p, binders, quasi+1)
		case "unquote", "unquote-splicing":
			if quasi > 0 {
				return renameArgsKeepHead(p, binders, quasi-1)
			}
		}
	}
	newCar := renameReferences(p.SyntaxCar(), binders, quasi)
	newCdr := renameReferences(p.SyntaxCdr(), binders, quasi)
	if newCar == p.SyntaxCar() && newCdr == p.SyntaxCdr() {
		return p
	}
	return syntax.NewSyntaxCons(newCar, newCdr, p.SourceContext())
}

// renameArgsKeepHead recurses into a form's arguments (the cdr) at the given
// quasi depth, leaving the operator keyword (quote/quasiquote/unquote) unchanged.
func renameArgsKeepHead(p *syntax.SyntaxPair, binders []introducedBinder, quasi int) syntax.SyntaxValue {
	newCdr := renameReferences(p.SyntaxCdr(), binders, quasi)
	if newCdr == p.SyntaxCdr() {
		return p
	}
	return syntax.NewSyntaxCons(p.SyntaxCar(), newCdr, p.SourceContext())
}

// renameVectorElements rewrites references inside a vector that sits within a
// quasiquote (where its elements may carry unquotes); a depth-0 literal vector is
// handled as data by the caller and never reaches here.
func renameVectorElements(v *syntax.SyntaxVector, binders []introducedBinder, quasi int) syntax.SyntaxValue {
	changed := false
	newVals := make([]syntax.SyntaxValue, len(v.Values))
	for i, elem := range v.Values {
		newVals[i] = renameReferences(elem, binders, quasi)
		if newVals[i] != elem {
			changed = true
		}
	}
	if !changed {
		return v
	}
	return syntax.NewSyntaxVector(v.SourceContext(), newVals...)
}

// maximalBinderMatch returns the fresh name of the binder that sym resolves to,
// applying Flatt's maximal (most-specific) rule: among binders with the same
// name whose scope set is a subset of sym's, the one with the largest scope set
// wins. This approximates environment.GetLocalIndex's maximal resolution so a
// reference nested under an inner binding form (which carries extra scopes) still
// resolves to its binder. An equal-cardinality tie — two same-name binders with
// equal-size but distinct scope sets both ⊆ sym — is a Flatt-model ambiguity
// (a program error either way); here it resolves to the first collected. The
// same-scope-set dedup does NOT rule this out (it keys on exact equality, not
// cardinality), but it needs two distinct macro-introduced top-level binders
// sharing a name with incomparable scopes visible to one reference — not
// reachable via define-values or any realistic macro.
func maximalBinderMatch(sym *syntax.SyntaxSymbol, binders []introducedBinder) (string, bool) {
	key := sym.Key()
	symScopes := sym.Scopes()
	bestFresh := ""
	bestCount := -1
	for i := range binders {
		if binders[i].key != key {
			continue
		}
		// binders[i].scopes ⊆ symScopes
		if !syntax.ScopesMatch(symScopes, binders[i].scopes) {
			continue
		}
		if len(binders[i].scopes) > bestCount {
			bestCount = len(binders[i].scopes)
			bestFresh = binders[i].fresh
		}
	}
	if bestCount < 0 {
		return "", false
	}
	return bestFresh, true
}

// sameScopeSet reports whether two scope sets are equal (mutual subset). Set
// equality is exact only on duplicate-free slices; scope sets are kept
// duplicate-free codebase-wide (AddScopeToSet dedups).
func sameScopeSet(a, b []*syntax.Scope) bool {
	return syntax.ScopesMatch(a, b) && syntax.ScopesMatch(b, a)
}
