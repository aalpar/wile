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
	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// inlineHOFSpec curates one tail higher-order procedure for callback
// specialization (Strategy A): the procedure whose single-sequence tail loop may
// be inlined at a call site that independently proves the callback capture-safe,
// so the inlined loop reclaims its env frame.
type inlineHOFSpec struct {
	// callbackParam is the index, in template's parameter list, of the callback
	// argument — the parameter tryInlineHOFCall stamps CaptureSafe. The stamp
	// records this index on the HOF binding (BindingMeta.InlineHOFCallbackParam);
	// the dispatch reads it back. BuildInlineHOFTemplates asserts it is a valid
	// parameter index, so a mis-authored template cannot stamp the wrong argument.
	callbackParam int
	// importGated distinguishes the two load paths, which have different soundness
	// boundaries:
	//   - false (sealed-base): the procedure lives in the sealed base (for-each,
	//     vector-map, vector-for-each, string-map, string-for-each). StampInlineHOFs
	//     sweeps that frame post-bootstrap. The sealed base holds only system
	//     definitions, so a name match there is always the real curated HOF.
	//   - true (import-gated): the procedure is reachable only by importing its
	//     library (fold, srfi/1). stampImportedInlineHOF stamps the per-import
	//     target binding by export name. CRITICAL: only import-GATED specs are
	//     stamped on the import path. A sealed-base name re-exported by an unrelated
	//     library (e.g. SRFI-13's string-map, which differs from R7RS string-map)
	//     must NOT be stamped — its binding is a different procedure, and inlining
	//     this template for it would be unsound. So the import path stamps fold and
	//     nothing else; the sealed-base names are stamped only at their real home.
	importGated bool
	// homeLib is the LibraryName.Key() of the library that actually DEFINES this
	// import-gated HOF (e.g. "srfi/1" for fold). The import path stamps the
	// template ONLY when the binding is imported from this exact library — the
	// identity gate. A different library exporting a same-named procedure with
	// different semantics (sourceLib != homeLib) is never stamped, so its own code
	// runs. Empty for sealed-base specs (importGated false), which are stamped at
	// their real home by StampInlineHOFs and need no source check.
	//
	// Soundness vs completeness: this gate is sound (never mis-inlines) but
	// slightly incomplete — a re-export chain ((import (srfi 1)) then (export
	// fold) from library B) presents sourceLib=B, so the REAL srfi-1 fold imported
	// through B is no longer inlined. That is a safe deoptimization (correct
	// result, lost optimization), strictly better than silent miscompilation.
	homeLib string
	// requires names the primitives the template's body calls that a dialect might
	// remove (e.g. vector-map's template fills the result with vector-set!). The
	// inline optimization applies only when every one of them is bound in the engine;
	// a dialect that removes one (NoMutation drops vector-set!/string-set!) leaves the
	// HOF un-stamped, so its call falls back to the real procedure — which such a
	// dialect swaps for a mutation-free definition. Empty for templates that depend on
	// no removable primitive (for-each, map, the for-each index loops). Without this
	// gate a removed primitive would surface as an unbound reference at every inlined
	// call site rather than a clean deoptimization.
	requires []string
	// template is the procedure's single-sequence reclaiming loop as a lambda
	// whose callbackParam-th parameter is the callback. Transcribed from the real
	// definition (pkg/registry/core/bootstrap_procedures.scm for the sealed-base
	// HOFs; the (null? lists) then-branch of pkg/stdlib/lib/srfi/1/fold.scm for
	// fold). Built once per Namespace through the real expander + validator
	// (BuildInlineHOFTemplates), so its free identifiers carry definition-env
	// hygiene and resolve to the sealed-base globals even when a call site shadows
	// them locally. Per-HOF correctness tests (against known-correct results, plus
	// the multi-arg fall-through and empty/single boundaries) guard transcription
	// drift; per-HOF hygiene tests guard the free-identifier resolution.
	template string
}

// inlineHOFSpecs is the SINGLE source of truth for the curated tail HOFs: each
// maps a procedure name to its callback index, load path, and inline template.
// The curation is deliberate, NOT auto-derived. Consumed by the stamp seams
// (StampInlineHOFs / stampImportedInlineHOF) and the template builder
// (BuildInlineHOFTemplates) — one map keeps the callback index physically
// adjacent to the template it indexes, so the two cannot drift apart.
//
// v1 = for-each (P3); the vector/string index loops and fold's arity-3 list fold
// were widened in P6. map/fold-right are non-tail in their real definitions, so
// their templates are TAIL REWRITES (accumulate + reverse): sound here because a
// template is reached only when the callback is provably capture-safe, so the
// call/cc-capturability that map/fold-right's non-tail shapes preserve
// (bootstrap_procedures.scm / srfi/1/fold.scm) is moot — a capturing callback
// falls through to the real, non-tail HOF. The rewrite also sheds the original's
// O(n) call-depth ceiling. Application order is preserved (verified): map applies
// f left-to-right; fold-right reverses first so kons still fires right-to-left.
var inlineHOFSpecs = map[string]inlineHOFSpec{
	"for-each": {callbackParam: 0, template: `(lambda (f lst)
  (let loop ((lst lst))
    (if (null? lst) (if #f #f)
        (begin (f (car lst)) (loop (cdr lst))))))`},
	// map's real single-list clause conses in non-tail position
	// ((cons (f (car lst)) (loop (cdr lst)))); the tail rewrite accumulates the
	// mapped results front-to-back and reverses once at the end, so f is still
	// applied left-to-right and the result order is unchanged.
	"map": {callbackParam: 0, template: `(lambda (f lst)
  (let loop ((lst lst) (acc '()))
    (if (null? lst) (reverse acc)
        (loop (cdr lst) (cons (f (car lst)) acc)))))`},
	"vector-map": {callbackParam: 0, requires: []string{"vector-set!"}, template: `(lambda (f v)
  (let ((len (vector-length v)))
    (let ((result (make-vector len)))
      (let loop ((i 0))
        (if (< i len)
            (begin
              (vector-set! result i (f (vector-ref v i)))
              (loop (+ i 1)))
            result)))))`},
	"vector-for-each": {callbackParam: 0, template: `(lambda (f v)
  (let ((len (vector-length v)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (f (vector-ref v i))
            (loop (+ i 1)))))))`},
	"string-map": {callbackParam: 0, requires: []string{"string-set!"}, template: `(lambda (f s)
  (let ((len (string-length s)))
    (let ((result (make-string len)))
      (let loop ((i 0))
        (if (< i len)
            (begin
              (string-set! result i (f (string-ref s i)))
              (loop (+ i 1)))
            result)))))`},
	"string-for-each": {callbackParam: 0, template: `(lambda (f s)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (f (string-ref s i))
            (loop (+ i 1)))))))`},
	"fold": {callbackParam: 0, importGated: true, homeLib: "srfi/1", template: `(lambda (kons knil ls)
  (let lp ((ls ls) (acc knil))
    (if (pair? ls) (lp (cdr ls) (kons (car ls) acc)) acc)))`},
	// fold-right's real single-list clause recurses in non-tail position
	// ((kons (car ls) (lp (cdr ls)))), applying kons innermost-first
	// (right-to-left). The tail rewrite reverses ls once, then folds left with
	// the accumulator — visiting elements in the same right-to-left kons order,
	// so both the result and the application order match the real fold-right.
	"fold-right": {callbackParam: 0, importGated: true, homeLib: "srfi/1", template: `(lambda (kons knil ls)
  (let lp ((ls (reverse ls)) (acc knil))
    (if (pair? ls) (lp (cdr ls) (kons (car ls) acc)) acc)))`},
}

// applyInlineHOFStamp records the inline-HOF capability on b. Callers guarantee a
// non-nil b and own the soundness decision (which bindings are eligible); this is
// just the shared write.
func applyInlineHOFStamp(b *environment.Binding, callbackParam int) {
	m := b.EnsureMeta()
	m.InlineHOF = true
	m.InlineHOFCallbackParam = callbackParam
}

// stampImportedInlineHOF marks the import target b when name is a curated
// IMPORT-GATED tail HOF (importGated true — currently only fold) imported from
// the library that actually defines it. A non-curated name, a sealed-base name,
// or an import from a DIFFERENT library is a no-op. This is the soundness
// boundary for the import path on two axes:
//   - name: a re-export of a sealed-base name (e.g. SRFI-13's string-map, a
//     different procedure from R7RS string-map) is NOT import-gated, so it is
//     never stamped here. The sealed-base HOFs are stamped only at their real
//     home (StampInlineHOFs).
//   - identity (sourceLib vs spec.homeLib): a library exporting its own `fold`
//     with non-SRFI-1 semantics presents sourceLib != "srfi/1" and is not
//     stamped, so the user's code runs instead of the SRFI-1 inline template.
func stampImportedInlineHOF(b *environment.Binding, name string, sourceLib LibraryName) {
	spec, ok := inlineHOFSpecs[name]
	if !ok || !spec.importGated {
		return
	}
	// Identity gate: only the library that defines this HOF may stamp it.
	if sourceLib.Key() != spec.homeLib {
		return
	}
	applyInlineHOFStamp(b, spec.callbackParam)
}

// StampInlineHOFs sweeps frame's own bindings, stamping every curated SEALED-BASE
// tail HOF bound there. Called post-bootstrap on the sealed base. Import-gated
// entries (fold) are skipped here and stamped on their import path instead.
func StampInlineHOFs(frame *environment.EnvironmentFrame) {
	for name, spec := range inlineHOFSpecs {
		if spec.importGated {
			continue
		}
		if !inlineHOFRequirementsMet(frame, spec) {
			continue
		}
		b := frame.GetBinding(values.NewSymbol(name), nil)
		if b != nil {
			applyInlineHOFStamp(b, spec.callbackParam)
		}
	}
}

// inlineHOFRequirementsMet reports whether every primitive spec.template depends on
// (spec.requires) is bound in frame. A dialect that removed one (e.g. NoMutation
// drops vector-set!) fails this check, so the HOF is left un-stamped and its call
// sites use the real procedure instead of the now-invalid inline template — a clean
// deoptimization rather than an unbound-reference compile error.
func inlineHOFRequirementsMet(frame *environment.EnvironmentFrame, spec inlineHOFSpec) bool {
	for _, name := range spec.requires {
		if frame.GetBinding(values.NewSymbol(name), nil) == nil {
			return false
		}
	}
	return true
}
