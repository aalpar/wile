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
// were widened in P6. map/fold-right are non-tail and deferred.
var inlineHOFSpecs = map[string]inlineHOFSpec{
	"for-each": {callbackParam: 0, template: `(lambda (f lst)
  (let loop ((lst lst))
    (if (null? lst) (if #f #f)
        (begin (f (car lst)) (loop (cdr lst))))))`},
	"vector-map": {callbackParam: 0, template: `(lambda (f v)
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
	"string-map": {callbackParam: 0, template: `(lambda (f s)
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
		b := frame.GetBinding(values.NewSymbol(name), nil)
		if b != nil {
			applyInlineHOFStamp(b, spec.callbackParam)
		}
	}
}
