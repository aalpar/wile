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

// inlineHOFCallbackParam maps each curated tail higher-order procedure to the
// parameter index of its callback. These are the procedures whose single-list
// tail loop may be inlined at a call site that independently proves the callback
// capture-safe (callback specialization Strategy A), so the inlined loop reclaims
// its env frame. This is the single source of truth for the curated set, consumed
// by the two sound stamp seams below; the curation is deliberate, NOT auto-derived.
//
// Two load paths reach these procedures, each with its own soundness boundary:
//   - Bootstrap-resident (for-each, vector-map, vector-for-each, string-map,
//     string-for-each) live in the sealed base; StampInlineHOFs sweeps that frame
//     post-bootstrap. The sealed base holds only system definitions, never user
//     code, so a name match there is always the real curated HOF.
//   - Import-gated (fold, srfi/1) is reached only by importing the library;
//     stampImportedInlineHOF stamps the per-import target binding by export name.
//     Only genuine library exports flow through the import path, so a user's own
//     (define (fold …)) — which lands in the mutable runtime, not an import — is
//     never stamped. That is the soundness boundary: a by-name stamp here is safe
//     precisely because it fires only on real exports.
var inlineHOFCallbackParam = map[string]int{
	"for-each":        0,
	"vector-map":      0,
	"vector-for-each": 0,
	"string-map":      0,
	"string-for-each": 0,
	"fold":            0,
}

// stampInlineHOF marks b with the inline-HOF capability when name is a curated
// tail HOF. Idempotent and skip-if-absent: a nil b or a non-curated name is a
// no-op. The caller is responsible for soundness — it must only pass bindings that
// are genuinely the curated procedure (a sealed-base binding or a library export),
// never a user redefinition.
func stampInlineHOF(b *environment.Binding, name string) {
	if b == nil {
		return
	}
	idx, ok := inlineHOFCallbackParam[name]
	if !ok {
		return
	}
	m := b.EnsureMeta()
	m.InlineHOF = true
	m.InlineHOFCallbackParam = idx
}

// StampInlineHOFs sweeps frame's own bindings, stamping every curated tail HOF
// bound there with the InlineHOF capability. Called post-bootstrap on the sealed
// base to mark the bootstrap-resident HOFs; import-gated entries (fold) are simply
// absent from frame and skipped, then stamped on their import path instead.
func StampInlineHOFs(frame *environment.EnvironmentFrame) {
	for name := range inlineHOFCallbackParam {
		stampInlineHOF(frame.GetBinding(values.NewSymbol(name), nil), name)
	}
}
