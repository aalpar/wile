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
	"slices"
	"strings"

	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// CompilerFunc is the uniform codegen signature for both Tier-1 and Tier-2 forms,
// dispatched by FormName through the per-engine forms registry. Its parameter is
// the ValidatedExpr interface (not a concrete type) so one type covers every form;
// each handler asserts its concrete type at its head.
type CompilerFunc func(ctc *CompileTimeContinuation, ctctx CompileTimeCallContext, expr forms.ValidatedExpr) error

type dispatchKind int

const (
	// dispatchExpandOnly marks a form handled entirely during expansion that
	// legitimately has no compiler; it never reaches the compilation phase.
	dispatchExpandOnly dispatchKind = iota
)

// formDispatch classifies the forms that legitimately have NO compiler — the
// expand-only macros. Post-SP1 every other form carries spec.Compile, so this
// table shrank from the former Tier-1 + expand-only mirror to expand-only alone.
var formDispatch = map[string]dispatchKind{
	"let-syntax":           dispatchExpandOnly,
	"letrec-syntax":        dispatchExpandOnly,
	TransformerSyntaxRules: dispatchExpandOnly,
}

// VerifyCompilers checks that every registered form either carries a valid
// CompilerFunc on its FormSpec or is classified expand-only in formDispatch. The
// type-checked assertion catches a mis-typed Compile (any → non-CompilerFunc),
// which compilerFor would otherwise swallow to a nil "no compiler" diagnostic.
func VerifyCompilers() error {
	var missing []string
	reg := forms.DefaultRegistry()
	for _, name := range reg.Names() {
		spec := reg.Lookup(name)
		if spec.Compile != nil {
			_, ok := spec.Compile.(CompilerFunc)
			if !ok {
				missing = append(missing, name+": Compile is not a CompilerFunc")
			}
			continue
		}
		_, classified := formDispatch[name]
		if classified {
			continue
		}
		missing = append(missing, name+": no compiler and no dispatch classification")
	}
	return formatMissing("compiler", missing)
}

// VerifyExpanders checks that every Tier 2 syntax compiler entry has a
// corresponding primitive expander entry. A Tier 2 form without an expander
// is silently treated as a procedure call during expansion — the most
// dangerous form of registration drift.
func VerifyExpanders() error {
	expanderNames := values.NewStringSet(len(primitiveExpanderEntries))
	for _, e := range primitiveExpanderEntries {
		expanderNames.Add(e.Name)
	}

	var missing []string
	for _, e := range syntaxCompilerEntries {
		has := expanderNames.ContainsOne(e.Name)
		if !has {
			missing = append(missing, e.Name+": syntax compiler has no expander")
		}
	}
	return formatMissing("expander", missing)
}

func formatMissing(kind string, missing []string) error {
	if len(missing) == 0 {
		return nil
	}
	slices.Sort(missing)
	var b strings.Builder
	for _, m := range missing {
		b.WriteString("\n  ")
		b.WriteString(m)
	}
	return werr.WrapForeignErrorf(werr.ErrInvalidArgument,
		"%s registration inconsistencies:%s", kind, b.String())
}
