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
	"strings"

	"github.com/aalpar/wile/pkg/werr"
)

// rejectNamespaceConsumedOptions panics when an option only bootstrapNamespace
// can apply was passed alongside WithNamespace.
//
// The WithNamespace path skips bootstrapNamespace entirely, so these options
// have nowhere to land. Before this check they were dropped in silence, and
// the authorizer family made that a security defect rather than a nuisance:
// NewEngine(WithNamespace(ns), WithSandbox()) returned a working engine with
// no sandbox and no error, so a caller who asked to be confined was not.
//
// Why a panic and not an error return: this is a construction-time programmer
// error, not a runtime condition to probe for. A caller cannot recover from
// "you passed the option to the wrong constructor" by branching — the fix is
// to move the option to NewNamespace, which consumes all of them. The panic is
// deliberately not catchable via errors.Is.
//
// Why the whole family and not just the security-relevant members: two rules
// ("some namespace options are fatal, others are quietly dropped") is the
// asymmetry this exists to remove. One rule — namespace options go to
// NewNamespace — is stateable in a sentence.
//
// ADDING A NEW OPTION: if it writes an engineConfig field that only
// bootstrapNamespace reads, add a row here. Do NOT add a row for a field
// NewEngine reads after the namespace exists (library paths, call depth,
// coverage) — those apply equally to a pre-built namespace.
// TestWithNamespaceRejectionCoversEveryConsumedField fails when engineConfig
// grows a field that is neither classified nor covered.
func rejectNamespaceConsumedOptions(cfg *engineConfig) {
	// Several options share one config field and are indistinguishable
	// afterwards, so a row names every option that could have written it.
	// Naming both beats guessing: the caller knows which one they wrote.
	rows := []struct {
		set     bool
		options string
	}{
		{cfg.registry != nil, "WithRegistry / WithoutCore"},
		{len(cfg.extensions) > 0, "WithExtension / WithExtensions"},
		{cfg.profileSet, "WithProfile"},
		{cfg.explicitAuthorizerSet, "WithAuthorizer"},
		{cfg.sandboxAuthorizer != nil, "WithSandbox"},
		{cfg.envMapSet, "WithEnv / WithEnvMap"},
		{cfg.topLevelMutabilitySet, "WithImmutableTopLevel / WithMutableTopLevel"},
		{cfg.strictLevel != strictLevelOff, "WithStrictNamespace / WithoutAmbientBindings"},
		{cfg.dialect != nil, "WithDialect"},
	}

	var offenders []string
	for _, row := range rows {
		if row.set {
			offenders = append(offenders, row.options)
		}
	}
	if len(offenders) == 0 {
		return
	}

	panic(werr.WrapForeignErrorf(werr.ErrEngineInit,
		"NewEngine: %s cannot be combined with WithNamespace — the pre-built-namespace path "+
			"skips the bootstrap step that applies them, so they would take no effect. "+
			"Pass them to NewNamespace instead and give its result to WithNamespace",
		strings.Join(offenders, ", ")))
}
