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
	"context"
	"errors"
	"io"
	"strings"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/werr"
)

// LoadBootstrapSources runs the canonical bootstrap pipeline — parse → expand → compile →
// optimize → execute — for each source string against env, using the default inline and
// expand-depth thresholds. Templates ARE optimized and executed on a pooled top-level
// context, matching production engine initialization. The kind argument ("macro" /
// "procedure") names the source category in error context.
//
// This is the single implementation shared by the engine-root (pkg/wile) and internal
// (internal/bootstrap) bootstrap paths. Each previously kept its own copy of this loop,
// and the copies had drifted: the internal one ran on a raw, unpooled context and did
// NOT optimize, so bootstrap procedures loaded via the internal API (test helpers,
// embedders on the internal API) ran unoptimized while the public NewEngine path
// optimized them. Consolidating here removes that divergence.
func LoadBootstrapSources(ctx context.Context, env *environment.EnvironmentFrame, sources []string, resolver FileResolver, kind string) error {
	for _, source := range sources {
		reader := strings.NewReader(source)
		pr := parser.NewParser(env, true, reader)
		for {
			stx, err := pr.ReadSyntax(ctx)
			if errors.Is(err, io.EOF) {
				break
			}
			if err != nil {
				return werr.WrapForeignErrorf(err, "error parsing bootstrap %s", kind)
			}
			err = runBootstrapStx(ctx, env, stx, resolver, kind)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

// runBootstrapStx expands, compiles, optimizes, and runs a single bootstrap form on a
// pooled top-level context. The context is released per form (a deferred release in this
// per-iteration helper, not at the end of LoadBootstrapSources's loop).
func runBootstrapStx(ctx context.Context, env *environment.EnvironmentFrame, stx syntax.SyntaxValue, resolver FileResolver, kind string) error {
	tpl, err := ExpandAndCompile(ctx, env, stx, resolver, DefaultInlineThreshold, DefaultMaxExpandDepth)
	if err != nil {
		return werr.WrapForeignErrorf(err, "error expanding/compiling bootstrap %s", kind)
	}
	tpl.Optimize()

	mc := machine.AcquireTopLevelContext(ctx, tpl, env)
	defer machine.ReleaseTopLevelContext(mc)
	err = mc.Run()
	if err != nil {
		return werr.WrapForeignErrorf(err, "error running bootstrap %s", kind)
	}
	return nil
}
