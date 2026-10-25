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

package wile_test

import (
	"context"
	"errors"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine/compilation"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// compileBound is one of the two compile bounds that live on the namespace.
type compileBound struct {
	name string
	opt  func(int) wile.EngineOnlyOption
	read func(*environment.Namespace) (int, bool)
	def  int
}

func compileBounds() []compileBound {
	return []compileBound{
		{
			name: "WithMaxExpandDepth",
			opt:  wile.WithMaxExpandDepth,
			read: func(ns *environment.Namespace) (int, bool) {
				return ns.MaxExpandDepth()
			},
			def: compilation.DefaultMaxExpandDepth,
		},
		{
			name: "WithInlineThreshold",
			opt:  wile.WithInlineThreshold,
			read: func(ns *environment.Namespace) (int, bool) {
				return ns.InlineThreshold()
			},
			def: compilation.DefaultInlineThreshold,
		},
	}
}

// Pin: a compile bound belongs to the namespace. The first engine to create or
// join it fixes the bound (its explicit value, else the default); a later engine
// inherits it without the option and is refused when it asks for a different
// value, since every engine on the namespace compiles at it.
func TestCompileBoundsBelongToTheNamespace(t *testing.T) {
	ctx := context.Background()
	newConfigured := func(t *testing.T, b compileBound, n int) *environment.Namespace {
		eng, err := wile.NewEngine(ctx, b.opt(n))
		qt.Assert(t, err, qt.IsNil)
		return eng.Namespace()
	}
	newUnconfigured := func(t *testing.T, _ compileBound, _ int) *environment.Namespace {
		ns, err := wile.NewNamespace(ctx)
		qt.Assert(t, err, qt.IsNil)
		return ns
	}
	newJoinedAtDefault := func(t *testing.T, _ compileBound, _ int) *environment.Namespace {
		ns := newUnconfigured(t, compileBound{}, 0)
		_, err := wile.NewEngineWithNamespace(ctx, ns)
		qt.Assert(t, err, qt.IsNil)
		return ns
	}
	tcs := []struct {
		name    string
		ns      func(t *testing.T, b compileBound, n int) *environment.Namespace
		join    func(b compileBound) []wile.EngineOnlyOption
		wantErr bool
		want    func(b compileBound) (int, bool)
	}{
		{
			name: "create: NewEngine sets the explicit value",
			ns:   newConfigured,
			want: func(compileBound) (int, bool) {
				return 7, true
			},
		},
		{
			name: "create: NewEngine sets the default without the option",
			ns: func(t *testing.T, _ compileBound, _ int) *environment.Namespace {
				eng, err := wile.NewEngine(ctx)
				qt.Assert(t, err, qt.IsNil)
				return eng.Namespace()
			},
			want: func(b compileBound) (int, bool) {
				return b.def, true
			},
		},
		{
			name: "join without the option inherits a configured namespace",
			ns:   newConfigured,
			join: func(compileBound) []wile.EngineOnlyOption {
				return []wile.EngineOnlyOption{}
			},
			want: func(compileBound) (int, bool) {
				return 7, true
			},
		},
		{
			name: "the first join without the option fixes the default",
			ns:   newUnconfigured,
			join: func(compileBound) []wile.EngineOnlyOption {
				return []wile.EngineOnlyOption{}
			},
			want: func(b compileBound) (int, bool) {
				return b.def, true
			},
		},
		{
			name: "the first join with the option fixes its value",
			ns:   newUnconfigured,
			join: func(b compileBound) []wile.EngineOnlyOption {
				return []wile.EngineOnlyOption{b.opt(7)}
			},
			want: func(compileBound) (int, bool) {
				return 7, true
			},
		},
		{
			name: "join with 0 after a first join at the default is refused",
			ns:   newJoinedAtDefault,
			join: func(b compileBound) []wile.EngineOnlyOption {
				return []wile.EngineOnlyOption{b.opt(0)}
			},
			wantErr: true,
			want: func(b compileBound) (int, bool) {
				return b.def, true
			},
		},
		{
			name: "join with the namespace's own value is accepted",
			ns:   newConfigured,
			join: func(b compileBound) []wile.EngineOnlyOption {
				return []wile.EngineOnlyOption{b.opt(7)}
			},
			want: func(compileBound) (int, bool) {
				return 7, true
			},
		},
		{
			name: "join with a different value is refused and writes nothing",
			ns:   newConfigured,
			join: func(b compileBound) []wile.EngineOnlyOption {
				return []wile.EngineOnlyOption{b.opt(0)}
			},
			wantErr: true,
			want: func(compileBound) (int, bool) {
				return 7, true
			},
		},
	}
	for _, b := range compileBounds() {
		for _, tc := range tcs {
			t.Run(b.name+"/"+tc.name, func(t *testing.T) {
				ns := tc.ns(t, b, 7)
				if tc.join != nil {
					_, err := wile.NewEngineWithNamespace(ctx, ns, tc.join(b)...)
					if tc.wantErr {
						qt.Assert(t, errors.Is(err, werr.ErrEngineInit), qt.IsTrue, qt.Commentf("%v", err))
						qt.Assert(t, errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue, qt.Commentf("%v", err))
					} else {
						qt.Assert(t, err, qt.IsNil)
					}
				}
				gotN, gotOK := b.read(ns)
				wantN, wantOK := tc.want(b)
				qt.Assert(t, gotOK, qt.Equals, wantOK)
				if wantOK {
					qt.Assert(t, gotN, qt.Equals, wantN)
				}
			})
		}
	}
}

// Pin: a second engine joining a namespace does not move the first engine's
// expansion bound. Before the bound was the namespace's alone, the join wrote the
// default over it and the first engine expanded this program unbounded.
func TestJoiningEngineKeepsTheFirstEnginesExpandBound(t *testing.T) {
	ctx := context.Background()
	eng1, err := wile.NewEngine(ctx, wile.WithMaxExpandDepth(100))
	qt.Assert(t, err, qt.IsNil)
	_, err = wile.NewEngineWithNamespace(ctx, eng1.Namespace())
	qt.Assert(t, err, qt.IsNil)
	src := "(define-syntax gen (syntax-rules () ((_ () acc) acc) ((_ (x . xs) acc) (gen xs (list acc)))))" +
		" (gen (" + strings.Repeat("a ", 600) + ") 0)"

	_, err = eng1.EvalMultiple(ctx, src)

	qt.Assert(t, errors.Is(err, werr.ErrExpandDepthExceeded), qt.IsTrue, qt.Commentf("%v", err))
}

// Pin: EvalIn compiles its top-level form at the TARGET namespace's bounds, the
// ones its nested compiles already used, not the calling engine's. The target
// here was configured by no engine, so it expands at the default and accepts a
// form the caller's bound of 3 would refuse.
func TestEvalInCompilesAtTheTargetNamespacesBound(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithMaxExpandDepth(3))
	qt.Assert(t, err, qt.IsNil)
	target, err := wile.NewNamespace(ctx)
	qt.Assert(t, err, qt.IsNil)
	expr := eng.MustParse(ctx,
		"(let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) (let ((e 5)) (+ a b c d e))))))")

	v, err := eng.EvalIn(ctx, expr, target)

	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "15")
}
