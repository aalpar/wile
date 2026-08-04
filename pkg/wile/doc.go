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

// Package wile provides the public API for embedding the Wile Scheme interpreter.
//
// Basic usage:
//
//	engine, err := wile.NewEngine(ctx)
//	if err != nil {
//	    log.Fatal(err)
//	}
//	result, err := engine.Eval(ctx, engine.MustParse(ctx, "(+ 1 2 3)"))
//	fmt.Println(result.SchemeString()) // 6
//
// Profile-based configuration:
//
//	engine, err := wile.NewEngine(ctx, wile.WithProfile(wile.Small))
//
//	engine, err := wile.NewEngine(ctx,
//	    wile.WithProfile(wile.Console),
//	    wile.WithEnv("APP_MODE", "production"),
//	)
//
// Sandboxed eval/load (wile-goast pattern):
//
//	engine, err := wile.NewEngine(ctx, wile.WithProfile(wile.ConsoleWithLoad))
//
// Ad-hoc extension selection (bypasses profiles):
//
//	engine, err := wile.NewEngine(ctx,
//	    wile.WithExtension(io.Extension),
//	    wile.WithExtension(system.Extension),
//	)
//
// Strict namespace (core-only top level; layer R7RS libraries on top):
//
//	engine, err := wile.NewEngine(ctx,
//	    wile.WithProfile(wile.Small), wile.WithStrictNamespace(),
//	    wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths(),
//	)
//	// (display 1) errors until imported; (import (scheme r5rs)) layers it on.
//
// One step further — nothing pre-bound at all, so every dependency is declared
// in source. Only the core special forms remain, because they are phase handlers
// rather than bindings:
//
//	engine, err := wile.NewEngine(ctx,
//	    wile.WithProfile(wile.Small), wile.WithoutAmbientBindings(),
//	    wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths(),
//	)
//	// (car '(1 2)) errors too; (import (scheme base)) restores it.
//	// Budget ~9.4 ms per import — see WithoutAmbientBindings.
//
// Custom primitives:
//
//	engine, _ := wile.NewEngine(ctx)
//	engine.RegisterPrimitive(wile.PrimitiveSpec{
//	    Name:       "my-func",
//	    ParamCount: 1,
//	    Impl:       myFuncImpl,
//	})
package wile
