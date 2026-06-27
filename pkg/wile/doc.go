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
//	fmt.Println(result) // 6
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
// Strict namespace (bare top level; layer R7RS libraries on top):
//
//	engine, err := wile.NewEngine(ctx,
//	    wile.WithProfile(wile.Small), wile.WithStrictNamespace(),
//	    wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths(),
//	)
//	// (display 1) errors until imported; (import (scheme r5rs)) layers it on.
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
