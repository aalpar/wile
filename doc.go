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
// With extensions:
//
//	engine, err := wile.NewEngine(ctx,
//	    wile.WithExtension(io.Extension),
//	    wile.WithExtension(system.Extension),
//	)
//
// Sandboxed engine (no eval, system, process, gointerop; file ops restricted to /tmp):
//
//	engine, err := wile.NewEngine(ctx, wile.WithProfile(wile.Console))
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
