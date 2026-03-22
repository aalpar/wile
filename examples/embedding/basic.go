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

// basic demonstrates embedding the Wile Scheme interpreter in a Go program.
//
// Run with: go run ./examples/embedding/basic.go
package main

import (
	"context"
	"fmt"
	"log"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/values"
)

func main() {
	ctx := context.Background()

	// 1. Create an engine with default (core) primitives.
	engine, err := wile.NewEngine(ctx)
	if err != nil {
		log.Fatal(err)
	}

	// 2. Evaluate a simple expression.
	result, err := engine.Eval(ctx, engine.MustParse(ctx, "(+ 1 2 3)"))
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("(+ 1 2 3) =>", result.SchemeString())

	// 3. Push Go values into Scheme.
	err = engine.Define("width", wile.NewInteger(800))
	if err != nil {
		log.Fatal(err)
	}

	err = engine.Define("height", wile.NewInteger(600))
	if err != nil {
		log.Fatal(err)
	}

	result, err = engine.Eval(ctx, engine.MustParse(ctx, "(* width height)"))
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("(* width height) =>", result.SchemeString())

	// 4. Register a Go function as a Scheme primitive.
	err = engine.RegisterPrimitive(wile.PrimitiveSpec{
		Name:       "go-max",
		ParamCount: 2,
		Impl: func(mc *wile.MachineContext) error {
			a := mc.Arg(0).(*values.Integer).Value
			b := mc.Arg(1).(*values.Integer).Value
			if a > b {
				mc.SetValue(values.NewInteger(a))
			} else {
				mc.SetValue(values.NewInteger(b))
			}
			return nil
		},
	})
	if err != nil {
		log.Fatal(err)
	}

	result, err = engine.Eval(ctx, engine.MustParse(ctx, "(go-max 42 17)"))
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("(go-max 42 17) =>", result.SchemeString())

	// 5. Define a Scheme function and call it from Go.
	_, err = engine.EvalMultiple(ctx, `
		(define (factorial n)
		  (if (<= n 1)
		      1
		      (* n (factorial (- n 1)))))
	`)
	if err != nil {
		log.Fatal(err)
	}

	proc, ok := engine.Get("factorial")
	if !ok {
		log.Fatal("factorial not found")
	}

	result, err = engine.Call(ctx, proc, wile.NewInteger(10))
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("(factorial 10) =>", result.SchemeString())

	// 6. Compile once, run with different inputs.
	_, err = engine.Eval(ctx, engine.MustParse(ctx, "(define x 0)"))
	if err != nil {
		log.Fatal(err)
	}

	compiled, err := engine.Compile(context.Background(), engine.MustParse(context.Background(), "(* x x)"))
	if err != nil {
		log.Fatal(err)
	}

	for _, n := range []int64{3, 7, 11} {
		err = engine.Define("x", wile.NewInteger(n))
		if err != nil {
			log.Fatal(err)
		}

		result, err = engine.Run(ctx, compiled)
		if err != nil {
			log.Fatal(err)
		}
		fmt.Printf("x=%d: (* x x) => %s\n", n, result.SchemeString())
	}
}
