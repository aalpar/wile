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
	"fmt"
	"log"

	"github.com/aalpar/wile"
	"github.com/aalpar/wile/internal/extensions/io"
	"github.com/aalpar/wile/values"
)

func ExampleNewEngine() {
	engine, err := wile.NewEngine()
	if err != nil {
		log.Fatal(err)
	}

	ctx := context.Background()
	result, err := engine.Eval(ctx, "(+ 1 2 3)")
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(result.SchemeString())
	// Output: 6
}

func ExampleEngine_EvalMultiple() {
	engine, err := wile.NewEngine()
	if err != nil {
		log.Fatal(err)
	}

	ctx := context.Background()
	result, err := engine.EvalMultiple(ctx, `
		(define x 10)
		(define y 20)
		(+ x y)
	`)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(result.SchemeString())
	// Output: 30
}

func ExampleEngine_Compile() {
	engine, err := wile.NewEngine()
	if err != nil {
		log.Fatal(err)
	}

	// Define a variable, then compile an expression that uses it.
	ctx := context.Background()
	_, err = engine.Eval(ctx, "(define x 0)")
	if err != nil {
		log.Fatal(err)
	}

	compiled, err := engine.Compile("(* x x)")
	if err != nil {
		log.Fatal(err)
	}

	// Run the same compiled code with different values of x.
	for _, n := range []int64{3, 5, 7} {
		err = engine.Define("x", wile.NewInteger(n))
		if err != nil {
			log.Fatal(err)
		}

		result, err := engine.Run(ctx, compiled)
		if err != nil {
			log.Fatal(err)
		}

		fmt.Println(result.SchemeString())
	}
	// Output:
	// 9
	// 25
	// 49
}

func ExampleEngine_Define() {
	engine, err := wile.NewEngine()
	if err != nil {
		log.Fatal(err)
	}

	err = engine.Define("width", wile.NewInteger(800))
	if err != nil {
		log.Fatal(err)
	}

	err = engine.Define("height", wile.NewInteger(600))
	if err != nil {
		log.Fatal(err)
	}

	ctx := context.Background()
	result, err := engine.Eval(ctx, "(* width height)")
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(result.SchemeString())
	// Output: 480000
}

func ExampleEngine_RegisterPrimitive() {
	engine, err := wile.NewEngine()
	if err != nil {
		log.Fatal(err)
	}

	// Register a Go function that doubles an integer.
	err = engine.RegisterPrimitive(wile.PrimitiveSpec{
		Name:       "double",
		ParamCount: 1,
		Impl: func(_ context.Context, mc *wile.MachineContext) error {
			n := mc.Arg(0).(*values.Integer).Value
			mc.SetValue(values.NewInteger(n * 2))
			return nil
		},
	})
	if err != nil {
		log.Fatal(err)
	}

	ctx := context.Background()
	result, err := engine.Eval(ctx, "(double 21)")
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(result.SchemeString())
	// Output: 42
}

func ExampleEngine_Call() {
	engine, err := wile.NewEngine()
	if err != nil {
		log.Fatal(err)
	}

	// Define a Scheme function.
	ctx := context.Background()
	_, err = engine.EvalMultiple(ctx, `
		(define (square x) (* x x))
	`)
	if err != nil {
		log.Fatal(err)
	}

	// Retrieve and call it from Go.
	proc, ok := engine.Get("square")
	if !ok {
		log.Fatal("square not found")
	}

	result, err := engine.Call(ctx, proc, wile.NewInteger(12))
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(result.SchemeString())
	// Output: 144
}

func ExampleNewEngine_withExtension() {
	_, err := wile.NewEngine(
		wile.WithExtension(io.Extension),
	)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("engine created with I/O extension")
	// Output: engine created with I/O extension
}
