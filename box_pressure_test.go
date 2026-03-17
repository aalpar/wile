package wile

import (
	"context"
	"fmt"
	"testing"
)

// Box allocation pressure measurement for flat closures.
// See plans/FLAT-CLOSURES.md open question #3.
//
// Boxing happens when a variable is both captured by a closure AND mutated
// via set!. Each call to the defining scope executes OpBox once per boxed
// variable, allocating a *values.Box on the heap. This file measures whether
// that allocation is significant in practice.

// boxBenchCase defines a Scheme program that exercises boxing.
type boxBenchCase struct {
	name  string
	setup string
	code  string
}

var boxBenchCases = []boxBenchCase{
	{
		// Counter: one boxed variable, called N times.
		// Each call to make-counter boxes count once. The returned
		// closure mutates count through the box on every invocation.
		name: "Counter",
		setup: `(define (make-counter)
			(let ((count 0))
				(lambda ()
					(set! count (+ count 1))
					count)))`,
		code: `(let ((c (make-counter)))
			(let loop ((i 0))
				(if (< i 1000)
					(begin (c) (loop (+ i 1)))
					(c))))`,
	},
	{
		// SharedMutation: two closures sharing a boxed variable.
		// Tests that the Box identity is shared (both closures
		// read/write the same box).
		name: "SharedMutation",
		setup: `(define (make-pair)
			(let ((val 0))
				(cons
					(lambda () (set! val (+ val 1)) val)
					(lambda () (set! val (- val 1)) val))))`,
		code: `(let ((p (make-pair)))
			(let loop ((i 0))
				(if (< i 1000)
					(begin ((car p)) ((cdr p)) (loop (+ i 1)))
					((car p)))))`,
	},
	{
		// NestedCapture: boxed variable captured through two levels.
		// Inner closure captures x from grandparent through parent's
		// freeVars (FromFreeVars=true path).
		name: "NestedCapture",
		setup: `(define (make-nested)
			(let ((x 0))
				(lambda ()
					(lambda ()
						(set! x (+ x 1))
						x))))`,
		code: `(let ((f ((make-nested))))
			(let loop ((i 0))
				(if (< i 1000)
					(begin (f) (loop (+ i 1)))
					(f))))`,
	},
	{
		// MixedBoxedUnboxed: some captured vars boxed, some not.
		// Only mutated captures get boxed; read-only captures stay flat.
		name: "MixedBoxedUnboxed",
		setup: `(define (make-mixed a b)
			(let ((count 0))
				(lambda ()
					(set! count (+ count 1))
					(+ a b count))))`,
		code: `(let ((f (make-mixed 10 20)))
			(let loop ((i 0))
				(if (< i 1000)
					(begin (f) (loop (+ i 1)))
					(f))))`,
	},
	{
		// HotBoxCreation: repeatedly creates closures that box variables.
		// Each iteration of the loop creates a new closure with a fresh box.
		// Measures OpBox allocation rate when closure creation is in the hot path.
		name: "HotBoxCreation",
		setup: `(define (make-adder start)
			(let ((n start))
				(lambda (x)
					(set! n (+ n x))
					n)))`,
		code: `(let loop ((i 0) (sum 0))
			(if (< i 1000)
				(let ((f (make-adder i)))
					(loop (+ i 1) (+ sum (f 1))))
				sum))`,
	},
}

func BenchmarkBoxPressure(b *testing.B) {
	for _, tc := range boxBenchCases {
		b.Run(tc.name, func(b *testing.B) {
			ctx := context.Background()
			engine, err := NewEngine(ctx)
			if err != nil {
				b.Fatal(err)
			}
			if tc.setup != "" {
				_, err = engine.Eval(ctx, tc.setup)
				if err != nil {
					b.Fatal(err)
				}
			}
			compiled, err := engine.Compile(ctx, tc.code)
			if err != nil {
				b.Fatal(err)
			}
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				_, err = engine.Run(ctx, compiled)
				if err != nil {
					b.Fatal(err)
				}
			}
			b.StopTimer()
			c := engine.LastCounters()
			b.ReportMetric(float64(c.FlatCopyApplies), "flat_applies/op")
			b.ReportMetric(float64(c.EnvsCopied), "envs_copied/op")
		})
	}
}

// TestBoxPressureProfile runs each box benchmark once and prints detailed
// VM counters including opcode histogram (requires WILE_OPCODE_HITS=1).
func TestBoxPressureProfile(t *testing.T) {
	for _, tc := range boxBenchCases {
		t.Run(tc.name, func(t *testing.T) {
			ctx := context.Background()
			engine, err := NewEngine(ctx)
			if err != nil {
				t.Fatal(err)
			}
			if tc.setup != "" {
				_, err = engine.Eval(ctx, tc.setup)
				if err != nil {
					t.Fatal(err)
				}
			}
			_, err = engine.Eval(ctx, tc.code)
			if err != nil {
				t.Fatal(err)
			}
			c := engine.LastCounters()
			t.Logf("VM counters:\n%s", c)
			hist := c.OpcodeHistogram()
			if hist != "" {
				t.Logf("Opcode histogram:\n%s", hist)
			}
			callHist := c.CallHistogram()
			if callHist != "" {
				t.Logf("Call histogram:\n%s", callHist)
			}
		})
	}
}

// TestBoxPressureSummary runs all cases and prints a summary table
// of flat closure and allocation metrics.
func TestBoxPressureSummary(t *testing.T) {
	ctx := context.Background()

	fmt.Println()
	fmt.Println("Box Allocation Pressure Summary")
	fmt.Println("───────────────────────────────────────────────────────────────────────────")
	fmt.Printf("%-20s %10s %10s %10s %12s\n",
		"Benchmark", "TotalOps", "Closures", "FlatApply", "EnvsCopied")
	fmt.Println("───────────────────────────────────────────────────────────────────────────")

	for _, tc := range boxBenchCases {
		engine, err := NewEngine(ctx)
		if err != nil {
			t.Fatal(err)
		}
		if tc.setup != "" {
			_, err = engine.Eval(ctx, tc.setup)
			if err != nil {
				t.Fatal(err)
			}
		}
		_, err = engine.Eval(ctx, tc.code)
		if err != nil {
			t.Fatal(err)
		}
		c := engine.LastCounters()
		fmt.Printf("%-20s %10d %10d %10d %12d\n",
			tc.name, c.OpsExecuted, c.ClosuresApplied, c.FlatCopyApplies, c.EnvsCopied)
	}
	fmt.Println("───────────────────────────────────────────────────────────────────────────")
	fmt.Println("Run with WILE_OPCODE_HITS=1 and TestBoxPressureProfile for per-opcode detail")
}
