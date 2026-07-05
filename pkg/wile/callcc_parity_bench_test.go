package wile_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/wile"
)

// Parity benchmarks for the staff-sweep #9 dual-mode restructure (PrimCallCC /
// call-with-composable-continuation collapsed from two hand-written apply arms
// to one target-selected apply seam). These exercise the INLINE mode
// (mc.Parent() != nil), which is the hot path the restructure must not regress:
// each iteration captures a continuation and immediately invokes it, running the
// callback in place. Compare this branch against master with benchstat.

func benchCallccProgram(b *testing.B, src string) {
	b.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	if err != nil {
		b.Fatal(err)
	}
	defer eng.Close()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, err := eng.EvalMultiple(ctx, src)
		if err != nil {
			b.Fatal(err)
		}
	}
}

// BenchmarkCallccInline: 5000 inline call/cc captures per op, each immediately
// invoking the captured continuation. Dominated by the PrimCallCC inline apply
// seam.
func BenchmarkCallccInline(b *testing.B) {
	benchCallccProgram(b, `
		(let loop ((i 0) (acc 0))
		  (if (= i 5000) acc
		      (loop (+ i 1) (+ acc (call/cc (lambda (k) (k 1)))))))`)
}

// BenchmarkComposableInline: the restructured sibling
// (call-with-composable-continuation). The callback captures the composable
// continuation and returns a value WITHOUT invoking it — this exercises the
// same inline apply seam (target.ApplyCallable(procCls, comp)) the restructure
// touches, without entering composition semantics (invoking a top-level-
// delimited composable continuation re-runs the captured segment).
func BenchmarkComposableInline(b *testing.B) {
	benchCallccProgram(b, `
		(let loop ((i 0) (acc 0))
		  (if (= i 5000) acc
		      (loop (+ i 1)
		            (+ acc (call-with-composable-continuation
		                     (lambda (k) 1)
		                     (default-continuation-prompt-tag))))))`)
}
