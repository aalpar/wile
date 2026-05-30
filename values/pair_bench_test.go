// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0

package values_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/values"
)

func makeList(n int) *values.Pair {
	var head values.Value = values.EmptyList
	for i := n - 1; i >= 0; i-- {
		head = values.NewCons(values.NewInteger(int64(i)), head)
	}
	return head.(*values.Pair)
}

func BenchmarkPairForEach_10(b *testing.B) {
	lst := makeList(10)
	ctx := context.Background()
	b.ReportAllocs()
	b.ResetTimer()
	for b.Loop() {
		_, _ = lst.ForEach(ctx, func(_ context.Context, _ int, _ bool, _ values.Value) error {
			return nil
		})
	}
}

func BenchmarkPairForEach_100(b *testing.B) {
	lst := makeList(100)
	ctx := context.Background()
	b.ReportAllocs()
	b.ResetTimer()
	for b.Loop() {
		_, _ = lst.ForEach(ctx, func(_ context.Context, _ int, _ bool, _ values.Value) error {
			return nil
		})
	}
}

func BenchmarkPairForEach_1000(b *testing.B) {
	lst := makeList(1000)
	ctx := context.Background()
	b.ReportAllocs()
	b.ResetTimer()
	for b.Loop() {
		_, _ = lst.ForEach(ctx, func(_ context.Context, _ int, _ bool, _ values.Value) error {
			return nil
		})
	}
}
