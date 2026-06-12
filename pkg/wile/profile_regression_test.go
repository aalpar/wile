// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.

package wile_test

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/wile"
	"github.com/aalpar/wile/stdlib"
)

// TestAlgebraGraphLoadsUnderSmallProfile pins the regression where the
// bigint-counting-semiring integration imported (wile algebragraph)
// unconditionally, breaking (wile algebra graph) for users on profiles
// that don't bundle the algebragraph extension. The fix is a cond-expand
// guard in graph.sld; this test ensures it stays.
func TestAlgebraGraphLoadsUnderSmallProfile(t *testing.T) {
	for _, prof := range []wile.Profile{
		wile.Tiny,
		wile.Console,
		wile.ConsoleWithLoad,
		wile.Small,
		wile.KitchenSink,
	} {
		t.Run(prof.String(), func(t *testing.T) {
			ctx := context.Background()
			eng, err := wile.NewEngine(ctx,
				wile.WithProfile(prof),
				wile.WithSourceFS(stdlib.FS),
				wile.WithLibraryPaths("."))
			if err != nil {
				t.Fatalf("NewEngine(%s): %v", prof, err)
			}
			defer eng.Close()

			_, err = eng.EvalMultiple(ctx,
				`(import (wile algebra graph) (wile algebra semiring))
				 (display
				  (graph-query (make-graph-analysis (counting-semiring)
				                                    '(("A" . (("B" . 1))) ("B" . ())) #f)
				               "A" "B"))`)
			// (counting-semiring) and (wile algebra graph) are both stdlib
			// libraries — they should load on any profile, including 'tiny'
			// where (wile algebragraph) is absent.
			if err != nil && strings.Contains(err.Error(), "algebragraph") {
				t.Fatalf("profile %s: (wile algebra graph) tried to load (wile algebragraph) unconditionally; cond-expand guard regressed: %v", prof, err)
			}
			// Other errors (e.g., 'display' not in tiny) are profile-expected;
			// we only police the algebragraph-import regression here.
		})
	}
}
