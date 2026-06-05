package wile_test

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile"
)

// Regression for the audit finding: untrusted deeply-nested text fed to the
// public API must return an error, NOT crash the host with a fatal stack
// overflow. If the bound regressed, this test binary would die rather than fail.
func TestEngine_DeepNesting_DoesNotCrash(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx)
	if err != nil {
		t.Fatal(err)
	}
	src := strings.Repeat("(", 2_000_000) // crashed the process pre-fix
	_, err = engine.EvalMultiple(ctx, src)
	if err == nil {
		t.Fatal("expected an error for pathologically nested input, got nil")
	}
	t.Logf("got expected error: %v", err)
}

func TestEngine_WithMaxParseDepth(t *testing.T) {
	ctx := context.Background()
	engine, err := wile.NewEngine(ctx, wile.WithMaxParseDepth(20))
	if err != nil {
		t.Fatal(err)
	}
	src := strings.Repeat("(", 100) + "1" + strings.Repeat(")", 100)
	_, err = engine.EvalMultiple(ctx, src)
	if err == nil {
		t.Fatal("depth 100 under a limit of 20 should error")
	}
}
