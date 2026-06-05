package parser

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/werr"
)

// Deeply nested parens must return a catchable ErrParseDepthExceeded,
// never crash with a fatal Go stack overflow.
func TestParser_DepthLimit_Trips(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	// Well past DefaultMaxParseDepth (10000) but tiny in memory.
	src := strings.Repeat("(", 50000)
	p := NewParser(env, true, strings.NewReader(src))
	_, err := p.ReadSyntax(context.TODO())
	if err == nil {
		t.Fatal("expected depth-limit error, got nil")
	}
	if !errors.Is(err, werr.ErrParseDepthExceeded) {
		t.Fatalf("expected ErrParseDepthExceeded, got: %v", err)
	}
}
