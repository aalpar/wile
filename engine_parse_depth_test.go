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
