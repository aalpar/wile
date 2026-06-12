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
	"os"
	"path/filepath"
	"testing"

	"github.com/aalpar/wile/pkg/wile"
	"github.com/aalpar/wile/security"
)

// Audit reproduction: under the Console profile (file access restricted to
// /tmp), a symlink staged in /tmp pointing outside /tmp must NOT let evaluated
// Scheme read a file outside the sandbox. Pre-fix this read /etc/passwd.
func TestEngine_SymlinkEscape_Denied(t *testing.T) {
	dir, err := os.MkdirTemp("/tmp", "wile_esc_")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)
	link := filepath.Join(dir, "escape")
	err = os.Symlink("/etc", link)
	if err != nil {
		t.Fatal(err)
	}
	target := filepath.Join(link, "passwd") // resolves to /etc/passwd

	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.Console))
	if err != nil {
		t.Fatal(err)
	}
	code := `(call-with-input-file "` + target + `" (lambda (p) (read-line p)))`
	_, err = eng.EvalMultiple(ctx, code)
	if err == nil {
		t.Fatalf("SECURITY: Console read %q outside /tmp via symlink", target)
	}
}

// Regression for wile-goast: ConsoleWithLoad must keep allowing (eval ...).
func TestEngine_Eval_AllowedUnderConsoleWithLoad(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.ConsoleWithLoad))
	if err != nil {
		t.Fatal(err)
	}
	_, err = eng.EvalMultiple(ctx, `(eval '(+ 1 2))`)
	if err != nil {
		t.Fatalf("ConsoleWithLoad should allow (eval ...), got: %v", err)
	}
}

// The new gate lets an authorizer deny code:eval. With eval present but a
// deny-all authorizer, (eval ...) and (compile ...) must be refused.
func TestEngine_Eval_DeniedByAuthorizer(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithAuthorizer(security.DenyAll()),
	)
	if err != nil {
		t.Fatal(err)
	}
	_, err = eng.EvalMultiple(ctx, `(eval '(+ 1 2))`)
	if err == nil {
		t.Fatal("deny-all authorizer should refuse (eval ...)")
	}
	_, err = eng.EvalMultiple(ctx, `(compile '(+ 1 2))`)
	if err == nil {
		t.Fatal("deny-all authorizer should refuse (compile ...)")
	}
}
