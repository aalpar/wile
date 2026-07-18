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

package files

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/security"
)

// confinedOpen via os.Root must refuse to open a path that escapes the root
// through a symlink, even when the path string lexically starts with the root.
// This is the race-free containment: os.Root resolves components relative to a
// directory descriptor, so a swapped component cannot redirect the open.
func TestConfinedOpen_SymlinkEscapeRefused(t *testing.T) {
	dir, err := os.MkdirTemp("/tmp", "wile_conf_")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)

	auth := security.ConsoleAuthorizer() // confines to /tmp

	// A real file under the root opens fine.
	good := filepath.Join(dir, "ok.txt")
	err = os.WriteFile(good, []byte("hi"), 0o600)
	if err != nil {
		t.Fatal(err)
	}
	f, err := confinedOpen(auth, good)
	if err != nil {
		t.Fatalf("legitimate /tmp file should open, got: %v", err)
	}
	_ = f.Close()

	// A symlink escaping the root must be refused by os.Root.
	link := filepath.Join(dir, "escape")
	err = os.Symlink("/etc", link)
	if err != nil {
		t.Fatal(err)
	}
	escaped := filepath.Join(link, "passwd") // lexically under /tmp, really /etc/passwd
	esc, err := confinedOpen(auth, escaped)
	if err == nil {
		_ = esc.Close()
		t.Fatalf("os.Root must refuse symlink escape %q", escaped)
	}
}

// With no confining authorizer, confinedOpen falls back to plain os.Open and
// can read outside any root (the unrestricted engine case).
func TestConfinedOpen_UnconfinedFallsBack(t *testing.T) {
	f, err := confinedOpen(nil, "/etc/hosts")
	if err != nil {
		t.Skipf("/etc/hosts not readable here: %v", err)
	}
	_ = f.Close()
}

// Every confined operation must work normally for legitimate paths under the
// root — the os.Root routing must not break ordinary file usage.
func TestConfinedOps_RoundTripUnderRoot(t *testing.T) {
	dir, err := os.MkdirTemp("/tmp", "wile_conf_rt_")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)
	auth := security.ConsoleAuthorizer()

	sub := filepath.Join(dir, "d")
	err = confinedMkdir(auth, sub, 0o755)
	if err != nil {
		t.Fatalf("confinedMkdir: %v", err)
	}
	file := filepath.Join(sub, "f.txt")
	w, err := confinedCreate(auth, file)
	if err != nil {
		t.Fatalf("confinedCreate: %v", err)
	}
	_, _ = w.WriteString("data")
	_ = w.Close()

	_, err = confinedStat(auth, file)
	if err != nil {
		t.Fatalf("confinedStat: %v", err)
	}
	entries, err := confinedReadDir(auth, sub)
	if err != nil || len(entries) != 1 || entries[0].Name() != "f.txt" {
		t.Fatalf("confinedReadDir = (%v, %v), want [f.txt]", entries, err)
	}
	err = confinedRemove(auth, file)
	if err != nil {
		t.Fatalf("confinedRemove: %v", err)
	}
	_, err = confinedStat(auth, file)
	if err == nil {
		t.Fatal("file should be gone after confinedRemove")
	}
}

func TestRelWithinRoot(t *testing.T) {
	// /tmp -> /private/tmp on macOS; rel must resolve either spelling cleanly.
	resolved, err := filepath.EvalSymlinks("/tmp")
	if err != nil {
		t.Fatal(err)
	}
	tcs := []struct {
		name, root, target, want string
	}{
		{"plain", "/tmp", "/tmp/a/b", "a/b"},
		{"resolved-root alias", "/tmp", filepath.Join(resolved, "a", "b"), "a/b"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got, err := relWithinRoot(tc.root, tc.target)
			if err != nil || got != tc.want {
				t.Fatalf("relWithinRoot(%q,%q) = (%q,%v), want %q", tc.root, tc.target, got, err, tc.want)
			}
		})
	}
}

// A custom path-gating authorizer that imposes no os.Root confinement must not
// have its gate bypassed by a pre-planted symlink. The authorizer allows only
// targets lexically under allowedDir; a symlink inside allowedDir points at a
// file outside it. The lexical gate on the link path passes, so before the
// re-gate the plain os.Open followed it and handed out the outside file. This
// is the extensions/files twin of the resolver's confinedOpenFile gap: the
// surface here is open-input-file / open-output-file / file-exists?, not code
// loading.
func TestConfinedOpen_SymlinkEscapeDeniedUnconfined(t *testing.T) {
	allowedDir := t.TempDir()
	outsideDir := t.TempDir()

	secret := filepath.Join(outsideDir, "secret.txt")
	err := os.WriteFile(secret, []byte("secret"), 0o600)
	if err != nil {
		t.Fatal(err)
	}

	link := filepath.Join(allowedDir, "link.txt")
	err = os.Symlink(secret, link)
	if err != nil {
		t.Fatal(err)
	}

	// Lexical prefix gate, deliberately NOT RootConfined.
	auth := security.AuthorizerFunc(func(req security.AccessRequest) error {
		if strings.HasPrefix(req.Target, allowedDir+string(filepath.Separator)) {
			return nil
		}
		return security.ErrAccessDenied
	})

	f, err := confinedOpen(auth, link)
	if err == nil {
		_ = f.Close()
		t.Fatalf("symlink at %q escaping to %q must be denied, not followed", link, secret)
	}
	if !errors.Is(err, security.ErrAccessDenied) {
		t.Fatalf("want ErrAccessDenied, got: %v", err)
	}

	// Same gate, same escape, through the stat path.
	_, err = confinedStat(auth, link)
	if !errors.Is(err, security.ErrAccessDenied) {
		t.Fatalf("confinedStat: want ErrAccessDenied, got: %v", err)
	}
}
