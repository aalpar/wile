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

package files_test

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// The OS half of W4-6's three-way split of file-exists?.
//
// R7RS §6.14 gives file-exists? a boolean and no error clause, and Wile answers
// #f for absence and for every kind of refusal — a can't-look is a can't-see.
// It does NOT answer #f for a system error that says nothing about whether the
// file is there: a truncated name, a symlink cycle, a failing disk. Those raise.
//
// The authorizer-denial half of the same split is gated in
// pkg/wile/engine_sandbox_test.go, which is where an engine with a policy lives.

// TestFileExistsQ_NonPermissionSystemErrorRaises is gate G5b.2b.
//
// Must-fail-first: before the split, PrimFileExistsQ collapsed every
// confinedStat failure into one BoolToBoolean(err == nil), so this answered #f.
func TestFileExistsQ_NonPermissionSystemErrorRaises(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	// A single component past NAME_MAX: ENAMETOOLONG, deterministically, with
	// no privileged setup. Assert the OS really classifies it as neither
	// absence nor permission, or the gate is pinning nothing.
	tooLong := filepath.Join(dir, strings.Repeat("a", 300))
	_, statErr := os.Stat(tooLong)
	c.Assert(statErr, qt.IsNotNil)
	c.Assert(errors.Is(statErr, fs.ErrNotExist), qt.IsFalse,
		qt.Commentf("over-long component reported as absence: %v", statErr))
	c.Assert(errors.Is(statErr, fs.ErrPermission), qt.IsFalse,
		qt.Commentf("over-long component reported as permission: %v", statErr))

	result := eval(t, engine,
		fmt.Sprintf(`(guard (e ((file-error? e) 'raised)) (file-exists? %q))`, tooLong))
	c.Assert(result.SchemeString(), qt.Equals, "raised")
}

// TestFileExistsQ_OSPermissionErrorAnswersFalse is G5b.2c, a CONTROL rather
// than a gate: it is green on both sides of the split. Before, everything
// answered #f; after, only the three quiet arms do. It pairs with G5b.2b —
// together they pin where the line falls, and neither alone does.
func TestFileExistsQ_OSPermissionErrorAnswersFalse(t *testing.T) {
	c := qt.New(t)
	if os.Getuid() == 0 {
		t.Skip("root bypasses directory permissions, so EACCES is unreachable")
	}
	engine := newEngine(t)
	dir := t.TempDir()

	locked := filepath.Join(dir, "locked")
	err := os.Mkdir(locked, 0o700)
	c.Assert(err, qt.IsNil)
	target := filepath.Join(locked, "secret.txt")
	err = os.WriteFile(target, []byte("secret"), 0o644)
	c.Assert(err, qt.IsNil)
	err = os.Chmod(locked, 0o000)
	c.Assert(err, qt.IsNil)
	// Registered after TempDir's own cleanup, so it runs first (LIFO) and the
	// tree is traversable again by the time RemoveAll walks it.
	t.Cleanup(func() {
		os.Chmod(locked, 0o700) //nolint:errcheck,gosec // best-effort test cleanup
	})

	_, statErr := os.Stat(target)
	c.Assert(errors.Is(statErr, fs.ErrPermission), qt.IsTrue,
		qt.Commentf("expected EACCES through the unreadable parent, got: %v", statErr))

	result := eval(t, engine, fmt.Sprintf(`(file-exists? %q)`, target))
	c.Assert(result.SchemeString(), qt.Equals, "#f")
}
