# OS Primitives Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add directory operations to the `files` extension and create a new `process` extension for subprocess execution, following SRFI-170 conventions.

**Architecture:** Two-phase delivery. Phase 1 adds 5 directory primitives to the existing `files` extension. Phase 2 adds a new `process` extension with a `*Process` value type, `ForeignProcessError` wrapper, new security actions, and 8 primitives.

**Tech Stack:** Go stdlib (`os`, `os/exec`, `syscall`), existing Wile extension/security/error infrastructure.

**Design doc:** `plans/OS-PRIMITIVES.md`

---

## Phase 1: Directory Operations

### Task 1: Add directory primitive implementations

**Files:**
- Create: `extensions/files/prim_directory.go`
- Create: `extensions/files/prim_directory_test.go`
- Modify: `extensions/files/register.go`

**Step 1: Write the failing tests**

Create `extensions/files/prim_directory_test.go`:

```go
package files_test

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func TestCreateDirectory(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	t.Run("creates directory", func(t *testing.T) {
		path := filepath.Join(dir, "newdir")
		eval(t, engine, fmt.Sprintf(`(create-directory %q)`, path))
		info, err := os.Stat(path)
		c.Assert(err, qt.IsNil)
		c.Assert(info.IsDir(), qt.IsTrue)
	})

	t.Run("error if already exists", func(t *testing.T) {
		path := filepath.Join(dir, "existing")
		err := os.Mkdir(path, 0o755)
		c.Assert(err, qt.IsNil)
		evalExpectError(t, engine, fmt.Sprintf(`(create-directory %q)`, path))
	})

	t.Run("error if parent missing", func(t *testing.T) {
		path := filepath.Join(dir, "no", "parent")
		evalExpectError(t, engine, fmt.Sprintf(`(create-directory %q)`, path))
	})

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(create-directory 42)`)
	})
}

func TestDeleteDirectory(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	t.Run("deletes empty directory", func(t *testing.T) {
		path := filepath.Join(dir, "rmme")
		err := os.Mkdir(path, 0o755)
		c.Assert(err, qt.IsNil)
		eval(t, engine, fmt.Sprintf(`(delete-directory %q)`, path))
		_, err = os.Stat(path)
		c.Assert(os.IsNotExist(err), qt.IsTrue)
	})

	t.Run("error if not empty", func(t *testing.T) {
		path := filepath.Join(dir, "notempty")
		err := os.Mkdir(path, 0o755)
		c.Assert(err, qt.IsNil)
		writeTestFile(t, path, "child.txt", "data")
		evalExpectError(t, engine, fmt.Sprintf(`(delete-directory %q)`, path))
	})

	t.Run("error if nonexistent", func(t *testing.T) {
		evalExpectError(t, engine, fmt.Sprintf(`(delete-directory %q)`, filepath.Join(dir, "nope")))
	})

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(delete-directory 42)`)
	})
}

func TestDirectoryFiles(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	t.Run("lists files", func(t *testing.T) {
		writeTestFile(t, dir, "a.txt", "a")
		writeTestFile(t, dir, "b.txt", "b")
		result := eval(t, engine, fmt.Sprintf(`(directory-files %q)`, dir))

		// Collect names from the Scheme list
		var names []string
		list := result.Internal().(values.Tuple)
		list.ForEach(nil, func(v values.Value) error {
			names = append(names, v.(*values.String).Value)
			return nil
		})
		sort.Strings(names)
		c.Assert(names, qt.Contains, "a.txt")
		c.Assert(names, qt.Contains, "b.txt")
	})

	t.Run("excludes dot entries", func(t *testing.T) {
		// ReadDir doesn't return . or .., but verify the contract
		result := eval(t, engine, fmt.Sprintf(`
			(let loop ((fs (directory-files %q)) (ok #t))
			  (if (null? fs)
			      ok
			      (loop (cdr fs)
			            (and ok
			                 (not (string=? (car fs) "."))
			                 (not (string=? (car fs) ".."))))))
		`, dir))
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("returns strings", func(t *testing.T) {
		result := eval(t, engine, fmt.Sprintf(`
			(let loop ((fs (directory-files %q)) (ok #t))
			  (if (null? fs)
			      ok
			      (loop (cdr fs) (and ok (string? (car fs))))))
		`, dir))
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("empty directory", func(t *testing.T) {
		empty := t.TempDir()
		result := eval(t, engine, fmt.Sprintf(`(null? (directory-files %q))`, empty))
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("error if nonexistent", func(t *testing.T) {
		evalExpectError(t, engine, fmt.Sprintf(`(directory-files %q)`, filepath.Join(dir, "nope")))
	})

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(directory-files 42)`)
	})
}

func TestCurrentDirectory(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns a string", func(t *testing.T) {
		result := eval(t, engine, `(string? (current-directory))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("matches os.Getwd", func(t *testing.T) {
		wd, err := os.Getwd()
		c.Assert(err, qt.IsNil)
		result := eval(t, engine, fmt.Sprintf(
			`(string=? (current-directory) %q)`, wd))
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})
}

func TestSetCurrentDirectory(t *testing.T) {
	engine := newEngine(t)
	c := qt.New(t)

	// Save and restore CWD since os.Chdir is process-global.
	origDir, err := os.Getwd()
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		os.Chdir(origDir) //nolint:errcheck
	})

	t.Run("changes directory", func(t *testing.T) {
		target := t.TempDir()
		eval(t, engine, fmt.Sprintf(`(set-current-directory! %q)`, target))
		wd, err := os.Getwd()
		c.Assert(err, qt.IsNil)
		// Resolve symlinks for comparison (macOS /tmp -> /private/tmp)
		resolvedTarget, _ := filepath.EvalSymlinks(target)
		resolvedWd, _ := filepath.EvalSymlinks(wd)
		c.Assert(resolvedWd, qt.Equals, resolvedTarget)
	})

	t.Run("error if nonexistent", func(t *testing.T) {
		evalExpectError(t, engine, `(set-current-directory! "/nonexistent/path/12345")`)
	})

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(set-current-directory! 42)`)
	})
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v -run 'Test(CreateDirectory|DeleteDirectory|DirectoryFiles|CurrentDirectory|SetCurrentDirectory)' ./extensions/files/...`
Expected: FAIL — undefined primitives.

**Step 3: Write the implementations**

Create `extensions/files/prim_directory.go`:

```go
package files

import (
	"os"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimCreateDirectory implements the (create-directory) primitive.
// Creates a single directory level. Errors if it already exists or
// the parent is missing (no recursive mkdir -p behavior).
func PrimCreateDirectory(mc *machine.MachineContext) error {
	path, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "create-directory")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionWrite,
		Target:   path.Value,
	})
	if err != nil {
		return err
	}
	err = os.Mkdir(path.Value, 0o755)
	if err != nil {
		return werr.WrapForeignFileError(err, "create-directory", path.Value)
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimDeleteDirectory implements the (delete-directory) primitive.
// Removes an empty directory. Errors if not empty or nonexistent.
func PrimDeleteDirectory(mc *machine.MachineContext) error {
	path, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "delete-directory")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionDelete,
		Target:   path.Value,
	})
	if err != nil {
		return err
	}
	err = os.Remove(path.Value)
	if err != nil {
		return werr.WrapForeignFileError(err, "delete-directory", path.Value)
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimDirectoryFiles implements the (directory-files) primitive.
// Returns a list of filename strings in the directory, excluding "." and "..".
// Names are filenames only (not full paths).
func PrimDirectoryFiles(mc *machine.MachineContext) error {
	path, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "directory-files")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceFile,
		Action:   security.ActionRead,
		Target:   path.Value,
	})
	if err != nil {
		return err
	}
	entries, err := os.ReadDir(path.Value)
	if err != nil {
		return werr.WrapForeignFileError(err, "directory-files", path.Value)
	}
	list := values.EmptyList
	for i := len(entries) - 1; i >= 0; i-- {
		name := entries[i].Name()
		if name == "." || name == ".." {
			continue
		}
		list = values.NewCons(values.NewString(name), list)
	}
	mc.SetValue(list)
	return nil
}

// PrimCurrentDirectory implements the (current-directory) primitive.
// Returns the current working directory as a string.
func PrimCurrentDirectory(mc *machine.MachineContext) error {
	wd, err := os.Getwd()
	if err != nil {
		return werr.WrapForeignErrorf(err, "current-directory: getwd failed")
	}
	mc.SetValue(values.NewString(wd))
	return nil
}

// PrimSetCurrentDirectory implements the (set-current-directory!) primitive.
//
// WARNING: os.Chdir is process-global. Multiple engines in the same Go
// process share a single working directory. Concurrent calls from different
// goroutines race on the same OS state. This is inherent to POSIX — there
// is no per-thread working directory.
func PrimSetCurrentDirectory(mc *machine.MachineContext) error {
	path, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "set-current-directory!")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceProcess,
		Action:   security.ActionWrite,
		Target:   "cwd",
	})
	if err != nil {
		return err
	}
	err = os.Chdir(path.Value)
	if err != nil {
		return werr.WrapForeignFileError(err, "set-current-directory!", path.Value)
	}
	mc.SetValue(values.Void)
	return nil
}
```

**Step 4: Register the primitives**

Add to `extensions/files/register.go` `addPrimitives` function, inside the `r.AddPrimitives` slice, after the existing entries:

```go
{Name: "create-directory", ParamCount: 1, Impl: PrimCreateDirectory,
    Doc: "Creates a directory.", ParamNames: []string{"path"}, Category: "files"},
{Name: "delete-directory", ParamCount: 1, Impl: PrimDeleteDirectory,
    Doc: "Deletes an empty directory.", ParamNames: []string{"path"}, Category: "files"},
{Name: "directory-files", ParamCount: 1, Impl: PrimDirectoryFiles,
    Doc: "Returns filenames in a directory as a list of strings.", ParamNames: []string{"path"}, Category: "files"},
{Name: "current-directory", Impl: PrimCurrentDirectory,
    Doc: "Returns the current working directory.", Category: "files"},
{Name: "set-current-directory!", ParamCount: 1, Impl: PrimSetCurrentDirectory,
    Doc: "Changes the current working directory.", ParamNames: []string{"path"}, Category: "files"},
```

**Step 5: Run tests to verify they pass**

Run: `go test -v -run 'Test(CreateDirectory|DeleteDirectory|DirectoryFiles|CurrentDirectory|SetCurrentDirectory)' ./extensions/files/...`
Expected: PASS

**Step 6: Run lint**

Run: `make lint`
Expected: PASS

**Step 7: Commit**

```
feat(files): add directory primitives

Add create-directory, delete-directory, directory-files,
current-directory, and set-current-directory! to the files extension.

SRFI-170 subset — single-level mkdir, POSIX rmdir semantics.
set-current-directory! gated by ResourceProcess/ActionWrite/"cwd".
```

---

### Task 2: Document os.Chdir semantics

**Files:**
- Modify: `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md`

**Step 1: Add os.Chdir documentation**

Append a new section to `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md`:

```markdown
## Process-Global Working Directory

**Primitive:** `set-current-directory!`

**Behavior:** Calls `os.Chdir`, which changes the working directory for the entire OS process.

**Impact:** Multiple Wile engines in the same Go process share one working directory. Concurrent calls from different goroutines race on the same OS state. This is inherent to POSIX — there is no per-thread working directory.

**Mitigation:** The primitive is gated by `security.ResourceProcess` / `security.ActionWrite` / target `"cwd"`, so embedders can deny it via their authorizer. When denied, all file operations should use absolute paths.

**R7RS context:** R7RS does not specify `set-current-directory!` or any directory operations. This follows SRFI-170 conventions. The SRFI acknowledges process-global CWD as a POSIX limitation.
```

**Step 2: Update the summary count**

Change the summary line from "Two known differences" to "Three known differences" and add a line item.

**Step 3: Commit**

```
docs: document os.Chdir process-global semantics
```

---

## Phase 2: Process Extension

### Task 3: Add security action constants

**Files:**
- Modify: `security/access.go`
- Modify: `security/security_test.go`

**Step 1: Add constants to `security/access.go`**

Add to the action constants block:

```go
ActionExec      = "exec"       // structured process execution (process-spawn)
ActionExecShell = "exec-shell" // shell command execution (system)
```

**Step 2: Add test assertions to `security/security_test.go`**

Find the existing action constant assertions and add:

```go
c.Assert(ActionExec, qt.Equals, "exec")
c.Assert(ActionExecShell, qt.Equals, "exec-shell")
```

**Step 3: Run tests**

Run: `go test -v ./security/...`
Expected: PASS

**Step 4: Commit**

```
feat(security): add ActionExec and ActionExecShell constants
```

---

### Task 4: Add ForeignProcessError to werr

**Files:**
- Modify: `werr/werr.go`
- Modify: `werr/werr_test.go`

**Step 1: Write the failing test**

Add to `werr/werr_test.go`:

```go
func TestForeignProcessError(t *testing.T) {
	c := qt.New(t)

	t.Run("wraps with command context", func(t *testing.T) {
		cause := fmt.Errorf("exec: not found")
		err := werr.WrapForeignProcessError(cause, "process-spawn", "nonexistent-cmd")
		c.Assert(err.Op, qt.Equals, "process-spawn")
		c.Assert(err.Command, qt.Equals, "nonexistent-cmd")
		c.Assert(err.Error(), qt.Matches, `.*process-spawn.*nonexistent-cmd.*`)
	})

	t.Run("unwraps to cause", func(t *testing.T) {
		cause := fmt.Errorf("signal: killed")
		err := werr.WrapForeignProcessError(cause, "process-wait", "sleep")
		c.Assert(errors.Is(err, cause), qt.IsTrue)
	})
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestForeignProcessError ./werr/...`
Expected: FAIL — `WrapForeignProcessError` not defined.

**Step 3: Write implementation**

Add to `werr/werr.go`, after `ForeignFileError` and `WrapForeignFileError`:

```go
// ForeignProcessError represents an error from a process operation.
// Parallel to ForeignFileError for programmatic inspection of failed
// process operations.
type ForeignProcessError struct {
	*ForeignError
	Command string // the command that was run
	Op      string // the operation (e.g., "process-spawn", "system")
}

// WrapForeignProcessError wraps an OS error with process context.
func WrapForeignProcessError(err error, op string, command string) *ForeignProcessError {
	q := &ForeignProcessError{
		ForeignError: WrapForeignErrorf(err, "%s: %s", op, command),
		Command:      command,
		Op:           op,
	}
	return q
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestForeignProcessError ./werr/...`
Expected: PASS

**Step 5: Commit**

```
feat(werr): add ForeignProcessError for process operation errors
```

---

### Task 5: Add ErrNotAProcess sentinel and Process value type

**Files:**
- Modify: `werr/werr.go` (add sentinel)
- Create: `values/process.go`
- Create: `values/process_test.go`

**Step 1: Add sentinel**

Add to `werr/werr.go` sentinel block:

```go
ErrNotAProcess = NewStaticError("not a process")
```

**Step 2: Write the failing test**

Create `values/process_test.go`:

```go
package values_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func TestProcess(t *testing.T) {
	c := qt.New(t)

	t.Run("SchemeString includes command", func(t *testing.T) {
		p := values.NewProcess("ls", nil, nil, nil, nil)
		c.Assert(p.SchemeString(), qt.Matches, `#<process "ls".*>`)
	})

	t.Run("IsVoid is false", func(t *testing.T) {
		p := values.NewProcess("ls", nil, nil, nil, nil)
		c.Assert(p.IsVoid(), qt.IsFalse)
	})

	t.Run("EqualTo is identity", func(t *testing.T) {
		p := values.NewProcess("ls", nil, nil, nil, nil)
		c.Assert(p.EqualTo(p), qt.IsTrue)

		q := values.NewProcess("ls", nil, nil, nil, nil)
		c.Assert(p.EqualTo(q), qt.IsFalse)
	})

	t.Run("Command returns command name", func(t *testing.T) {
		p := values.NewProcess("grep", nil, nil, nil, nil)
		c.Assert(p.Command(), qt.Equals, "grep")
	})
}
```

**Step 3: Run test to verify it fails**

Run: `go test -v -run TestProcess ./values/...`
Expected: FAIL — `Process` type not defined.

**Step 4: Write the Process type**

Create `values/process.go`:

```go
package values

import (
	"fmt"
	"os/exec"
)

var _ Value = (*Process)(nil)

// Process represents a running OS process.
// Wraps *exec.Cmd and its connected pipes. Accessors return
// the ports for stdout, stderr, and stdin.
type Process struct {
	cmd     *exec.Cmd
	command string
	stdin   *CharacterOutputPort
	stdout  *CharacterInputPort
	stderr  *CharacterInputPort
}

// NewProcess creates a Process value. The cmd may be nil for testing.
// Ports may be nil if the process was not started with pipes.
func NewProcess(
	command string,
	cmd *exec.Cmd,
	stdin *CharacterOutputPort,
	stdout *CharacterInputPort,
	stderr *CharacterInputPort,
) *Process {
	return &Process{
		cmd:     cmd,
		command: command,
		stdin:   stdin,
		stdout:  stdout,
		stderr:  stderr,
	}
}

// Command returns the command name.
func (p *Process) Command() string {
	return p.command
}

// Cmd returns the underlying *exec.Cmd.
func (p *Process) Cmd() *exec.Cmd {
	return p.cmd
}

// Stdin returns the output port connected to the process stdin.
func (p *Process) Stdin() *CharacterOutputPort {
	return p.stdin
}

// Stdout returns the input port connected to the process stdout.
func (p *Process) Stdout() *CharacterInputPort {
	return p.stdout
}

// Stderr returns the input port connected to the process stderr.
func (p *Process) Stderr() *CharacterInputPort {
	return p.stderr
}

// SchemeString returns the Scheme external representation.
func (p *Process) SchemeString() string {
	if p.cmd != nil && p.cmd.Process != nil {
		return fmt.Sprintf(`#<process %q pid=%d>`, p.command, p.cmd.Process.Pid)
	}
	return fmt.Sprintf(`#<process %q>`, p.command)
}

// IsVoid returns false.
func (p *Process) IsVoid() bool {
	return false
}

// EqualTo returns true only for identity (same pointer).
func (p *Process) EqualTo(v Value) bool {
	return p == v
}
```

**Step 5: Run tests to verify they pass**

Run: `go test -v -run TestProcess ./values/...`
Expected: PASS

**Step 6: Commit**

```
feat(values): add Process type for subprocess handles
```

---

### Task 6: Create process extension with all primitives

**Files:**
- Create: `extensions/process/doc.go`
- Create: `extensions/process/register.go`
- Create: `extensions/process/prim_process.go`
- Create: `extensions/process/prim_process_test.go`

**Step 1: Write the failing tests**

Create `extensions/process/prim_process_test.go`:

```go
package process_test

import (
	"context"
	"runtime"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	extio "github.com/aalpar/wile/internal/extensions/io"
	extprocess "github.com/aalpar/wile/extensions/process"
	"github.com/aalpar/wile/values"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(extprocess.Extension),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

func engineEval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.EvalMultiple(context.Background(), code)
	qt.Assert(t, err, qt.IsNil)
	return result
}

func engineEvalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	expr, err := engine.Parse(context.Background(), code)
	if err != nil {
		return
	}
	_, err = engine.Eval(context.Background(), expr)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestSystem(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("system uses /bin/sh")
	}
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns zero for success", func(t *testing.T) {
		result := engineEval(t, engine, `(system "true")`)
		c.Assert(result.Internal().(*values.Integer).Value, qt.Equals, int64(0))
	})

	t.Run("returns nonzero for failure", func(t *testing.T) {
		result := engineEval(t, engine, `(system "false")`)
		c.Assert(result.Internal().(*values.Integer).Value, qt.Not(qt.Equals), int64(0))
	})

	t.Run("wrong type", func(t *testing.T) {
		engineEvalExpectError(t, engine, `(system 42)`)
	})
}

func TestProcessSpawn(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns a process", func(t *testing.T) {
		result := engineEval(t, engine, `
			(let ((p (process-spawn "echo" "hello")))
			  (process-wait p)
			  (process? p))
		`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("can read stdout", func(t *testing.T) {
		result := engineEval(t, engine, `
			(let ((proc (process-spawn "echo" "hello")))
			  (let ((line (read-line (process-stdout proc))))
			    (process-wait proc)
			    line))
		`)
		c.Assert(result.Internal().(*values.String).Value, qt.Equals, "hello")
	})

	t.Run("can write stdin and read stdout", func(t *testing.T) {
		result := engineEval(t, engine, `
			(let ((proc (process-spawn "cat")))
			  (display "ping" (process-stdin proc))
			  (close-output-port (process-stdin proc))
			  (let ((line (read-line (process-stdout proc))))
			    (process-wait proc)
			    line))
		`)
		c.Assert(result.Internal().(*values.String).Value, qt.Equals, "ping")
	})

	t.Run("process-wait returns exit code", func(t *testing.T) {
		result := engineEval(t, engine, `
			(let ((proc (process-spawn "true")))
			  (process-wait proc))
		`)
		c.Assert(result.Internal().(*values.Integer).Value, qt.Equals, int64(0))
	})

	t.Run("process-wait returns nonzero on failure", func(t *testing.T) {
		result := engineEval(t, engine, `
			(let ((proc (process-spawn "false")))
			  (process-wait proc))
		`)
		c.Assert(result.Internal().(*values.Integer).Value, qt.Not(qt.Equals), int64(0))
	})
}

func TestProcessKill(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix signals")
	}
	engine := newEngine(t)

	t.Run("kill terminates process", func(t *testing.T) {
		// process-wait should return after kill
		engineEval(t, engine, `
			(let ((proc (process-spawn "sleep" "60")))
			  (process-kill proc 'kill)
			  (process-wait proc))
		`)
		// If we get here without hanging, kill worked.
	})

	t.Run("term terminates process", func(t *testing.T) {
		engineEval(t, engine, `
			(let ((proc (process-spawn "sleep" "60")))
			  (process-kill proc 'term)
			  (process-wait proc))
		`)
	})

	t.Run("invalid signal", func(t *testing.T) {
		engineEvalExpectError(t, engine, `
			(let ((proc (process-spawn "sleep" "60")))
			  (process-kill proc 'bogus))
		`)
	})
}

func TestProcessPredicate(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("process? true for process", func(t *testing.T) {
		result := engineEval(t, engine, `
			(let ((proc (process-spawn "true")))
			  (process-wait proc)
			  (process? proc))
		`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("process? false for non-process", func(t *testing.T) {
		result := engineEval(t, engine, `(process? 42)`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v ./extensions/process/...`
Expected: FAIL — package doesn't exist yet.

**Step 3: Create package doc**

Create `extensions/process/doc.go`:

```go
// Package process provides subprocess execution primitives.
//
// This extension is NOT included in SafeExtensions(). Embedders must
// opt in explicitly with WithExtension(process.Extension).
//
// Two security actions gate process creation:
//   - security.ActionExec gates process-spawn (structured, no shell)
//   - security.ActionExecShell gates system (shell command string)
//
// Both use security.ResourceProcess with the command as target.
package process
```

**Step 4: Create register.go**

Create `extensions/process/register.go`:

```go
package process

import (
	"github.com/aalpar/wile/registry"
)

// Extension is the process execution extension.
var Extension = registry.NewExtension("process", AddToRegistry)

// Builder aggregates all process registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all process primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "system", ParamCount: 1, Impl: PrimSystem,
			Doc: "Runs a shell command string via /bin/sh -c. Returns exit code.",
			ParamNames: []string{"command"}, Category: "process"},
		{Name: "process-spawn", ParamCount: 2, IsVariadic: true, Impl: PrimProcessSpawn,
			Doc: "Starts a process with pipes. Returns a process object.",
			ParamNames: []string{"command", "args"}, Category: "process"},
		{Name: "process-stdout", ParamCount: 1, Impl: PrimProcessStdout,
			Doc: "Returns the stdout input port of a process.",
			ParamNames: []string{"process"}, Category: "process"},
		{Name: "process-stderr", ParamCount: 1, Impl: PrimProcessStderr,
			Doc: "Returns the stderr input port of a process.",
			ParamNames: []string{"process"}, Category: "process"},
		{Name: "process-stdin", ParamCount: 1, Impl: PrimProcessStdin,
			Doc: "Returns the stdin output port of a process.",
			ParamNames: []string{"process"}, Category: "process"},
		{Name: "process-wait", ParamCount: 1, Impl: PrimProcessWait,
			Doc: "Waits for a process to exit. Returns exit code.",
			ParamNames: []string{"process"}, Category: "process"},
		{Name: "process-kill", ParamCount: 2, Impl: PrimProcessKill,
			Doc: "Sends a signal to a process. Signal is a symbol: term, kill, int, hup.",
			ParamNames: []string{"process", "signal"}, Category: "process"},
		{Name: "process?", ParamCount: 1, Impl: PrimProcessQ,
			Doc: "Returns #t if the argument is a process object.",
			ParamNames: []string{"obj"}, Category: "process"},
	}, registry.PhaseRuntime)
	return nil
}
```

**Step 5: Create prim_process.go**

Create `extensions/process/prim_process.go`:

```go
package process

import (
	"os/exec"
	"syscall"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// PrimSystem implements the (system) primitive.
// Runs a shell command via /bin/sh -c and returns the exit code.
func PrimSystem(mc *machine.MachineContext) error {
	command, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "system")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceProcess,
		Action:   security.ActionExecShell,
		Target:   command.Value,
	})
	if err != nil {
		return err
	}
	cmd := exec.Command("/bin/sh", "-c", command.Value)
	runErr := cmd.Run()
	if runErr != nil {
		exitErr, ok := runErr.(*exec.ExitError)
		if ok {
			mc.SetValue(values.NewInteger(int64(exitErr.ExitCode())))
			return nil
		}
		return werr.WrapForeignProcessError(runErr, "system", command.Value)
	}
	mc.SetValue(values.NewInteger(0))
	return nil
}

// PrimProcessSpawn implements the (process-spawn) primitive.
// Creates a subprocess with stdin/stdout/stderr pipes.
func PrimProcessSpawn(mc *machine.MachineContext) error {
	command, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "process-spawn")
	if err != nil {
		return err
	}
	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceProcess,
		Action:   security.ActionExec,
		Target:   command.Value,
	})
	if err != nil {
		return err
	}

	// Collect string arguments from the rest list.
	var args []string
	rest := mc.Arg(1)
	tail, iterErr := values.ForEach(nil, rest, func(v values.Value) error {
		s, ok := v.(*values.String)
		if !ok {
			return werr.WrapForeignErrorf(
				werr.ErrNotAString,
				"process-spawn: argument is not a string: %T", v,
			)
		}
		args = append(args, s.Value)
		return nil
	})
	if iterErr != nil {
		return iterErr
	}
	if !values.IsEmptyList(tail) {
		return werr.WrapForeignErrorf(
			werr.ErrImproperList,
			"process-spawn: arguments must be a proper list",
		)
	}

	cmd := exec.Command(command.Value, args...)

	stdinPipe, err := cmd.StdinPipe()
	if err != nil {
		return werr.WrapForeignProcessError(err, "process-spawn", command.Value)
	}
	stdoutPipe, err := cmd.StdoutPipe()
	if err != nil {
		return werr.WrapForeignProcessError(err, "process-spawn", command.Value)
	}
	stderrPipe, err := cmd.StderrPipe()
	if err != nil {
		return werr.WrapForeignProcessError(err, "process-spawn", command.Value)
	}

	err = cmd.Start()
	if err != nil {
		return werr.WrapForeignProcessError(err, "process-spawn", command.Value)
	}

	proc := values.NewProcess(
		command.Value,
		cmd,
		values.NewCharacterOutputPortFromWriter(stdinPipe),
		values.NewCharacterInputPortFromReader(stdoutPipe),
		values.NewCharacterInputPortFromReader(stderrPipe),
	)
	mc.SetValue(proc)
	return nil
}

// PrimProcessStdout implements the (process-stdout) primitive.
func PrimProcessStdout(mc *machine.MachineContext) error {
	proc, err := helpers.RequireArg[*values.Process](mc, 0, werr.ErrNotAProcess, "process-stdout")
	if err != nil {
		return err
	}
	mc.SetValue(proc.Stdout())
	return nil
}

// PrimProcessStderr implements the (process-stderr) primitive.
func PrimProcessStderr(mc *machine.MachineContext) error {
	proc, err := helpers.RequireArg[*values.Process](mc, 0, werr.ErrNotAProcess, "process-stderr")
	if err != nil {
		return err
	}
	mc.SetValue(proc.Stderr())
	return nil
}

// PrimProcessStdin implements the (process-stdin) primitive.
func PrimProcessStdin(mc *machine.MachineContext) error {
	proc, err := helpers.RequireArg[*values.Process](mc, 0, werr.ErrNotAProcess, "process-stdin")
	if err != nil {
		return err
	}
	mc.SetValue(proc.Stdin())
	return nil
}

// PrimProcessWait implements the (process-wait) primitive.
// Blocks until the process exits and returns the exit code.
func PrimProcessWait(mc *machine.MachineContext) error {
	proc, err := helpers.RequireArg[*values.Process](mc, 0, werr.ErrNotAProcess, "process-wait")
	if err != nil {
		return err
	}
	waitErr := proc.Cmd().Wait()
	if waitErr != nil {
		exitErr, ok := waitErr.(*exec.ExitError)
		if ok {
			mc.SetValue(values.NewInteger(int64(exitErr.ExitCode())))
			return nil
		}
		return werr.WrapForeignProcessError(waitErr, "process-wait", proc.Command())
	}
	mc.SetValue(values.NewInteger(0))
	return nil
}

// signalMap maps Scheme signal symbols to OS signals.
var signalMap = map[string]syscall.Signal{
	"term": syscall.SIGTERM,
	"kill": syscall.SIGKILL,
	"int":  syscall.SIGINT,
	"hup":  syscall.SIGHUP,
}

// PrimProcessKill implements the (process-kill) primitive.
// Sends a signal to the process. Signal is a symbol: term, kill, int, hup.
func PrimProcessKill(mc *machine.MachineContext) error {
	proc, err := helpers.RequireArg[*values.Process](mc, 0, werr.ErrNotAProcess, "process-kill")
	if err != nil {
		return err
	}
	sigSym, err := helpers.RequireArg[*values.Symbol](mc, 1, werr.ErrNotASymbol, "process-kill")
	if err != nil {
		return err
	}
	sig, ok := signalMap[sigSym.Key()]
	if !ok {
		return werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"process-kill: unknown signal '%s' (expected term, kill, int, or hup)",
			sigSym.Key(),
		)
	}
	killErr := proc.Cmd().Process.Signal(sig)
	if killErr != nil {
		return werr.WrapForeignProcessError(killErr, "process-kill", proc.Command())
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimProcessQ implements the (process?) predicate.
func PrimProcessQ(mc *machine.MachineContext) error {
	_, ok := mc.Arg(0).(*values.Process)
	mc.SetValue(values.BoolToBoolean(ok))
	return nil
}
```

**Step 6: Run tests to verify they pass**

Run: `go test -v ./extensions/process/...`
Expected: PASS

**Step 7: Run lint**

Run: `make lint`
Expected: PASS

**Step 8: Commit**

```
feat(process): add process extension with spawn/wait/kill/system

New extension providing subprocess execution primitives:
- system: shell command via /bin/sh -c, returns exit code
- process-spawn: structured command+args, returns process object
- process-stdout/stderr/stdin: port accessors
- process-wait: blocks for exit, returns exit code
- process-kill: sends signal (term/kill/int/hup)
- process?: type predicate

Not in SafeExtensions — embedders opt in explicitly.
Gated by ActionExec (structured) and ActionExecShell (shell).
```

---

### Task 7: Verify full test suite and lint

**Step 1: Run all tests**

Run: `make test`
Expected: PASS — no regressions.

**Step 2: Run lint and covercheck**

Run: `make lint && make covercheck`
Expected: PASS

---

## Sentinel Checklist

New sentinels needed (verify they don't already exist before adding):
- [ ] `ErrNotAProcess` in `werr/werr.go`
- [ ] `ErrImproperList` — check if this exists; use it in `process-spawn` arg iteration

New security actions:
- [ ] `ActionExec` in `security/access.go`
- [ ] `ActionExecShell` in `security/access.go`

Existing sentinels reused:
- `ErrNotAString` — argument validation
- `ErrNotASymbol` — signal argument in `process-kill`
- `ErrInvalidArgument` — unknown signal name

## File Summary

| File | Action | Phase |
|------|--------|-------|
| `extensions/files/prim_directory.go` | Create | 1 |
| `extensions/files/prim_directory_test.go` | Create | 1 |
| `extensions/files/register.go` | Modify | 1 |
| `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` | Modify | 1 |
| `security/access.go` | Modify | 2 |
| `security/security_test.go` | Modify | 2 |
| `werr/werr.go` | Modify | 2 |
| `werr/werr_test.go` | Modify | 2 |
| `values/process.go` | Create | 2 |
| `values/process_test.go` | Create | 2 |
| `extensions/process/doc.go` | Create | 2 |
| `extensions/process/register.go` | Create | 2 |
| `extensions/process/prim_process.go` | Create | 2 |
| `extensions/process/prim_process_test.go` | Create | 2 |
