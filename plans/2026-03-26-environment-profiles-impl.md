# Environment Profiles Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Status:** 10/10 tasks complete (implemented on `promote-eval-extension`, PR #662)

> All ten tasks landed: Profile type (Tiny/Console/ConsoleWithLoad/Small/KitchenSink), ConsoleAuthorizer, ConsoleWithLoadAuthorizer, Sandbox modifier, envvars split out of system, virtual env map (`WithEnv`/`WithEnvMap`), retirement of `SafeExtensions`/`AllExtensions`/`WithSafeExtensions`/`WithAllExtensions`, `environment_tiny.go` → `bootstrap.go` rename, Scheme-level `(environment '(wile <profile>))` via `eval.ProfileFactory` callback, integration tests, and documentation updates. This file is retained as a historical record of the work.

**Goal:** Replace the SafeExtensions/AllExtensions API with named profiles (Tiny, Console, Small, KitchenSink), an orthogonal sandbox modifier, and a virtual environment map for capability-oriented configuration.

**Architecture:** Profile enum maps to extension lists + optional authorizer. Sandbox composes via `security.All()` (most-restrictive-wins). Virtual env map lives on `engineConfig`, flows to envvars primitives via namespace. `(environment '(wile tiny))` etc. recognized in `PrimEnvironment`.

**Tech Stack:** Go, existing `security.Authorizer` interface, `registry.Extension` pattern.

**Design Doc:** `plans/2026-03-26-environment-profiles.md`

**Prerequisite status (2026-04-17):** The `eval` extension was promoted out of
`internal/extensions/eval/` to public `extensions/eval/` (separate work,
satisfying the wile-goast requirement noted in `TODO.md`). All references in
this plan use the public path `github.com/aalpar/wile/extensions/eval`. The
`exteval` import alias is no longer needed -- import as plain `eval`.

**Embedder composition pattern (wile-goast and similar callers):** This plan
ships `ConsoleWithLoad` as a baked profile because the naive composition

```go
// WRONG -- (eval ...) works but (load ...) fails: ConsoleAuthorizer denies code:load
wile.NewEngine(ctx, wile.WithProfile(wile.Console), wile.WithExtension(eval.Extension))
```

does not work. `ConsoleAuthorizer` blocks `code:load` (the gate used by the
file resolvers when `(load ...)`/`include`/library-import reads from disk),
so adding `eval.Extension` alone leaves `(load ...)` unusable. The fix is
not extension composition but a different authorizer that allows `code:load`
under the same `/tmp` envelope as files. That coupling is what makes
`ConsoleWithLoad` a distinct profile rather than a one-line append.

Correct usage:

```go
eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.ConsoleWithLoad))
```

Embedders that want a different envelope (e.g. `/var/wile/scripts` instead
of `/tmp`) should still use `WithProfile(Console) + WithAuthorizer(custom)`.

---

### Task 1: Add Profile Type and Constants

**Files:**
- Create: `profile.go`
- Test: `profile_test.go`

**Step 1: Write the failing test**

```go
// profile_test.go
package wile

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestProfile_String(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		profile Profile
		want    string
	}{
		{Tiny, "tiny"},
		{Console, "console"},
		{ConsoleWithLoad, "console-with-load"},
		{Small, "small"},
		{KitchenSink, "kitchen-sink"},
	}
	for _, tt := range tests {
		c.Run(tt.want, func(c *qt.C) {
			c.Assert(tt.profile.String(), qt.Equals, tt.want)
		})
	}
}

func TestProfile_Extensions_Tiny(t *testing.T) {
	c := qt.New(t)
	exts := Tiny.extensions()
	c.Assert(exts, qt.HasLen, 0)
}

func TestProfile_Extensions_ConsoleWithLoad_HasEval(t *testing.T) {
	c := qt.New(t)
	consoleExts := Console.extensions()
	cwlExts := ConsoleWithLoad.extensions()
	// ConsoleWithLoad is Console + eval -- exactly one more extension
	c.Assert(len(cwlExts), qt.Equals, len(consoleExts)+1)
}

func TestProfile_Extensions_KitchenSink(t *testing.T) {
	c := qt.New(t)
	exts := KitchenSink.extensions()
	c.Assert(len(exts) > 0, qt.IsTrue)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestProfile ./...`
Expected: FAIL -- `Profile` type not defined

**Step 3: Write minimal implementation**

```go
// profile.go
package wile

import (
	"github.com/aalpar/wile/extensions/eval"
	"github.com/aalpar/wile/extensions/files"
	"github.com/aalpar/wile/extensions/gointerop"
	"github.com/aalpar/wile/extensions/introspection"
	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/extensions/process"
	"github.com/aalpar/wile/extensions/system"
	"github.com/aalpar/wile/extensions/threads"
	"github.com/aalpar/wile/internal/extensions/all"
	ioext "github.com/aalpar/wile/internal/extensions/io"
	nsext "github.com/aalpar/wile/internal/extensions/namespace"
	"github.com/aalpar/wile/registry"
)

// Profile identifies a named environment configuration.
// Each profile defines which extensions are loaded and what
// authorization constraints apply.
type Profile int

const (
	// Tiny is a pure computational Scheme -- core primitives only.
	// No I/O, no filesystem, no threads. LCD of all profiles.
	Tiny Profile = iota

	// Console adds I/O and sandboxed file access to Tiny.
	// All port primitives work. File operations restricted to /tmp.
	// stdin/stdout/stderr available. Environment variables read from
	// virtual env map only (no os.Getenv fallthrough).
	Console

	// ConsoleWithLoad is Console plus the eval extension, with an
	// authorizer that allows `code:load` under /tmp (in addition to
	// file r/w/d under /tmp). Enables (eval ...) and (load ...) within
	// the same /tmp security envelope. Process execution still denied.
	// Primary consumer: wile-goast and similar embedders that stage
	// Scheme files into /tmp and load them.
	ConsoleWithLoad

	// Small is R7RS-small complete -- all 16 (scheme ...) libraries.
	// Includes file I/O, system interface. No threads, no Go interop.
	Small

	// KitchenSink includes every available extension: threads, Go interop,
	// process execution, namespace manipulation.
	KitchenSink
)

// String returns the kebab-case name of the profile.
func (p Profile) String() string {
	switch p {
	case Tiny:
		return "tiny"
	case Console:
		return "console"
	case ConsoleWithLoad:
		return "console-with-load"
	case Small:
		return "small"
	case KitchenSink:
		return "kitchen-sink"
	default:
		return "unknown"
	}
}

// extensions returns the registry extensions for this profile.
// Tiny returns nil (core only). Each successive profile is a superset.
func (p Profile) extensions() []registry.Extension {
	switch p {
	case Tiny:
		return nil
	case Console:
		return []registry.Extension{
			ioext.Extension,
			files.Extension,
			math.Extension,
			all.SafeExtension,
			// envvars.Extension added in Task 4
		}
	case ConsoleWithLoad:
		return []registry.Extension{
			ioext.Extension,
			files.Extension,
			math.Extension,
			all.SafeExtension,
			eval.Extension,
			// envvars.Extension added in Task 4
		}
	case Small:
		return []registry.Extension{
			ioext.Extension,
			files.Extension,
			math.Extension,
			introspection.Extension,
			eval.Extension,
			all.Extension,
			system.Extension,
			// envvars.Extension added in Task 4
		}
	case KitchenSink:
		return []registry.Extension{
			ioext.Extension,
			files.Extension,
			math.Extension,
			introspection.Extension,
			eval.Extension,
			nsext.Extension,
			threads.Extension,
			gointerop.Extension,
			all.Extension,
			system.Extension,
			process.Extension,
			// envvars.Extension added in Task 4
		}
	default:
		return nil
	}
}

// authorizer returns the built-in authorizer for this profile, or nil.
// Console bakes in a /tmp-only file authorizer. Other profiles have
// no built-in restrictions.
func (p Profile) authorizer() security.Authorizer {
	// Stub: Console authorizer added in Task 2
	return nil
}

// WithProfile configures the engine with the named profile's
// extensions and authorization constraints.
func WithProfile(p Profile) EngineOption {
	return func(cfg *engineConfig) {
		cfg.extensions = append(cfg.extensions, p.extensions()...)
		auth := p.authorizer()
		if auth != nil {
			cfg.authorizer = auth
		}
	}
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestProfile ./...`
Expected: PASS

**Step 5: Commit**

```
feat(profiles): add Profile type with Tiny, Console, Small, KitchenSink constants
```

---

### Task 2: Console + ConsoleWithLoad Authorizers

**Files:**
- Create: `security/console_authorizer.go`
- Test: `security/console_authorizer_test.go`
- Create: `security/console_with_load_authorizer.go`
- Test: `security/console_with_load_authorizer_test.go`
- Modify: `profile.go` -- wire Console and ConsoleWithLoad authorizers

**Step 1: Write the failing test**

```go
// security/console_authorizer_test.go
package security_test

import (
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/aalpar/wile/security"
)

func TestConsoleAuthorizer(t *testing.T) {
	c := qt.New(t)
	auth := security.ConsoleAuthorizer()

	tests := []struct {
		name    string
		req     security.AccessRequest
		allowed bool
	}{
		{"read /tmp/foo", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/tmp/foo"}, true},
		{"write /tmp/bar", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionWrite, Target: "/tmp/bar"}, true},
		{"delete /tmp/baz", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionDelete, Target: "/tmp/baz"}, true},
		{"read /tmp subdir", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/tmp/sub/dir/file"}, true},
		{"read /etc/passwd", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/etc/passwd"}, false},
		{"write /home/user", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionWrite, Target: "/home/user/file"}, false},
		{"read env", security.AccessRequest{Resource: security.ResourceEnv, Action: security.ActionRead, Target: "APP_MODE"}, true},
		{"load code", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "file.scm"}, false},
		{"exec process", security.AccessRequest{Resource: security.ResourceProcess, Action: security.ActionExec, Target: "ls"}, false},
		{"path traversal /tmp/../etc", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/tmp/../etc/passwd"}, false},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			err := auth.Authorize(tt.req)
			if tt.allowed {
				c.Assert(err, qt.IsNil)
			} else {
				c.Assert(err, qt.IsNotNil)
			}
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestConsoleAuthorizer ./security/...`
Expected: FAIL -- `ConsoleAuthorizer` not defined

**Step 3: Write minimal implementation**

```go
// security/console_authorizer.go
package security

import (
	"path/filepath"
	"strings"
)

// ConsoleAuthorizer returns an Authorizer for the Console profile.
// File operations are restricted to /tmp. Environment variable reads
// are allowed (the envvars primitive handles virtual-vs-OS routing).
// Code loading and process execution are denied.
func ConsoleAuthorizer() Authorizer {
	return AuthorizerFunc(func(req AccessRequest) error {
		switch req.Resource {
		case ResourceFile:
			cleaned := filepath.Clean(req.Target)
			if !strings.HasPrefix(cleaned, "/tmp/") && cleaned != "/tmp" {
				return ErrAccessDenied
			}
			return nil
		case ResourceEnv:
			return nil
		default:
			return ErrAccessDenied
		}
	})
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestConsoleAuthorizer ./security/...`
Expected: PASS

**Step 5: Write the failing test for ConsoleWithLoadAuthorizer**

```go
// security/console_with_load_authorizer_test.go
package security_test

import (
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/aalpar/wile/security"
)

func TestConsoleWithLoadAuthorizer(t *testing.T) {
	c := qt.New(t)
	auth := security.ConsoleWithLoadAuthorizer()

	tests := []struct {
		name    string
		req     security.AccessRequest
		allowed bool
	}{
		// File ops: same /tmp envelope as Console
		{"read /tmp/foo", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/tmp/foo"}, true},
		{"write /tmp/bar", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionWrite, Target: "/tmp/bar"}, true},
		{"delete /tmp/baz", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionDelete, Target: "/tmp/baz"}, true},
		{"read /etc/passwd", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/etc/passwd"}, false},
		// Env: same as Console
		{"read env", security.AccessRequest{Resource: security.ResourceEnv, Action: security.ActionRead, Target: "APP_MODE"}, true},
		// Code load: NEW capability vs Console -- allowed under /tmp
		{"load /tmp/lib.scm", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "/tmp/lib.scm"}, true},
		{"load /tmp/sub/lib.scm", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "/tmp/sub/lib.scm"}, true},
		{"load /etc/lib.scm", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "/etc/lib.scm"}, false},
		{"load path traversal /tmp/../etc/lib.scm", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "/tmp/../etc/lib.scm"}, false},
		// Process: still denied
		{"exec process", security.AccessRequest{Resource: security.ResourceProcess, Action: security.ActionExec, Target: "ls"}, false},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			err := auth.Authorize(tt.req)
			if tt.allowed {
				c.Assert(err, qt.IsNil)
			} else {
				c.Assert(err, qt.IsNotNil)
			}
		})
	}
}
```

**Step 6: Run test to verify it fails**

Run: `go test -v -run TestConsoleWithLoadAuthorizer ./security/...`
Expected: FAIL -- `ConsoleWithLoadAuthorizer` not defined

**Step 7: Write ConsoleWithLoadAuthorizer**

```go
// security/console_with_load_authorizer.go
package security

import (
	"path/filepath"
	"strings"
)

// ConsoleWithLoadAuthorizer returns an Authorizer for the ConsoleWithLoad
// profile. File operations and code loading are both restricted to /tmp.
// Environment variable reads are allowed. Process execution is denied.
//
// This is the security envelope wile-goast and similar embedders use to
// run sandboxed (eval ...) and (load ...) on Scheme files staged in /tmp.
func ConsoleWithLoadAuthorizer() Authorizer {
	return AuthorizerFunc(func(req AccessRequest) error {
		switch req.Resource {
		case ResourceFile, ResourceCode:
			cleaned := filepath.Clean(req.Target)
			if !strings.HasPrefix(cleaned, "/tmp/") && cleaned != "/tmp" {
				return ErrAccessDenied
			}
			return nil
		case ResourceEnv:
			return nil
		default:
			return ErrAccessDenied
		}
	})
}
```

**Step 8: Run test to verify it passes**

Run: `go test -v -run TestConsoleWithLoadAuthorizer ./security/...`
Expected: PASS

**Step 9: Wire authorizers to profiles**

Update `profile.go` -- replace the stub `authorizer()` method:

```go
func (p Profile) authorizer() security.Authorizer {
	switch p {
	case Console:
		return security.ConsoleAuthorizer()
	case ConsoleWithLoad:
		return security.ConsoleWithLoadAuthorizer()
	default:
		return nil
	}
}
```

**Step 10: Commit**

```
feat(security): add Console and ConsoleWithLoad authorizers (/tmp-only)
```

---

### Task 3: Sandbox Modifier

**Files:**
- Create: `security/sandbox_authorizer.go`
- Create: `sandbox.go`
- Test: `security/sandbox_authorizer_test.go`
- Test: `sandbox_test.go`

**Step 1: Write the failing test for sandbox authorizer**

```go
// security/sandbox_authorizer_test.go
package security_test

import (
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/aalpar/wile/security"
)

func TestSandboxAuthorizer_DefaultPrefix(t *testing.T) {
	c := qt.New(t)
	auth := security.SandboxAuthorizer("WILE_")

	tests := []struct {
		name    string
		req     security.AccessRequest
		allowed bool
	}{
		{"read file", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionRead, Target: "/any/file"}, true},
		{"write file", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionWrite, Target: "/any/file"}, false},
		{"delete file", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionDelete, Target: "/any/file"}, false},
		{"stat file", security.AccessRequest{Resource: security.ResourceFile, Action: security.ActionStat, Target: "/any/file"}, true},
		{"read env WILE_MODE", security.AccessRequest{Resource: security.ResourceEnv, Action: security.ActionRead, Target: "WILE_MODE"}, true},
		{"read env HOME", security.AccessRequest{Resource: security.ResourceEnv, Action: security.ActionRead, Target: "HOME"}, false},
		{"load code", security.AccessRequest{Resource: security.ResourceCode, Action: security.ActionLoad, Target: "file.scm"}, false},
		{"exec process", security.AccessRequest{Resource: security.ResourceProcess, Action: security.ActionExec, Target: "ls"}, false},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			err := auth.Authorize(tt.req)
			if tt.allowed {
				c.Assert(err, qt.IsNil)
			} else {
				c.Assert(err, qt.IsNotNil)
			}
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestSandboxAuthorizer ./security/...`
Expected: FAIL -- `SandboxAuthorizer` not defined

**Step 3: Write sandbox authorizer**

```go
// security/sandbox_authorizer.go
package security

import "strings"

// SandboxAuthorizer returns an Authorizer that allows read-only file
// access, env reads with a prefix filter, and denies code loading
// and process execution.
func SandboxAuthorizer(envPrefix string) Authorizer {
	return AuthorizerFunc(func(req AccessRequest) error {
		switch req.Resource {
		case ResourceFile:
			if req.Action == ActionRead || req.Action == ActionStat {
				return nil
			}
			return ErrAccessDenied
		case ResourceEnv:
			if req.Action == ActionRead && strings.HasPrefix(req.Target, envPrefix) {
				return nil
			}
			return ErrAccessDenied
		default:
			return ErrAccessDenied
		}
	})
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v -run TestSandboxAuthorizer ./security/...`
Expected: PASS

**Step 5: Write WithSandbox API**

```go
// sandbox.go
package wile

import "github.com/aalpar/wile/security"

// SandboxOption configures the sandbox modifier.
type SandboxOption func(*sandboxConfig)

type sandboxConfig struct {
	envPrefix string
}

// SandboxEnvPrefix sets the environment variable prefix that the
// sandbox allows reading. Default is "WILE_".
func SandboxEnvPrefix(prefix string) SandboxOption {
	return func(cfg *sandboxConfig) {
		cfg.envPrefix = prefix
	}
}

// WithSandbox layers a restrictive authorizer on top of any profile.
// File writes are denied. Environment variable reads are prefix-filtered
// (default "WILE_"). Code loading and process execution are denied.
//
// When composed with a profile's built-in authorizer, the result is
// the intersection (most-restrictive-wins) via security.All().
func WithSandbox(opts ...SandboxOption) EngineOption {
	scfg := &sandboxConfig{envPrefix: "WILE_"}
	for _, opt := range opts {
		opt(scfg)
	}

	sandboxAuth := security.SandboxAuthorizer(scfg.envPrefix)

	return func(cfg *engineConfig) {
		if cfg.authorizer != nil {
			cfg.authorizer = security.All(cfg.authorizer, sandboxAuth)
		} else {
			cfg.authorizer = sandboxAuth
		}
	}
}
```

**Step 6: Write integration test**

```go
// sandbox_test.go
package wile

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestWithSandbox_DefaultPrefix(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithProfile(Small),
		WithSandbox(),
	)
	c.Assert(err, qt.IsNil)
	c.Assert(eng, qt.IsNotNil)
}

func TestWithSandbox_CustomPrefix(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithProfile(Small),
		WithSandbox(SandboxEnvPrefix("MYAPP_")),
	)
	c.Assert(err, qt.IsNil)
	c.Assert(eng, qt.IsNotNil)
}
```

**Step 7: Run tests**

Run: `go test -v -run TestWithSandbox ./...`
Expected: PASS

**Step 8: Commit**

```
feat(sandbox): add WithSandbox modifier with configurable env prefix
```

---

### Task 4: Split envvars Extension from system

**Files:**
- Create: `internal/extensions/envvars/doc.go`
- Create: `internal/extensions/envvars/register.go`
- Create: `internal/extensions/envvars/prim_envvars.go`
- Test: `internal/extensions/envvars/prim_envvars_test.go`
- Modify: `extensions/system/register.go` -- remove env var registrations (lines 39-42)
- Modify: `extensions/system/prim_system.go` -- remove env var implementations (lines 101-147)
- Modify: `profile.go` -- add envvars.Extension to Console, Small, KitchenSink

**Step 1: Create the envvars package**

Create `internal/extensions/envvars/doc.go`:

```go
// Package envvars provides environment variable access primitives.
//
// This extension is split from the system extension because environment
// variable access is a configuration concern, not a system interface.
// Console profile includes this for capability-oriented configuration
// via virtual env maps.
//
// Primitives:
//   - get-environment-variable: look up a single variable
//   - get-environment-variables: return alist of all visible variables
package envvars
```

**Step 2: Create register.go**

```go
// internal/extensions/envvars/register.go
package envvars

import "github.com/aalpar/wile/registry"

// Builder aggregates envvars primitive registration.
var Builder = registry.RegistryBuilder{
	AddPrimitiveFn: addPrimitives,
}

// Extension is the envvars extension for use with WithExtension.
var Extension = registry.NewExtension("envvars", Builder)

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{Name: "get-environment-variable", ParamCount: 1, Impl: PrimGetEnvironmentVariable,
			Doc: "Returns the value of the named environment variable, or #f.",
			ParamNames: []string{"name"}, Category: "envvars"},
		{Name: "get-environment-variables", Impl: PrimGetEnvironmentVariables,
			Doc: "Returns an alist of all visible environment variables.",
			Category: "envvars"},
	}, registry.PhaseRuntime)
	return nil
}
```

**Step 3: Create prim_envvars.go**

Move implementations from `extensions/system/prim_system.go` (lines 101-147).
Add virtual env map check. The `EnvMap()` method on Namespace is added in Task 5;
for now the method must exist but can return nil (add a stub to `environment/namespace.go`).

```go
// internal/extensions/envvars/prim_envvars.go
package envvars

import (
	"os"
	"strings"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/helpers"
	"github.com/aalpar/wile/werr"
)

// PrimGetEnvironmentVariable implements (get-environment-variable name).
// Checks the virtual env map first; falls through to os.Getenv if no
// virtual map is set, subject to authorizer.
func PrimGetEnvironmentVariable(mc *machine.MachineContext) error {
	name, err := helpers.RequireType[*values.String](
		mc.Arg(0), werr.ErrNotAString, "get-environment-variable")
	if err != nil {
		return err
	}

	envMap := mc.EnvironmentFrame().Namespace().EnvMap()
	if envMap != nil {
		val, ok := envMap[name.Value]
		if ok {
			mc.SetValue(values.NewString(val))
		} else {
			mc.SetValue(values.FalseValue)
		}
		return nil
	}

	err = security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceEnv,
		Action:   security.ActionRead,
		Target:   name.Value,
	})
	if err != nil {
		return werr.WrapForeignErrorf(err,
			"get-environment-variable: access denied for %q", name.Value)
	}

	val, ok := os.LookupEnv(name.Value)
	if ok {
		mc.SetValue(values.NewString(val))
	} else {
		mc.SetValue(values.FalseValue)
	}
	return nil
}

// PrimGetEnvironmentVariables implements (get-environment-variables).
// Returns an alist of all visible environment variables.
func PrimGetEnvironmentVariables(mc *machine.MachineContext) error {
	envMap := mc.EnvironmentFrame().Namespace().EnvMap()
	if envMap != nil {
		q := values.Value(values.EmptyList)
		for k, v := range envMap {
			pair := values.Cons(values.NewString(k), values.NewString(v))
			q = values.Cons(pair, q)
		}
		mc.SetValue(q)
		return nil
	}

	err := security.CheckWithAuthorizer(mc.Authorizer(), security.AccessRequest{
		Resource: security.ResourceEnv,
		Action:   security.ActionRead,
		Target:   "*",
	})
	if err != nil {
		return werr.WrapForeignErrorf(err, "get-environment-variables: access denied")
	}

	q := values.Value(values.EmptyList)
	for _, envStr := range os.Environ() {
		parts := strings.SplitN(envStr, "=", 2)
		if len(parts) == 2 {
			pair := values.Cons(values.NewString(parts[0]), values.NewString(parts[1]))
			q = values.Cons(pair, q)
		}
	}
	mc.SetValue(q)
	return nil
}
```

**Step 4: Add EnvMap stub to Namespace**

Add to `environment/namespace.go` (temporary stub, fully wired in Task 5):

```go
// EnvMap returns the virtual environment variable map, or nil
// if no virtual map was configured.
func (p *Namespace) EnvMap() map[string]string {
	return p.envMap
}
```

And add `envMap map[string]string` field to the `Namespace` struct.

**Step 5: Write tests**

```go
// internal/extensions/envvars/prim_envvars_test.go
package envvars_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/aalpar/wile"
	"github.com/aalpar/wile/internal/extensions/envvars"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithExtension(envvars.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return eng
}

func TestGetEnvironmentVariable_OSFallthrough(t *testing.T) {
	c := qt.New(t)
	eng := newEngine(t)
	t.Setenv("WILE_TEST_VAR", "hello")

	result, err := eng.EvalString(context.Background(),
		`(get-environment-variable "WILE_TEST_VAR")`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, `"hello"`)
}

func TestGetEnvironmentVariable_NotFound(t *testing.T) {
	c := qt.New(t)
	eng := newEngine(t)

	result, err := eng.EvalString(context.Background(),
		`(get-environment-variable "WILE_NONEXISTENT_12345")`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, "#f")
}
```

**Step 6: Remove env var primitives from system extension**

In `extensions/system/register.go`: delete the two spec entries for
`get-environment-variable` and `get-environment-variables`.

In `extensions/system/prim_system.go`: delete `PrimGetEnvironmentVariable`
(lines 101-123) and `PrimGetEnvironmentVariables` (lines 125-147).

Move any system-package tests for env vars to
`internal/extensions/envvars/prim_envvars_test.go`.

**Step 7: Add envvars to profile extension lists**

In `profile.go`, add `envvars.Extension` to Console, Small, and KitchenSink
extension slices (where the `// envvars.Extension added in Task 4` comments are).

**Step 8: Run all tests**

Run: `make test`
Expected: PASS

**Step 9: Run linter**

Run: `make lint`
Expected: PASS

**Step 10: Commit**

```
refactor(envvars): split environment variable primitives from system extension
```

---

### Task 5: Virtual Environment Map

**Files:**
- Modify: `environment/namespace.go` -- add `SetEnvMap()` method
- Modify: `options.go` -- add `WithEnv`, `WithEnvMap`, add `envMap` to `engineConfig`
- Modify: `engine.go` -- wire `cfg.envMap` to namespace
- Modify: `profile.go` -- Console ensures empty envMap for no-OS-fallthrough
- Test: `engine_envmap_test.go` (new)

**Step 1: Write the failing test**

```go
// engine_envmap_test.go
package wile

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestWithEnv_SingleVar(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithProfile(Console),
		WithEnv("APP_MODE", "test"),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalString(ctx,
		`(get-environment-variable "APP_MODE")`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, `"test"`)
}

func TestWithEnv_NotFound(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithProfile(Console),
		WithEnv("APP_MODE", "test"),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalString(ctx,
		`(get-environment-variable "NOPE")`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, "#f")
}

func TestWithEnvMap(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithProfile(Console),
		WithEnvMap(map[string]string{
			"DB_HOST": "localhost",
			"DB_PORT": "5432",
		}),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalString(ctx,
		`(get-environment-variable "DB_HOST")`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, `"localhost"`)
}

func TestWithEnv_ConsoleNoOSFallthrough(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	t.Setenv("HOME", "/home/test")

	eng, err := NewEngine(ctx,
		WithProfile(Console),
		WithEnvMap(map[string]string{}),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalString(ctx,
		`(get-environment-variable "HOME")`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, "#f")
}

func TestWithEnv_GetEnvironmentVariables(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithProfile(Console),
		WithEnvMap(map[string]string{"K": "V"}),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalString(ctx,
		`(get-environment-variables)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Contains, "K")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestWithEnv ./...`
Expected: FAIL -- `WithEnv` not defined

**Step 3: Add SetEnvMap to Namespace**

In `environment/namespace.go`, add (EnvMap already added in Task 4 stub):

```go
// SetEnvMap sets the virtual environment variable map.
// When set, envvars primitives read from this map instead of os.Getenv.
func (p *Namespace) SetEnvMap(m map[string]string) {
	p.envMap = m
}
```

**Step 4: Add WithEnv/WithEnvMap to options.go**

Add `envMap map[string]string` to `engineConfig` struct (around line 49-60).

```go
// WithEnv adds a single virtual environment variable.
// When any virtual env var is set, the envvars extension reads from
// the virtual map instead of os.Getenv.
func WithEnv(key, value string) EngineOption {
	return func(cfg *engineConfig) {
		if cfg.envMap == nil {
			cfg.envMap = make(map[string]string)
		}
		cfg.envMap[key] = value
	}
}

// WithEnvMap sets the complete virtual environment variable map.
// Replaces any previously set virtual env vars.
func WithEnvMap(m map[string]string) EngineOption {
	return func(cfg *engineConfig) {
		cfg.envMap = make(map[string]string, len(m))
		for k, v := range m {
			cfg.envMap[k] = v
		}
	}
}
```

**Step 5: Wire envMap in engine.go**

In `NewEngine` (around line 161) and `NewNamespace` (around line 96),
after `ns = environment.NewNamespace()`:

```go
if cfg.envMap != nil {
	ns.SetEnvMap(cfg.envMap)
}
```

**Step 6: Sandboxed profiles ensure non-nil envMap**

In `profile.go`, update `WithProfile` to ensure Console and ConsoleWithLoad
always have a virtual env map so the envvars primitive never falls through
to `os.Getenv`:

```go
func WithProfile(p Profile) EngineOption {
	return func(cfg *engineConfig) {
		cfg.extensions = append(cfg.extensions, p.extensions()...)
		auth := p.authorizer()
		if auth != nil {
			cfg.authorizer = auth
		}
		if (p == Console || p == ConsoleWithLoad) && cfg.envMap == nil {
			cfg.envMap = make(map[string]string)
		}
	}
}
```

**Step 7: Run tests**

Run: `go test -v -run TestWithEnv ./...`
Expected: PASS

**Step 8: Commit**

```
feat(envmap): add WithEnv/WithEnvMap for virtual environment variables
```

---

### Task 6: Delete Old API and Update Call Sites

**Files:**
- Modify: `options.go` -- remove SafeExtensions, WithSafeExtensions, AllExtensions, WithAllExtensions (lines 224-324)
- Modify: `doc.go` -- update examples (lines 29-35)
- Modify: `cmd/wile/main.go` -- WithAllExtensions -> WithProfile(KitchenSink)
- Modify: ~20 test files (see list below)

**Step 1: Remove old functions from options.go**

Delete `SafeExtensions()` (lines 257-264), `WithSafeExtensions()` (lines 273-279),
`AllExtensions()` (lines 295-309), `WithAllExtensions()` (lines 318-324),
and their doc comments.

Remove any extension imports that become unused after deletion. Most will
still be used by `profile.go`.

**Step 2: Update cmd/wile/main.go**

Replace `wile.WithAllExtensions()` with `wile.WithProfile(wile.KitchenSink)`.

**Step 3: Update test files**

Replace across all files:

| Old | New |
|-----|-----|
| `wile.WithAllExtensions()` | `wile.WithProfile(wile.KitchenSink)` |
| `wile.WithSafeExtensions()` | `wile.WithProfile(wile.Console)` |

Files to update:

- `engine_all_extensions_test.go`
- `engine_sandbox_test.go`
- `engine_stdlib_test.go`
- `callcc_engine_test.go`
- `fs_source_test.go`
- `wile_test.go`
- `example_test.go`
- `integration/callcc_callback_test.go`
- `integration/circular_test.go`
- `extensions/eval/load_path_integration_test.go`
- `doc.go`

For `wile_test.go` functions `TestWithSafeExtensions` and `TestSafeExtensions`:
rename to `TestWithProfile_Console` and update to use `WithProfile(Console)`.

For `engine_sandbox_test.go`: `TestSafeEngine_RejectsPrivileged` and
`TestSafeEngine_AllowsSafe` should be renamed to `TestConsole_Rejects...`
and `TestConsole_Allows...`, updated to use `WithProfile(Console)`.
Review which primitives are expected to be rejected/allowed -- Console includes
more extensions than old SafeExtensions (adds files with /tmp restriction).

**Step 4: Run all tests**

Run: `make test`
Expected: PASS

**Step 5: Run linter**

Run: `make lint`
Expected: PASS (no unused imports, no dead code)

**Step 6: Commit**

```
refactor(api): replace SafeExtensions/AllExtensions with WithProfile
```

---

### Task 7: Rename environment_tiny.go

**Files:**
- Rename: `internal/bootstrap/environment_tiny.go` -> `internal/bootstrap/bootstrap.go`
- Rename: `internal/bootstrap/environment_tiny_test.go` -> `internal/bootstrap/bootstrap_test.go`
- Modify: `internal/bootstrap/CLAUDE.local.md` -- update file references

**Step 1: Rename files**

```bash
git mv internal/bootstrap/environment_tiny.go internal/bootstrap/bootstrap.go
git mv internal/bootstrap/environment_tiny_test.go internal/bootstrap/bootstrap_test.go
```

**Step 2: Rename exported function**

In `bootstrap.go`, rename `NewNamespaceFrameTiny` -> `NewNamespaceFrame`.

Find all references:

```bash
grep -r "NewNamespaceFrameTiny" --include="*.go"
```

Update each call site.

**Step 3: Remove stale allExtensions var**

The `allExtensions` package-level variable (line 66-78) is no longer the source
of truth -- extension selection is driven by `profile.go` and `engineConfig.extensions`.

If `initializeEnvironmentWithRegistry` uses `allExtensions` as default when
`exts` is nil (line 85-87), change the nil-default to be explicit: callers
must always pass extensions. This prevents accidentally loading everything.

**Step 4: Update CLAUDE.local.md**

Update `internal/bootstrap/CLAUDE.local.md` to reference `bootstrap.go` and
`NewNamespaceFrame`.

**Step 5: Run tests**

Run: `make test`
Expected: PASS

**Step 6: Commit**

```
refactor(bootstrap): rename environment_tiny to bootstrap
```

---

### Task 8: Scheme-Level (environment '(wile ...)) Support

**Files:**
- Modify: `extensions/eval/prim_eval.go` -- update `PrimEnvironment` (line 281)
- Modify: `internal/bootstrap/bootstrap.go` -- expose profile environment factory
- Test: `extensions/eval/prim_eval_test.go`

**Step 1: Write the failing test**

Add to `extensions/eval/prim_eval_test.go`:

```go
func TestEnvironment_WileProfiles(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("wile tiny has arithmetic", func(t *testing.T) {
		result := evalExpr(t, engine,
			`(eval '(+ 1 2) (environment '(wile tiny)))`)
		c.Assert(result.String(), qt.Equals, "3")
	})

	t.Run("wile tiny no io", func(t *testing.T) {
		evalExpectError(t, engine,
			`(eval '(display "hi") (environment '(wile tiny)))`)
	})

	t.Run("wile console", func(t *testing.T) {
		result := evalExpr(t, engine,
			`(eval '(+ 1 2) (environment '(wile console)))`)
		c.Assert(result.String(), qt.Equals, "3")
	})

	t.Run("wile console-with-load has eval", func(t *testing.T) {
		result := evalExpr(t, engine,
			`(eval '(+ 1 2) (environment '(wile console-with-load)))`)
		c.Assert(result.String(), qt.Equals, "3")
	})

	t.Run("wile console-with-load supports nested eval", func(t *testing.T) {
		// eval-of-eval works: ConsoleWithLoad provides eval.Extension
		result := evalExpr(t, engine,
			`(eval '(eval '(+ 2 3) (environment '(wile tiny))) (environment '(wile console-with-load)))`)
		c.Assert(result.String(), qt.Equals, "5")
	})

	t.Run("wile small", func(t *testing.T) {
		result := evalExpr(t, engine,
			`(eval '(+ 1 2) (environment '(wile small)))`)
		c.Assert(result.String(), qt.Equals, "3")
	})

	t.Run("wile kitchen-sink", func(t *testing.T) {
		result := evalExpr(t, engine,
			`(eval '(+ 1 2) (environment '(wile kitchen-sink)))`)
		c.Assert(result.String(), qt.Equals, "3")
	})

	t.Run("wile unknown errors", func(t *testing.T) {
		evalExpectError(t, engine, `(environment '(wile unknown))`)
	})
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v -run TestEnvironment_WileProfiles ./extensions/eval/...`
Expected: FAIL -- `(wile tiny)` not recognized

**Step 3: Expose profile environment factory**

Add to `internal/bootstrap/bootstrap.go`:

```go
// NewProfileEnvironment creates a new environment for a named profile.
// The environment shares symbol interning with callerNS but gets its
// own bindings initialized with only the profile's extensions.
func NewProfileEnvironment(
	ctx context.Context,
	callerNS *environment.Namespace,
	exts []registry.Extension,
) (*environment.Namespace, error) {
	newNS := callerNS.NewChildNamespace()
	env := newNS.Runtime()

	_, err := initializeEnvironmentWithRegistry(ctx, env, exts)
	if err != nil {
		return nil, err
	}

	return newNS, nil
}
```

Note: `initializeEnvironmentWithRegistry` with an explicit `exts` slice
(non-nil) already skips the `allExtensions` default.

**Step 4: Implement profile detection in PrimEnvironment**

In `PrimEnvironment`, before the `ForEach` loop, add detection for
`(wile <name>)` specs. When the first (and only) import spec is a
`(wile ...)` form, delegate to the profile factory:

```go
// In PrimEnvironment, after creating argsVal but before the ForEach:

// Check for (wile <profile>) profile constructor
if profileNS, handled, err := tryWileProfile(mc, argsVal); handled {
	if err != nil {
		return err
	}
	mc.SetValue(profileNS)
	return nil
}
```

Helper function:

```go
// tryWileProfile checks if the import spec is a (wile <name>) profile
// constructor. Returns (namespace, true, nil) on match, (nil, false, nil)
// if not a profile spec, or (nil, true, err) on error.
func tryWileProfile(
	mc *machine.MachineContext,
	argsVal values.Value,
) (*environment.Namespace, bool, error) {
	// Profile spec comes as a single-element list containing
	// a list like (wile tiny)
	args, ok := argsVal.(values.Tuple)
	if !ok {
		return nil, false, nil
	}
	first := args.Car()
	rest := args.Cdr()
	if !values.IsEmptyList(rest) {
		return nil, false, nil // multiple specs, not a profile
	}

	// first should be a list like (wile tiny)
	spec, ok := first.(values.Tuple)
	if !ok {
		return nil, false, nil
	}
	head := spec.Car()
	// Check if head is the symbol 'wile'
	sym, ok := head.(*values.Symbol)
	if !ok || sym.Name() != "wile" {
		return nil, false, nil
	}

	// Get profile name
	namePart := spec.Cdr()
	nameList, ok := namePart.(values.Tuple)
	if !ok {
		return nil, false, nil
	}
	nameSym, ok := nameList.Car().(*values.Symbol)
	if !ok {
		return nil, false, nil
	}

	// Map name to profile extensions
	var exts []registry.Extension
	switch nameSym.Name() {
	case "tiny":
		exts = nil // core only
	case "console":
		exts = wile.Console.extensions() // need to resolve import path
	case "console-with-load":
		exts = wile.ConsoleWithLoad.extensions()
	case "small":
		exts = wile.Small.extensions()
	case "kitchen-sink":
		exts = wile.KitchenSink.extensions()
	default:
		return nil, true, werr.WrapForeignErrorf(
			werr.ErrInvalidArgument,
			"environment: unknown wile profile %q", nameSym.Name())
	}

	callerNS := mc.EnvironmentFrame().Namespace()
	ns, err := bootstrap.NewProfileEnvironment(mc.Context(), callerNS, exts)
	if err != nil {
		return nil, true, werr.WrapForeignErrorf(err,
			"environment: failed to create %s profile", nameSym.Name())
	}

	// Inherit env map from caller
	if callerNS.EnvMap() != nil {
		ns.SetEnvMap(callerNS.EnvMap())
	}

	return ns, true, nil
}
```

Note: The profile extension lists will need to be accessible from the eval
package (now public at `extensions/eval`). The root `wile` package imports
`extensions/eval`, so `eval` cannot import the root -- the import-cycle
constraint is unchanged by the public promotion. Options: (a) duplicate the
lists, (b) expose a function on a shared package, (c) put
profile-to-extensions mapping in `internal/bootstrap`. Option (c) is
cleanest -- the bootstrap package already knows about all extensions. Add a
`ProfileExtensions(name string) ([]registry.Extension, error)` function
there. `extensions/eval` is allowed to import `internal/bootstrap` because
both live under the `github.com/aalpar/wile` module root.

**Step 5: Run test**

Run: `go test -v -run TestEnvironment_WileProfiles ./extensions/eval/...`
Expected: PASS

**Step 6: Commit**

```
feat(scheme): support (environment '(wile tiny/console/small/kitchen-sink))
```

---

### Task 9: Integration Tests and Profile Smoke Tests

**Files:**
- Create: `engine_profile_test.go`

**Step 1: Write comprehensive profile smoke tests**

```go
// engine_profile_test.go
package wile

import (
	"context"
	"fmt"
	"os"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestProfile_Tiny_CoreOnly(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(Tiny))
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalString(ctx, "(+ 1 2)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, "3")

	_, err = eng.EvalString(ctx, "(display 42)")
	c.Assert(err, qt.IsNotNil)
}

func TestProfile_Console_IOWorks(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(Console))
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalString(ctx, "(+ 1 2)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, "3")

	result, err = eng.EvalString(ctx,
		`(let ((p (open-output-string))) (write "hello" p) (get-output-string p))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Contains, "hello")
}

func TestProfile_Console_FileSandbox(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(Console))
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalString(ctx, `(file-exists? "/tmp")`)
	c.Assert(err, qt.IsNil)

	_, err = eng.EvalString(ctx, `(file-exists? "/etc/passwd")`)
	c.Assert(err, qt.IsNotNil)
}

func TestProfile_ConsoleWithLoad_EvalAndLoad(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(ConsoleWithLoad))
	c.Assert(err, qt.IsNil)

	// (eval ...) works
	result, err := eng.EvalString(ctx,
		`(eval '(+ 1 2) (interaction-environment))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, "3")

	// (load ...) of a /tmp file works
	tmpFile, err := os.CreateTemp("/tmp", "wile-cwl-*.scm")
	c.Assert(err, qt.IsNil)
	defer os.Remove(tmpFile.Name())
	_, err = tmpFile.WriteString("(define cwl-loaded-value 42)\n")
	c.Assert(err, qt.IsNil)
	c.Assert(tmpFile.Close(), qt.IsNil)

	_, err = eng.EvalString(ctx, fmt.Sprintf(`(load %q)`, tmpFile.Name()))
	c.Assert(err, qt.IsNil)
	result, err = eng.EvalString(ctx, `cwl-loaded-value`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, "42")
}

func TestProfile_ConsoleWithLoad_DeniesLoadOutsideTmp(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(ConsoleWithLoad))
	c.Assert(err, qt.IsNil)

	// load outside /tmp is denied even though eval is enabled
	_, err = eng.EvalString(ctx, `(load "/etc/hosts")`)
	c.Assert(err, qt.IsNotNil)
}

func TestProfile_Small_R7RS(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(Small))
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalString(ctx,
		"(eval '(+ 1 2) (interaction-environment))")
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, "3")
}

func TestProfile_KitchenSink_Threads(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithProfile(KitchenSink))
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalString(ctx, "(thread? (current-thread))")
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, "#t")
}

func TestProfile_Superset_Invariant(t *testing.T) {
	c := qt.New(t)

	tinyExts := Tiny.extensions()
	consoleExts := Console.extensions()
	cwlExts := ConsoleWithLoad.extensions()
	smallExts := Small.extensions()
	kitchenExts := KitchenSink.extensions()

	c.Assert(len(tinyExts), qt.Equals, 0)
	c.Assert(len(consoleExts) > len(tinyExts), qt.IsTrue)
	c.Assert(len(cwlExts) > len(consoleExts), qt.IsTrue)
	c.Assert(len(smallExts) > len(cwlExts), qt.IsTrue)
	c.Assert(len(kitchenExts) > len(smallExts), qt.IsTrue)
}

func TestProfile_NoProfile_BareEngine(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalString(ctx, "(+ 1 2)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, "3")
}

func TestProfile_WithSandbox_Composition(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx,
		WithProfile(Small),
		WithSandbox(),
	)
	c.Assert(err, qt.IsNil)

	result, err := eng.EvalString(ctx, "(+ 1 2)")
	c.Assert(err, qt.IsNil)
	c.Assert(result.String(), qt.Equals, "3")
}
```

**Step 2: Run tests**

Run: `go test -v -run TestProfile ./...`
Expected: PASS

**Step 3: Run full suite + lint**

Run: `make lint && make test`
Expected: PASS

**Step 4: Commit**

```
test(profiles): add comprehensive profile integration tests
```

---

### Task 10: Update Documentation

**Files:**
- Modify: `doc.go` -- update package examples
- Modify: `CLAUDE.md` -- update architecture, extension list, security model
- Modify: `TODO.md` -- mark completed if applicable
- Modify: `internal/bootstrap/CLAUDE.local.md` -- already partially done in Task 7

**Step 1: Update doc.go examples**

Replace `WithExtension(io.Extension)` / `WithSafeExtensions()` examples with
profile-based examples:

```go
//	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.Small))
//
//	eng, err := wile.NewEngine(ctx,
//	    wile.WithProfile(wile.Console),
//	    wile.WithEnv("APP_MODE", "production"),
//	)
//
//	// Sandboxed eval/load (wile-goast pattern):
//	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.ConsoleWithLoad))
```

**Step 2: Update CLAUDE.md**

- Architecture section: mention profiles as the primary API
- Security Model section: add Console authorizer, ConsoleWithLoad authorizer,
  WithSandbox, virtual env map
- Update extension count

**Step 3: Update TODO.md if applicable**

**Step 4: Final validation**

Run: `make lint && make test`
Expected: PASS

**Step 5: Commit**

```
docs: update for environment profiles API
```

---

## Task Dependency Graph

```
Task 1 (Profile type)
  |
  +---> Task 2 (Console authorizer) ----------+
  |                                            |
  +---> Task 3 (Sandbox modifier) ------------+
  |                                            |
  +---> Task 4 (envvars split) --+             |
                                 |             |
                                 v             |
                           Task 5 (env map) ---+
                                               |
                                               v
                                         Task 6 (delete old API)
                                               |
                                               v
                                         Task 7 (rename files)
                                               |
                                               v
                                         Task 8 (Scheme API)
                                               |
                                               v
                                         Task 9 (integration tests)
                                               |
                                               v
                                         Task 10 (docs)

Tasks 2, 3, 4 can run in parallel after Task 1.
```

## Risk Notes

- **`all.Extension` includes system sub-extension.** After splitting envvars
  from system, verify that `all.Extension`'s builder still works. The `all`
  builder includes `system` as a sub-extension; if `system` no longer has env
  var primitives, `all` won't provide them either. This is correct -- envvars
  is a separate extension added by profiles.

- **`PrimEnvironment` profile constructor** needs access to bootstrap
  initialization. This crosses a package boundary (`extensions/eval`
  to `internal/bootstrap`). Both packages live under the `github.com/aalpar/wile`
  module root, so the import is permitted by Go's `internal` rule. Consider
  exposing a factory function via the `machine` package (which both depend on)
  to avoid a direct dependency if the bootstrap surface area grows.

- **eval extension is now public.** Embedders (e.g. wile-goast) can compose
  `eval.Extension` onto any profile via `WithExtension(eval.Extension)`. This
  is intentional -- the security boundary is opt-in capability, not package
  visibility. Profiles that omit `eval.Extension` (currently Tiny, Console)
  do not provide `(eval ...)` or `(load ...)`; profiles that include it
  (Small, KitchenSink) do. See "Embedder composition pattern" in the header.

- **Test count.** ~20 test files reference the old API. Budget time for Task 6.

- **Console authorizer path traversal.** The `/tmp/../etc` test case is
  critical. `filepath.Clean` must be applied before prefix checking.

- **Option ordering.** `WithProfile` must be applied before `WithSandbox`
  for `security.All()` composition to work. Document this or handle it
  in engine construction (apply profile authorizer first, then compose).
