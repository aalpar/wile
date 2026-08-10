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

package gorules_test

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// TestRuleguardRules runs golangci-lint against fixture files to verify that
// each custom ruleguard rule fires on positive cases and stays silent on
// negative cases.
//
// This is the only reliable way to test ruleguard rules: they are loaded by
// gocritic at lint time and cannot be exercised as normal Go functions.
func TestRuleguardRules(t *testing.T) {
	golangciLint, err := exec.LookPath("golangci-lint")
	if err != nil {
		t.Skip("golangci-lint not found; skipping ruleguard integration test")
	}

	// Resolve the absolute path to rules.go so the temp module can reference it.
	rulesPath, err := filepath.Abs("rules.go")
	if err != nil {
		t.Fatalf("resolve rules.go path: %v", err)
	}
	_, err = os.Stat(rulesPath)
	if err != nil {
		t.Fatalf("rules.go not found at %s: %v", rulesPath, err)
	}

	// Create a temporary Go module with fixture files.
	tmpDir := t.TempDir()

	writeFile := func(relPath, content string) {
		t.Helper()
		abs := filepath.Join(tmpDir, relPath)
		dir := filepath.Dir(abs)
		err := os.MkdirAll(dir, 0o755)
		if err != nil {
			t.Fatalf("mkdir %s: %v", dir, err)
		}
		err = os.WriteFile(abs, []byte(content), 0o644)
		if err != nil {
			t.Fatalf("write %s: %v", abs, err)
		}
	}

	// Module scaffolding. The dsl dependency is needed by golangci-lint to
	// parse the rules file (it resolves imports from the analyzed module).
	writeFile("go.mod", "module testmod\n\ngo 1.24\n\n"+
		"require github.com/quasilyte/go-ruleguard/dsl v0.3.23\n")

	// Minimal werr stub so production fixtures that reference werr compile.
	// This file intentionally uses errors.New (it's a stub) — it will
	// trigger noErrorsNew, which we ignore in expectations.
	writeFile("werr/werr.go", `package werr

import "errors"

var ErrNotAList = errors.New("not a list")       //nolint:gocritic
var ErrNotANumber = errors.New("not a number")   //nolint:gocritic
var SomeOtherVar = errors.New("not a sentinel")  //nolint:gocritic

func WrapForeignErrorf(err error, msg string, vs ...any) error {
	return err
}
`)

	// Minimal values stub. The package MUST be named "values": the
	// noUncheckedArgCast patterns are syntactic on the `*values.$t` selector,
	// so any other package name silently matches nothing.
	writeFile("values/values.go", `package values

// Integer is a concrete pointer target for the assertion fixtures.
type Integer struct {
	Value int64
}

// Get exists so the selector-chain form has something to chain onto.
func (p *Integer) Get() int64 {
	return p.Value
}
`)

	// golangci-lint configuration: only enable gocritic/ruleguard.
	//
	// The issues block is load-bearing. Both of golangci-lint's default output
	// caps hide diagnostics this table asserts on. max-same-issues is 3 and
	// counts by identical message text, so with it in place only three of the
	// six same-message noUncheckedArgCast positives are emitted (measured:
	// 41:2, 46:9 and 51:9 dropped, making live patterns read as dead).
	// uniq-by-line keeps one issue per line, which would hide a second rule
	// firing on a line another expectation already pins.
	writeFile(".golangci.yml", `version: "2"
run:
  timeout: 1m
linters:
  default: none
  enable:
    - gocritic
  settings:
    gocritic:
      enabled-checks:
        - ruleguard
      settings:
        ruleguard:
          rules: '`+rulesPath+`'
  exclusions:
    rules:
      - path: _test\.go
        linters:
          - errcheck
issues:
  uniq-by-line: false
  max-same-issues: 0
`)

	// ── Production fixture: positive cases (should trigger) ──────────
	//
	// Each function targets exactly one rule. Line numbers in expectations
	// are derived from counting lines in this string literal.

	writeFile("positive.go", `package main

import (
	"errors"
	"fmt"
	"os"
	"testmod/werr"
)

// noCompoundIf — positive
func compoundIfBad() {
	if f, err := os.Open("x"); err != nil {
		_ = f
	}
}

// noErrorsNew — positive
func errorsNewBad() error {
	return errors.New("bad")
}

// noFmtErrorf — positive
func fmtErrorfBad() error {
	return fmt.Errorf("bad: %s", "reason")
}

// noBareSentinelPanic — positive (ErrNotAList)
func barePanicBad1() {
	panic(werr.ErrNotAList)
}

// noBareSentinelPanic — positive (ErrNotANumber)
func barePanicBad2() {
	panic(werr.ErrNotANumber)
}
`)

	// ── Production fixture: negative cases (should NOT trigger) ──────

	writeFile("negative.go", `package main

import (
	"os"
	"testmod/werr"
)

// noCompoundIf — negative: separate init from condition
func separateIfGood() {
	f, err := os.Open("x")
	if err != nil {
		_ = f
	}
}

// noBareSentinelPanic — negative: wrapped panic
func wrappedPanicGood() {
	panic(werr.WrapForeignErrorf(werr.ErrNotAList, "site: context"))
}

// noBareSentinelPanic — negative: non-Err-prefixed name
func nonSentinelPanicGood() {
	panic(werr.SomeOtherVar)
}
`)

	// ── Production fixture: value-register accesses ──────────────────
	//
	// Targets noDirectValueRegisterAccess. The machineContext type and the
	// vm_state.go fixture (filename-exempted) both reach into the fields
	// directly; production accesses elsewhere should fire.

	writeFile("value_register.go", `package main

type machineContext struct {
	singleValue any
	multiValues []any
}

// noDirectValueRegisterAccess — positive (singleValue write)
func directWriteSingleBad(m *machineContext) {
	m.singleValue = "x"
}

// noDirectValueRegisterAccess — positive (multiValues write)
func directWriteMultiBad(m *machineContext) {
	m.multiValues = nil
}

// noDirectValueRegisterAccess — positive (singleValue read)
func directReadBad(m *machineContext) any {
	return m.singleValue
}
`)

	// ── Filename-exempted fixture: vm_state.go ───────────────────────
	//
	// noDirectValueRegisterAccess exempts vm_state.go by filename.

	writeFile("vm_state.go", `package main

// noDirectValueRegisterAccess — negative: vm_state.go is filename-exempted
func vmStateAllowedAccess(m *machineContext) {
	m.singleValue = "x"
	m.multiValues = nil
	_ = m.singleValue
}
`)

	// ── Test file fixture: errors.New, fmt.Errorf, bare panic allowed ─
	// noCompoundIf has no test-file exclusion, so it fires here too.

	writeFile("fixture_test.go", `package main

import (
	"errors"
	"fmt"
	"os"
	"testing"
	"testmod/werr"
)

// noErrorsNew — skipped in test files
func TestErrorsNewAllowed(t *testing.T) {
	_ = errors.New("ok in test")
}

// noFmtErrorf — skipped in test files
func TestFmtErrorfAllowed(t *testing.T) {
	_ = fmt.Errorf("ok in test: %s", "reason")
}

// noBareSentinelPanic — skipped in test files
func TestBarePanicAllowed(t *testing.T) {
	defer func() {
		_ = recover()
	}()
	panic(werr.ErrNotAList)
}

// noCompoundIf — fires even in test files (no exclusion)
func TestCompoundIfFlagged(t *testing.T) {
	if f, err := os.Open("x"); err != nil {
		_ = f
	}
}

// noDirectValueRegisterAccess — skipped in test files
func TestRegisterDirectAccessAllowed(t *testing.T) {
	m := &machineContext{}
	m.singleValue = "x"
	m.multiValues = nil
}
`)

	// ── Production fixture: Arg() type assertions ────────────────────
	//
	// Targets noUncheckedArgCast: one function per pattern, so a dead pattern
	// shows up as exactly one failing subtest, plus a second call-argument
	// case that puts the assertion in a non-first argument slot.

	writeFile("argcast.go", `package main

import "testmod/values"

type argCtx struct {
	args []any
}

func (p *argCtx) Arg(n int) any {
	return p.args[n]
}

func consume(v *values.Integer) int64 {
	return v.Get()
}

func consume2(i int, v *values.Integer) int64 {
	return v.Get() + int64(i)
}

// noUncheckedArgCast — positive (short assign)
func shortAssignBad(mc *argCtx) {
	n := mc.Arg(0).(*values.Integer)
	_ = n
}

// noUncheckedArgCast — positive (plain assign)
func plainAssignBad(mc *argCtx) {
	var n *values.Integer
	n = mc.Arg(0).(*values.Integer)
	_ = n
}

// noUncheckedArgCast — positive (selector chain)
func selectorChainBad(mc *argCtx) int64 {
	return mc.Arg(0).(*values.Integer).Get()
}

// noUncheckedArgCast — positive (return)
func returnBad(mc *argCtx) *values.Integer {
	return mc.Arg(0).(*values.Integer)
}

// noUncheckedArgCast — positive (call argument, sole)
func callArgSoleBad(mc *argCtx) int64 {
	return consume(mc.Arg(0).(*values.Integer))
}

// noUncheckedArgCast — positive (call argument, non-first)
func callArgNonFirstBad(mc *argCtx) int64 {
	return consume2(1, mc.Arg(0).(*values.Integer))
}

// noUncheckedArgCast — negative (comma-ok)
func commaOkGood(mc *argCtx) {
	n, ok := mc.Arg(0).(*values.Integer)
	if !ok {
		return
	}
	_ = n
}
`)

	// Populate go.sum via "go get" instead of "go mod tidy". The latter
	// would strip the dsl require (nothing in our Go sources imports it)
	// but golangci-lint needs it to parse the rules file.
	ctx, cancel := testContext(t, 2*time.Minute)
	defer cancel()

	goGet := exec.CommandContext(ctx, "go", "get", "github.com/quasilyte/go-ruleguard/dsl@v0.3.23")
	goGet.Dir = tmpDir
	out, err := goGet.CombinedOutput()
	if err != nil {
		t.Fatalf("go get ruleguard/dsl: %v\n%s", err, out)
	}

	// Run golangci-lint. It will exit non-zero if it finds issues (expected).
	cmd := exec.CommandContext(ctx, golangciLint, "run", "./...")
	cmd.Dir = tmpDir
	output, _ := cmd.CombinedOutput()
	lintOutput := string(output)

	// Log the full output for debugging when tests fail.
	t.Logf("golangci-lint output:\n%s", lintOutput)

	// ── Expectation table ────────────────────────────────────────────
	//
	// Each entry specifies a file, a diagnostic substring, and whether that
	// combination must appear (present=true) or must not appear (present=false).
	//
	// Line numbers: we match "file:line:col: ruleguard: <message>" to pin
	// exactly which statement triggered. Lines are counted from the fixture
	// string literals above.

	type expectation struct {
		name    string // subtest name
		substr  string // substring to search for in lint output
		present bool   // true = must appear, false = must NOT appear
	}

	expectations := []expectation{
		// ── noCompoundIf ─────────────────────────────────────────
		{
			name:    "noCompoundIf/positive_production",
			substr:  `positive.go:12:2: ruleguard: compound if-init statement`,
			present: true,
		},
		{
			name:    "noCompoundIf/positive_test_file",
			substr:  `fixture_test.go:31:2: ruleguard: compound if-init statement`,
			present: true,
		},
		{
			name:    "noCompoundIf/negative_separate_init",
			substr:  "negative.go",
			present: false,
		},

		// ── noErrorsNew ──────────────────────────────────────────
		{
			name:    "noErrorsNew/positive_production",
			substr:  `positive.go:19:9: ruleguard: use werr.WrapForeignErrorf(sentinel, msg) instead of errors.New`,
			present: true,
		},
		{
			name:    "noErrorsNew/negative_test_file",
			substr:  `fixture_test.go:13`,
			present: false,
		},

		// ── noFmtErrorf ──────────────────────────────────────────
		{
			name:    "noFmtErrorf/positive_production",
			substr:  `positive.go:24:9: ruleguard: use werr.WrapForeignErrorf(sentinel, msg, args...) instead of fmt.Errorf`,
			present: true,
		},
		{
			name:    "noFmtErrorf/negative_test_file",
			substr:  `fixture_test.go:18`,
			present: false,
		},

		// ── noBareSentinelPanic ──────────────────────────────────
		{
			name:    "noBareSentinelPanic/positive_ErrNotAList",
			substr:  `positive.go:29:2: ruleguard: panic with bare sentinel`,
			present: true,
		},
		{
			name:    "noBareSentinelPanic/positive_ErrNotANumber",
			substr:  `positive.go:34:2: ruleguard: panic with bare sentinel`,
			present: true,
		},
		{
			name:    "noBareSentinelPanic/negative_wrapped_panic",
			substr:  `negative.go:18`,
			present: false,
		},
		{
			name:    "noBareSentinelPanic/negative_non_sentinel_name",
			substr:  `negative.go:23`,
			present: false,
		},
		{
			name:    "noBareSentinelPanic/negative_test_file_panic",
			substr:  `fixture_test.go:26`,
			present: false,
		},

		// ── noDirectValueRegisterAccess ──────────────────────────
		{
			name:    "noDirectValueRegisterAccess/positive_singleValue_write",
			substr:  `value_register.go:10:2: ruleguard: direct value-register access`,
			present: true,
		},
		{
			name:    "noDirectValueRegisterAccess/positive_multiValues_write",
			substr:  `value_register.go:15:2: ruleguard: direct value-register access`,
			present: true,
		},
		{
			name:    "noDirectValueRegisterAccess/positive_singleValue_read",
			substr:  `value_register.go:20:9: ruleguard: direct value-register access`,
			present: true,
		},
		{
			// Negative substrings omit the :col: and :ruleguard: suffix —
			// golangci-lint emits "file:line:col: ruleguard: msg", so the
			// presence test must match a prefix (file:line:) that the linter
			// would emit IF the rule fired. Matches the shape used by
			// noErrorsNew/negative_test_file above.
			name:    "noDirectValueRegisterAccess/negative_vm_state_file_exempt",
			substr:  `vm_state.go:5:`,
			present: false,
		},
		{
			name:    "noDirectValueRegisterAccess/negative_test_file_exempt",
			substr:  `fixture_test.go:39:`,
			present: false,
		},

		// ── noUncheckedArgCast ───────────────────────────────────
		{
			name:    "noUncheckedArgCast/positive_short_assign",
			substr:  `argcast.go:23:2: ruleguard: unchecked type assertion on Arg()`,
			present: true,
		},
		{
			name:    "noUncheckedArgCast/positive_plain_assign",
			substr:  `argcast.go:30:2: ruleguard: unchecked type assertion on Arg()`,
			present: true,
		},
		{
			name:    "noUncheckedArgCast/positive_selector_chain",
			substr:  `argcast.go:36:9: ruleguard: unchecked type assertion on Arg()`,
			present: true,
		},
		{
			name:    "noUncheckedArgCast/positive_return",
			substr:  `argcast.go:41:2: ruleguard: unchecked type assertion on Arg()`,
			present: true,
		},
		{
			name:    "noUncheckedArgCast/positive_call_arg_sole",
			substr:  `argcast.go:46:9: ruleguard: unchecked type assertion on Arg()`,
			present: true,
		},
		{
			name:    "noUncheckedArgCast/positive_call_arg_nonfirst",
			substr:  `argcast.go:51:9: ruleguard: unchecked type assertion on Arg()`,
			present: true,
		},
		{
			name:    "noUncheckedArgCast/negative_comma_ok",
			substr:  `argcast.go:56:`,
			present: false,
		},
		{
			name:    "noUncheckedArgCast/negative_values_stub",
			substr:  `values/values.go`,
			present: false,
		},
	}

	for _, exp := range expectations {
		t.Run(exp.name, func(t *testing.T) {
			found := strings.Contains(lintOutput, exp.substr)
			if exp.present && !found {
				t.Errorf("expected lint output to contain:\n  %s", exp.substr)
			}
			if !exp.present && found {
				t.Errorf("expected lint output NOT to contain:\n  %s", exp.substr)
			}
		})
	}
}

// testContext returns a context bounded by either the test deadline (if set)
// or the given fallback duration, whichever is shorter.
func testContext(t *testing.T, fallback time.Duration) (context.Context, context.CancelFunc) {
	t.Helper()
	dl, ok := t.Deadline()
	if ok {
		return context.WithDeadline(context.Background(), dl)
	}
	return context.WithTimeout(context.Background(), fallback)
}
