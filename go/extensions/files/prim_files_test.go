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
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	extfiles "github.com/aalpar/wile/go/extensions/files"
	extio "github.com/aalpar/wile/go/extensions/io"
	"github.com/aalpar/wile/go/values"
	"github.com/aalpar/wile/go/wile"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with core + io + files extensions.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(
		wile.WithExtension(extio.Extension),
		wile.WithExtension(extfiles.Extension),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// eval runs Scheme code and returns the result.
func eval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.Eval(context.Background(), code)
	qt.Assert(t, err, qt.IsNil)
	return result
}

// evalExpectError runs Scheme code and expects an error.
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	_, err := engine.Eval(context.Background(), code)
	qt.Assert(t, err, qt.IsNotNil)
}

// writeTestFile creates a file with the given contents in the temp directory.
func writeTestFile(t *testing.T, dir, name, contents string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	err := os.WriteFile(path, []byte(contents), 0o644)
	qt.Assert(t, err, qt.IsNil)
	return path
}

func TestOpenInputFile(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	path := writeTestFile(t, dir, "hello.txt", "hello world")

	// Happy path: opens file and returns an input port
	result := eval(t, engine, fmt.Sprintf(`(input-port? (open-input-file %q))`, path))
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// Nonexistent file: error
	evalExpectError(t, engine, fmt.Sprintf(`(open-input-file %q)`, filepath.Join(dir, "nonexistent.txt")))

	// Wrong type: error
	evalExpectError(t, engine, `(open-input-file 42)`)
}

func TestOpenOutputFile(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	outPath := filepath.Join(dir, "out.txt")

	// Happy path: creates file and returns an output port
	result := eval(t, engine, fmt.Sprintf(`(output-port? (open-output-file %q))`, outPath))
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// File should exist after opening
	_, err := os.Stat(outPath)
	c.Assert(err, qt.IsNil)

	// Wrong type: error
	evalExpectError(t, engine, `(open-output-file 42)`)
}

func TestOpenBinaryInputFile(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	path := writeTestFile(t, dir, "data.bin", "\x00\x01\x02")

	// Happy path: opens file and returns an input port
	result := eval(t, engine, fmt.Sprintf(`(input-port? (open-binary-input-file %q))`, path))
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// Nonexistent file: error
	evalExpectError(t, engine, fmt.Sprintf(`(open-binary-input-file %q)`, filepath.Join(dir, "nope.bin")))
}

func TestOpenBinaryOutputFile(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	outPath := filepath.Join(dir, "out.bin")

	// Happy path: creates file and returns an output port
	result := eval(t, engine, fmt.Sprintf(`(output-port? (open-binary-output-file %q))`, outPath))
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// File should exist
	_, err := os.Stat(outPath)
	c.Assert(err, qt.IsNil)
}

func TestFileExistsQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	path := writeTestFile(t, dir, "exists.txt", "content")

	// Existing file returns #t
	result := eval(t, engine, fmt.Sprintf(`(file-exists? %q)`, path))
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// Missing file returns #f
	result = eval(t, engine, fmt.Sprintf(`(file-exists? %q)`, filepath.Join(dir, "missing.txt")))
	c.Assert(result.Internal(), qt.Equals, values.FalseValue)

	// Wrong type: error
	evalExpectError(t, engine, `(file-exists? 42)`)
}

func TestDeleteFile(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	path := writeTestFile(t, dir, "delete-me.txt", "content")

	// Deletes existing file
	eval(t, engine, fmt.Sprintf(`(delete-file %q)`, path))
	_, err := os.Stat(path)
	c.Assert(os.IsNotExist(err), qt.IsTrue)

	// Error on nonexistent file
	evalExpectError(t, engine, fmt.Sprintf(`(delete-file %q)`, filepath.Join(dir, "nope.txt")))

	// Wrong type: error
	evalExpectError(t, engine, `(delete-file 42)`)
}

func TestCallWithInputFile(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	path := writeTestFile(t, dir, "read-me.txt", "hello")

	// Reads file contents via proc
	result := eval(t, engine, fmt.Sprintf(
		`(call-with-input-file %q (lambda (port) (read-char port)))`, path))
	c.Assert(result.Internal().(*values.Character).Value, qt.Equals, 'h')

	// Port is passed as argument
	result = eval(t, engine, fmt.Sprintf(
		`(call-with-input-file %q (lambda (port) (input-port? port)))`, path))
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestCallWithOutputFile(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	outPath := filepath.Join(dir, "write-me.txt")

	// Writes via proc
	eval(t, engine, fmt.Sprintf(
		`(call-with-output-file %q (lambda (port) (write-char #\A port)))`, outPath))

	data, err := os.ReadFile(outPath)
	c.Assert(err, qt.IsNil)
	c.Assert(string(data), qt.Equals, "A")
}

func TestWithInputFromFile(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	path := writeTestFile(t, dir, "input.txt", "X")

	// Redirects current-input-port during thunk
	result := eval(t, engine, fmt.Sprintf(
		`(with-input-from-file %q (lambda () (read-char)))`, path))
	c.Assert(result.Internal().(*values.Character).Value, qt.Equals, 'X')
}

func TestWithOutputToFile(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	outPath := filepath.Join(dir, "output.txt")

	// Redirects current-output-port during thunk
	eval(t, engine, fmt.Sprintf(
		`(with-output-to-file %q (lambda () (write-char #\Z)))`, outPath))

	data, err := os.ReadFile(outPath)
	c.Assert(err, qt.IsNil)
	c.Assert(string(data), qt.Equals, "Z")
}
