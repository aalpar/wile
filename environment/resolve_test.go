package environment

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestResolveFile_AbsolutePath(t *testing.T) {
	c := qt.New(t)

	// Create a temporary file
	tmpFile, err := os.CreateTemp("", "resolve-test-*.scm")
	c.Assert(err, qt.IsNil)
	defer os.Remove(tmpFile.Name())
	tmpFile.Close()

	stack := NewLoadPathStack()

	// Absolute path should be returned as-is (after verification it exists)
	result, err := ResolveFile(stack, tmpFile.Name(), nil)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, tmpFile.Name())
}

func TestResolveFile_AbsolutePath_NotFound(t *testing.T) {
	c := qt.New(t)
	stack := NewLoadPathStack()

	nonexistent := "/nonexistent/file.scm"
	_, err := ResolveFile(stack, nonexistent, nil)
	c.Assert(err, qt.Not(qt.IsNil))
	c.Assert(err.Error(), qt.Contains, "not found")
	c.Assert(err.Error(), qt.Contains, nonexistent)
}

func TestResolveFile_StackRelative(t *testing.T) {
	c := qt.New(t)

	// Create temporary directory structure:
	// tmpDir/
	//   main.scm
	//   sub/
	//     helper.scm
	tmpDir, err := os.MkdirTemp("", "resolve-test-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir)

	mainFile := filepath.Join(tmpDir, "main.scm")
	c.Assert(os.WriteFile(mainFile, []byte(""), 0644), qt.IsNil)

	subDir := filepath.Join(tmpDir, "sub")
	c.Assert(os.Mkdir(subDir, 0755), qt.IsNil)

	helperFile := filepath.Join(subDir, "helper.scm")
	c.Assert(os.WriteFile(helperFile, []byte(""), 0644), qt.IsNil)

	stack := NewLoadPathStack()
	c.Assert(stack.Push(mainFile), qt.IsNil)

	// Resolve "sub/helper.scm" relative to main.scm's directory
	result, err := ResolveFile(stack, "sub/helper.scm", nil)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, helperFile)
}

func TestResolveFile_FallbackDirectories(t *testing.T) {
	c := qt.New(t)

	// Create two directories with files
	tmpDir1, err := os.MkdirTemp("", "resolve-test-1-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir1)

	tmpDir2, err := os.MkdirTemp("", "resolve-test-2-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir2)

	file1 := filepath.Join(tmpDir1, "lib.scm")
	c.Assert(os.WriteFile(file1, []byte(""), 0644), qt.IsNil)

	file2 := filepath.Join(tmpDir2, "util.scm")
	c.Assert(os.WriteFile(file2, []byte(""), 0644), qt.IsNil)

	stack := NewLoadPathStack()
	fallbacks := []string{tmpDir1, tmpDir2}

	// File in first fallback directory
	result, err := ResolveFile(stack, "lib.scm", fallbacks)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, file1)

	// File in second fallback directory
	result, err = ResolveFile(stack, "util.scm", fallbacks)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, file2)
}

func TestResolveFile_StackTakesPrecedenceOverFallback(t *testing.T) {
	c := qt.New(t)

	// Create two directories, both with "common.scm"
	stackDir, err := os.MkdirTemp("", "resolve-stack-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(stackDir)

	fallbackDir, err := os.MkdirTemp("", "resolve-fallback-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(fallbackDir)

	stackFile := filepath.Join(stackDir, "main.scm")
	c.Assert(os.WriteFile(stackFile, []byte(""), 0644), qt.IsNil)

	stackCommon := filepath.Join(stackDir, "common.scm")
	c.Assert(os.WriteFile(stackCommon, []byte("stack"), 0644), qt.IsNil)

	fallbackCommon := filepath.Join(fallbackDir, "common.scm")
	c.Assert(os.WriteFile(fallbackCommon, []byte("fallback"), 0644), qt.IsNil)

	stack := NewLoadPathStack()
	c.Assert(stack.Push(stackFile), qt.IsNil)

	// Should resolve to stack directory, not fallback
	result, err := ResolveFile(stack, "common.scm", []string{fallbackDir})
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, stackCommon)
}

func TestResolveFile_NotFound_ErrorMessage(t *testing.T) {
	c := qt.New(t)

	tmpDir1, err := os.MkdirTemp("", "resolve-test-1-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir1)

	tmpDir2, err := os.MkdirTemp("", "resolve-test-2-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir2)

	mainFile := filepath.Join(tmpDir1, "main.scm")
	c.Assert(os.WriteFile(mainFile, []byte(""), 0644), qt.IsNil)

	stack := NewLoadPathStack()
	c.Assert(stack.Push(mainFile), qt.IsNil)

	fallbacks := []string{tmpDir2}

	_, err = ResolveFile(stack, "nonexistent.scm", fallbacks)
	c.Assert(err, qt.Not(qt.IsNil))

	errMsg := err.Error()
	c.Assert(errMsg, qt.Contains, "nonexistent.scm")
	c.Assert(errMsg, qt.Contains, "not found")
	c.Assert(errMsg, qt.Contains, "searched:")
	// Should list both the stack directory and fallback directory
	c.Assert(errMsg, qt.Contains, tmpDir1)
	c.Assert(errMsg, qt.Contains, tmpDir2)
}

func TestResolveFile_EmptyStackAndFallbacks(t *testing.T) {
	c := qt.New(t)

	stack := NewLoadPathStack()

	_, err := ResolveFile(stack, "helper.scm", nil)
	c.Assert(err, qt.Not(qt.IsNil))
	c.Assert(err.Error(), qt.Contains, "not found")
	c.Assert(err.Error(), qt.Contains, "no search paths available")
}

func TestResolveFile_EmptyFallbacksSkipped(t *testing.T) {
	c := qt.New(t)

	tmpDir, err := os.MkdirTemp("", "resolve-test-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir)

	testFile := filepath.Join(tmpDir, "test.scm")
	c.Assert(os.WriteFile(testFile, []byte(""), 0644), qt.IsNil)

	stack := NewLoadPathStack()

	// Empty strings in fallbacks should be skipped
	fallbacks := []string{"", tmpDir, ""}

	result, err := ResolveFile(stack, "test.scm", fallbacks)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, testFile)
}

func TestResolveFile_ReturnsAbsolutePath(t *testing.T) {
	c := qt.New(t)

	tmpDir, err := os.MkdirTemp("", "resolve-test-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir)

	testFile := filepath.Join(tmpDir, "test.scm")
	c.Assert(os.WriteFile(testFile, []byte(""), 0644), qt.IsNil)

	stack := NewLoadPathStack()

	tcs := []struct {
		name      string
		setupFunc func() (string, []string)
	}{
		{
			name: "from stack relative",
			setupFunc: func() (string, []string) {
				mainFile := filepath.Join(tmpDir, "main.scm")
				os.WriteFile(mainFile, []byte(""), 0644) //nolint:errcheck
				_ = stack.Push(mainFile)
				return "test.scm", nil
			},
		},
		{
			name: "from fallback",
			setupFunc: func() (string, []string) {
				return "test.scm", []string{tmpDir}
			},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			path, fallbacks := tc.setupFunc()
			result, err := ResolveFile(stack, path, fallbacks)
			c.Assert(err, qt.IsNil)
			c.Assert(filepath.IsAbs(result), qt.IsTrue, qt.Commentf("result should be absolute: %s", result))

			// Clean up stack for next test
			for stack.Depth() > 0 {
				stack.Pop()
			}
		})
	}
}

func TestResolveFile_NilStack(t *testing.T) {
	c := qt.New(t)

	tmpDir, err := os.MkdirTemp("", "resolve-test-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir)

	testFile := filepath.Join(tmpDir, "test.scm")
	c.Assert(os.WriteFile(testFile, []byte(""), 0644), qt.IsNil)

	// nil stack should work - just skip stack-relative resolution
	result, err := ResolveFile(nil, "test.scm", []string{tmpDir})
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, testFile)
}

func TestResolveFile_ErrorMessageHint(t *testing.T) {
	c := qt.New(t)

	tmpDir1, err := os.MkdirTemp("", "resolve-hint-1-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir1)

	tmpDir2, err := os.MkdirTemp("", "resolve-hint-2-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir2)

	tcs := []struct {
		name      string
		setupFunc func() (*LoadPathStack, []string)
		wantHint  bool
	}{
		{
			name: "empty stack with fallback shows hint",
			setupFunc: func() (*LoadPathStack, []string) {
				return NewLoadPathStack(), []string{tmpDir1}
			},
			wantHint: true,
		},
		{
			name: "nil stack with fallback shows hint",
			setupFunc: func() (*LoadPathStack, []string) {
				return nil, []string{tmpDir1}
			},
			wantHint: true,
		},
		{
			name: "stack with current dir shows no hint",
			setupFunc: func() (*LoadPathStack, []string) {
				mainFile := filepath.Join(tmpDir1, "main.scm")
				os.WriteFile(mainFile, []byte(""), 0644) //nolint:errcheck

				s := NewLoadPathStack()
				_ = s.Push(mainFile)
				return s, []string{tmpDir2}
			},
			wantHint: false,
		},
		{
			name: "no search paths at all - different error",
			setupFunc: func() (*LoadPathStack, []string) {
				return NewLoadPathStack(), nil
			},
			wantHint: false, // "no search paths available" not "searched: ..."
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			stack, fallbacks := tc.setupFunc()
			_, err := ResolveFile(stack, "nonexistent.scm", fallbacks)
			c.Assert(err, qt.Not(qt.IsNil))

			if tc.wantHint {
				c.Assert(err.Error(), qt.Contains, "load from a file context")
			} else {
				hasHint := strings.Contains(err.Error(), "load from a file context")
				c.Assert(hasHint, qt.IsFalse)
			}
		})
	}
}
