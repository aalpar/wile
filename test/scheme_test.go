package test

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestSchemeTestSuite runs all Scheme-level unit tests.
// Tests are discovered by the run-all.sh script, which finds all
// *-test.scm and *_test.scm files in test/ and lib/ directories.
func TestSchemeTestSuite(t *testing.T) {
	c := qt.New(t)

	// Find the scheme binary
	// Go test runs with ./test as the working directory, so paths are relative to test/
	schemePath := "../dist/scheme"
	_, err := os.Stat(schemePath)
	if os.IsNotExist(err) {
		// Try platform-specific paths
		candidates := []string{
			"../dist/darwin/arm64/scheme",
			"../dist/darwin/amd64/scheme",
			"../dist/linux/arm64/scheme",
			"../dist/linux/amd64/scheme",
		}
		found := false
		for _, path := range candidates {
			_, statErr := os.Stat(path)
			if statErr == nil {
				schemePath = path
				found = true
				break
			}
		}
		if !found {
			t.Skip("Scheme binary not found; run 'make build' first")
		}
	}

	// Get absolute path to scheme binary
	absPath, err := filepath.Abs(schemePath)
	c.Assert(err, qt.IsNil)

	// Run the test suite
	// run-all.sh is in the same directory as this test file
	ctx := context.Background()
	cmd := exec.CommandContext(ctx, "./run-all.sh")
	cmd.Env = append(os.Environ(), "SCHEME="+absPath)

	output, err := cmd.CombinedOutput()
	t.Logf("Scheme test output:\n%s", output)

	if err != nil {
		t.Logf("Test command failed: %v", err)
	}
	c.Assert(err, qt.IsNil, qt.Commentf("Scheme test suite failed"))
}
