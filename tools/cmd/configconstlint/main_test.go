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

package main

import (
	"os"
	"path/filepath"
	"testing"
)

// TestScanFile pins the detector against testdata/sample.go: the two call
// arguments (lines 7 and 8) are reported; the assignment on line 12 and the
// reader call on line 13 are not.
func TestScanFile(t *testing.T) {
	findings, err := scanFile(filepath.Join("testdata", "sample.go"))
	if err != nil {
		t.Fatalf("scanFile: %v", err)
	}
	wantLines := []int{7, 8}
	if len(findings) != len(wantLines) {
		t.Fatalf("got %d findings %v, want lines %v", len(findings), findings, wantLines)
	}
	for i, want := range wantLines {
		if findings[i].line != want {
			t.Errorf("finding %d: got line %d, want %d", i, findings[i].line, want)
		}
	}
}

// TestAllowedFile pins the load_bootstrap.go exemption by basename.
func TestAllowedFile(t *testing.T) {
	dir := t.TempDir()
	src, err := os.ReadFile(filepath.Join("testdata", "sample.go"))
	if err != nil {
		t.Fatal(err)
	}
	allowed := filepath.Join(dir, allowedFile)
	err = os.WriteFile(allowed, src, 0o600)
	if err != nil {
		t.Fatal(err)
	}
	findings, err := scanFile(allowed)
	if err != nil {
		t.Fatalf("scanFile: %v", err)
	}
	if len(findings) != 0 {
		t.Errorf("allowed file reported %v", findings)
	}
}

// TestScanDirSkipsTestFiles pins that _test.go files and testdata directories are not scanned.
func TestScanDirSkipsTestFiles(t *testing.T) {
	dir := t.TempDir()
	src, err := os.ReadFile(filepath.Join("testdata", "sample.go"))
	if err != nil {
		t.Fatal(err)
	}
	err = os.WriteFile(filepath.Join(dir, "x_test.go"), src, 0o600)
	if err != nil {
		t.Fatal(err)
	}
	err = os.Mkdir(filepath.Join(dir, "testdata"), 0o700)
	if err != nil {
		t.Fatal(err)
	}
	err = os.WriteFile(filepath.Join(dir, "testdata", "sample.go"), src, 0o600)
	if err != nil {
		t.Fatal(err)
	}
	findings, err := scanDir(dir)
	if err != nil {
		t.Fatalf("scanDir: %v", err)
	}
	if len(findings) != 0 {
		t.Errorf("test file reported %v", findings)
	}
}
