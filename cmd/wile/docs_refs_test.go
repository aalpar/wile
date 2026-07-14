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
	"context"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"testing"
)

// docPathRef matches a repo-path-looking reference in Markdown: an inline code
// span, or a link target.
var docPathRef = regexp.MustCompile("`([^`\n]+)`|\\]\\(([^)\\s]+)\\)")

// TestTrackedDocsDoNotReferenceIgnoredPaths guards the public docs against
// pointing at files a clone does not have. The working-notes directories
// (plans/, memory/) and the CLAUDE*.md files are gitignored, so a tracked doc
// that cites one sends the reader to a path that exists only on the author's
// machine. Either inline the content or drop the pointer.
func TestTrackedDocsDoNotReferenceIgnoredPaths(t *testing.T) {
	root := gitOutput(t, ".", "rev-parse", "--show-toplevel")

	refs := map[string][]string{} // referenced path -> citing docs
	for doc := range strings.SplitSeq(gitOutput(t, root, "ls-files", "--", "*.md"), "\n") {
		if doc == "" {
			continue
		}
		body, err := os.ReadFile(filepath.Join(root, doc))
		if err != nil {
			continue // deleted in the working tree; nothing to cite
		}
		for _, m := range docPathRef.FindAllStringSubmatch(string(body), -1) {
			candidate := m[1]
			if candidate == "" {
				candidate = m[2]
			}
			if !looksLikeRepoPath(candidate) {
				continue
			}
			repoPath := resolveRef(root, doc, candidate)
			if repoPath == "" {
				continue
			}
			refs[repoPath] = append(refs[repoPath], doc)
		}
	}

	if len(refs) == 0 {
		return
	}
	paths := make([]string, 0, len(refs))
	for p := range refs {
		paths = append(paths, p)
	}
	sort.Strings(paths)

	// git check-ignore echoes back the subset of its input paths that .gitignore
	// excludes, and exits 1 when none are — so the output, not the exit status,
	// carries the answer. The paths go in on stdin rather than argv: a candidate
	// scraped from prose can contain glob characters or a leading dash, and one
	// such argument makes git fail the whole invocation (which would make this
	// guard silently pass).
	cmd := exec.CommandContext(context.Background(), "git", "check-ignore", "--stdin")
	cmd.Dir = root
	cmd.Stdin = strings.NewReader(strings.Join(paths, "\n") + "\n")
	out, err := cmd.Output()
	var exitErr *exec.ExitError
	isExit := errors.As(err, &exitErr)
	if err != nil && (!isExit || exitErr.ExitCode() != 1) {
		t.Fatalf("git check-ignore: %v", err)
	}

	for ignored := range strings.SplitSeq(strings.TrimSpace(string(out)), "\n") {
		if ignored == "" {
			continue
		}
		for _, doc := range refs[ignored] {
			if uncleanedDocs[doc] {
				continue
			}
			t.Errorf("%s references %q, which is gitignored and absent from a clone: inline the content or drop the pointer",
				doc, ignored)
		}
	}
}

// uncleanedDocs are the tracked docs that still cite gitignored working notes.
// They predate this guard, which exists to stop the set from growing: a doc not
// listed here must keep its references resolvable in a fresh clone. Delete an
// entry once its references are inlined or dropped — never add one.
var uncleanedDocs = map[string]bool{
	".github/pull_request_template.md":       true,
	"CHANGELOG.md":                           true,
	"CODING_STYLE.md":                        true,
	"TODO.md":                                true,
	"docs/algebra/reference.md":              true,
	"docs/compiler/inlining.md":              true,
	"docs/compiler/macro-system.md":          true,
	"docs/dev/iteration-idioms.md":           true,
	"docs/reference/implementation-notes.md": true,
	"docs/security/sandboxing.md":            true,
	"scripts/PLAN_ISSUES_TEMPLATE.md":        true,
	"scripts/PLAN_TODO_TEMPLATE.md":          true,
	"scripts/README.md":                      true,
	"tools/sage/README.md":                   true,
}

// resolveRef turns a reference as written in doc into a repo-relative path.
// Markdown links resolve against the citing doc's directory; prose cites paths
// from the repo root ("see `memory/foo.md`") and the text does not say which.
// A reference that resolves doc-relative to a real file is that file; otherwise
// it is read as repo-root-relative, which is what a dangling `plans/…` citation
// is. Returns "" for a reference that leaves the repository.
func resolveRef(root, doc, candidate string) string {
	docRelative := filepath.Clean(filepath.Join(filepath.Dir(doc), candidate))
	_, err := os.Stat(filepath.Join(root, docRelative))
	if err == nil {
		candidate = docRelative
	}
	repoPath := filepath.Clean(candidate)
	if strings.HasPrefix(repoPath, "..") || filepath.IsAbs(repoPath) {
		return ""
	}
	return repoPath
}

// looksLikeRepoPath filters code spans and link targets down to those claiming
// to be files in this repository.
func looksLikeRepoPath(s string) bool {
	if s == "" || strings.Contains(s, "://") || strings.HasPrefix(s, "#") {
		return false
	}
	if strings.ContainsAny(s, " \t") {
		return false
	}
	if strings.HasPrefix(s, "plans/") || strings.HasPrefix(s, "memory/") {
		return true
	}
	return strings.HasSuffix(s, ".md")
}

// gitOutput runs git in dir and returns trimmed stdout, skipping the test when
// git is unavailable (e.g. a source tarball rather than a checkout).
func gitOutput(t *testing.T, dir string, args ...string) string {
	t.Helper()
	cmd := exec.CommandContext(context.Background(), "git", args...)
	cmd.Dir = dir
	out, err := cmd.Output()
	if err != nil {
		t.Skipf("git %v: %v (not a git checkout?)", args, err)
	}
	return strings.TrimSpace(string(out))
}
