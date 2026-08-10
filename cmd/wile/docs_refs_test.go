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
	"maps"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"slices"
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
	paths := slices.Sorted(maps.Keys(refs))

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

// TestCorrectedDocClaimsStayCorrected is a per-document forbidden-phrase
// ratchet: each row names a claim a tracked document used to make and that was
// measured false, so the document must never carry that exact wording again.
//
// What it does NOT cover, stated because a suite named for a property owes the
// reader its boundary. This is not a path-existence checker and not a truth
// checker: it sees the stale spelling, never the corrected one, so an edit that
// deletes the phrase and writes a second falsehood passes. A tree-wide "every
// cited repo path resolves" check would cover more and is not available as this
// gate — tracked .md carries well over a hundred backticked path spans that do
// not resolve today (historical CHANGELOG entries, docs written before the
// pkg/ move), so it would need an allowlist larger than the fix. Re-measure that
// with docPathRef and looksLikeRepoPath over `git ls-files -- '*.md'` rather
// than trusting the figure; it was taken 2026-08-10.
func TestCorrectedDocClaimsStayCorrected(t *testing.T) {
	root := gitOutput(t, ".", "rev-parse", "--show-toplevel")

	for _, c := range correctedDocClaims {
		t.Run(c.name, func(t *testing.T) {
			body, err := os.ReadFile(filepath.Join(root, c.doc))
			if err != nil {
				t.Fatalf("%s: %v", c.doc, err)
			}
			if !strings.Contains(string(body), c.stale) {
				return
			}
			t.Errorf("%s carries %q again.\n%s", c.doc, c.stale, c.why)
		})
	}
}

// correctedDocClaim is one wording removed from a tracked document because it
// was measured false, together with the measurement that condemned it.
//
// stale carries its own backticks wherever the claim is a code span. Without
// them a corrected spelling that extends the stale one — `pkg/stdlib/lib/chibi/
// test.sld` over `stdlib/lib/chibi/test.sld` — would keep its row red forever
// and the row would say nothing.
type correctedDocClaim struct {
	name  string
	doc   string
	stale string
	why   string
}

var correctedDocClaims = []correctedDocClaim{
	{
		name:  "CONTRIBUTING_parser_is_public",
		doc:   "CONTRIBUTING.md",
		stale: "`internal/parser`",
		why:   "The package is pkg/parser and has been public since it was promoted out of pkg/internal.",
	},
	{
		name:  "CONTRIBUTING_syntax_is_public",
		doc:   "CONTRIBUTING.md",
		stale: "`internal/syntax`",
		why:   "The package is pkg/syntax and has been public since it was promoted out of pkg/internal.",
	},
	{
		name:  "CONTRIBUTING_stdlib_test_dir",
		doc:   "CONTRIBUTING.md",
		stale: "`stdlib/lib/srfi/1/test/fold-test.scm`",
		why: "pkg/stdlib/lib holds no test/ directory at any depth. Its two suites, " +
			"wile/er-macro-test.scm and wile/algebra/sat-test.scm, sit beside the library they test.",
	},
	{
		name:  "CONTRIBUTING_regression_name_never_runs",
		doc:   "CONTRIBUTING.md",
		stale: "`test/regression/issue-123-macro-hygiene.scm`",
		why: "test/run-all.sh discovers by `find test pkg/stdlib/lib -name '*-test.scm'`, so a " +
			"name ending .scm but not -test.scm is never run wherever it is put.",
	},
	{
		name:  "testREADME_stdlib_test_dir",
		doc:   "test/README.md",
		stale: "`stdlib/lib/srfi/1/test/fold-test.scm`",
		why:   "Verbatim twin of the CONTRIBUTING.md row above, and false for the same reason.",
	},
	{
		name:  "testREADME_regression_name_never_runs",
		doc:   "test/README.md",
		stale: "`test/regression/issue-123-macro-hygiene.scm`",
		why:   "Verbatim twin of the CONTRIBUTING.md row above, and false for the same reason.",
	},
	{
		name:  "COMPATIBILITY_chibi_test_path",
		doc:   "test/COMPATIBILITY.md",
		stale: "`stdlib/lib/chibi/test.sld`",
		why:   "The stdlib moved under pkg/ in 5536e995; the file is at pkg/stdlib/lib/chibi/test.sld.",
	},
	{
		name:  "sage_algebra_design_suffix",
		doc:   "tools/sage/README.md",
		stale: "`memory/2026-04-12-sage-algebra-validation-design.md`",
		why:   "The archived design carries the .local.md suffix every file in memory/ carries.",
	},
	{
		name:  "sage_oracle_coverage_location",
		doc:   "tools/sage/README.md",
		stale: "`plans/2026-06-07-sage-oracle-coverage-design.md`",
		why:   "That design was archived: it is memory/2026-06-07-sage-oracle-coverage-design.local.md.",
	},
	{
		name:  "r7rs_RegisterFunc_spelling",
		doc:   "docs/reference/r7rs-differences.md",
		stale: "`wile.RegisterFunc`",
		why: "RegisterFunc is a method on Engine (pkg/wile/ffi.go), not a package-level " +
			"function; wile.RegisterFunc does not compile. Engine.RegisterFunc is the spelling " +
			"docs/embedding/api-design.md already uses.",
	},
	{
		name:  "CHANGELOG_versioned_names_unimplemented",
		doc:   "CHANGELOG.md",
		stale: "since versioned library names are unimplemented",
		why: "Version references are implemented: they are parsed and dropped, so " +
			"`(import (rnrs hashtables (6)))` resolves and (hashtable? (make-eq-hashtable)) " +
			"answers #t. r7rs-differences.md item 15 documents the drop.",
	},
	{
		name:  "TODO_versioned_name_rejected",
		doc:   "TODO.md",
		stale: "import-set resolver rejects a list as a name part",
		why: "The resolver accepts a list in the final position and drops it as an R6RS version " +
			"reference; only a list elsewhere in the name is an error.",
	},
	{
		name:  "TODO_four_blind_guards",
		doc:   "TODO.md",
		stale: "and 4 more are guarded by a test",
		why: "Three, not four: TestRunEval is not blind. runCLI re-execs the test binary through " +
			"TestMain, which calls main(), so go-flags parsing is exercised end to end.",
	},
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
