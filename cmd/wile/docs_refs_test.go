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
	"bytes"
	"context"
	"errors"
	"maps"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"testing"
)

// docPathRef matches a repo-path-looking reference in Markdown: an inline code
// span, or a link target.
var docPathRef = regexp.MustCompile("`([^`\n]+)`|\\]\\(([^)\\s]+)\\)")

// TestTrackedDocsDoNotReferenceIgnoredPaths guards the public docs against
// pointing at files a clone does not have. plans/ and memory/ are tracked as
// symlinks into a sibling checkout of the working notes, so nothing beneath
// them is in a clone; the CLAUDE*.md files are gitignored. A tracked doc that
// cites one sends the reader to a path that exists only on the author's
// machine. Either inline the content or drop the pointer.
func TestTrackedDocsDoNotReferenceIgnoredPaths(t *testing.T) {
	root := gitOutput(t, ".", "rev-parse", "--show-toplevel")

	refs := map[string][]string{} // referenced path -> citing docs
	for _, d := range trackedDocs(t, root) {
		doc := d.name
		body := d.body
		for _, m := range docPathRef.FindAllStringSubmatch(body, -1) {
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

	for _, absent := range absentFromClone(t, root, paths) {
		for _, doc := range refs[absent] {
			if uncleanedDocs[doc] {
				continue
			}
			t.Errorf("%s references %q, which is absent from a clone: inline the content or drop the pointer",
				doc, absent)
		}
	}
}

// TestAbsentFromCloneCatchesWorkingNotes pins the classifier the guard above
// rests on. A classifier that answers "nothing is absent" passes that guard
// vacuously, so its positives are asserted here directly.
func TestAbsentFromCloneCatchesWorkingNotes(t *testing.T) {
	root := gitOutput(t, ".", "rev-parse", "--show-toplevel")
	paths := []string{
		"README.md",
		"docs/INDEX.md",
		"docs/notes.local.md",
		"memory",
		"memory/2026-01-01-x.local.md",
		"plans/foo.local.md",
		"plans/sub/bar.md",
	}
	want := []string{
		"docs/notes.local.md",
		"memory",
		"memory/2026-01-01-x.local.md",
		"plans/foo.local.md",
		"plans/sub/bar.md",
	}
	got := absentFromClone(t, root, paths)
	slices.Sort(got)
	if !slices.Equal(got, want) {
		t.Errorf("absentFromClone(%q)\n got %q\nwant %q", paths, got, want)
	}
}

// absentFromClone returns the subset of paths a fresh clone does not have:
// those .gitignore excludes, and those at or beyond a tracked symlink whose
// target leaves the repository. memory and plans are such symlinks, into a
// checkout of the working notes that sits beside the repository.
//
// The symlink case cannot go through git check-ignore, which refuses any path
// beyond a symlink ("beyond a symbolic link", exit 128) and so fails the whole
// batch. It needs no ignore rule either: git never tracks a path beyond a
// symlink, so a clone has one only if the link's target supplies it, and a
// target outside the repository is not part of the clone. The target is read
// from the index rather than the filesystem, so a dangling link (CI, a
// worktree) classifies the same as a live one.
func absentFromClone(t *testing.T, root string, paths []string) []string {
	t.Helper()
	var q []string
	escaping := escapingSymlinks(t, root)
	var rest []string
	for _, p := range paths {
		if atOrBeyondAny(p, escaping) {
			q = append(q, p)
			continue
		}
		rest = append(rest, p)
	}
	if len(rest) == 0 {
		return q
	}

	// git check-ignore echoes back the subset of its input paths that .gitignore
	// excludes, and exits 1 when none are — so the output, not the exit status,
	// carries the answer. The paths go in on stdin rather than argv: a candidate
	// scraped from prose can contain glob characters or a leading dash, and one
	// such argument makes git fail the whole invocation (which would make this
	// guard silently pass).
	cmd := exec.CommandContext(context.Background(), "git", "check-ignore", "--stdin")
	cmd.Dir = root
	cmd.Stdin = strings.NewReader(strings.Join(rest, "\n") + "\n")
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
		q = append(q, ignored)
	}
	return q
}

// escapingSymlinks returns the tracked symlinks whose target, resolved against
// the link's directory, leaves the repository.
func escapingSymlinks(t *testing.T, root string) []string {
	t.Helper()
	var q []string
	for entry := range strings.SplitSeq(gitOutput(t, root, "ls-files", "-s"), "\n") {
		// <mode> <object> <stage>\t<path>; 120000 is git's symlink mode.
		meta, link, found := strings.Cut(entry, "\t")
		if !found || !strings.HasPrefix(meta, "120000 ") {
			continue
		}
		object := strings.Fields(meta)[1]
		target := gitOutput(t, root, "cat-file", "blob", object)
		if filepath.IsLocal(filepath.Join(filepath.Dir(link), target)) {
			continue
		}
		q = append(q, link)
	}
	return q
}

// atOrBeyondAny reports whether path names one of links or a path inside it.
func atOrBeyondAny(path string, links []string) bool {
	for _, link := range links {
		if path == link || strings.HasPrefix(path, link+"/") {
			return true
		}
	}
	return false
}

// uncleanedDocs are the tracked docs that still cite gitignored working notes.
// They predate this guard, which exists to stop the set from growing: a doc not
// listed here must keep its references resolvable in a fresh clone. Delete an
// entry once its references are inlined or dropped — never add one. The
// todo/*.md entries are not additions: they are the files TODO.md was split
// into on 2026-10-17, carrying its rows and its citations verbatim.
var uncleanedDocs = map[string]bool{
	".github/pull_request_template.md":       true,
	"CHANGELOG.md":                           true,
	"CODING_STYLE.md":                        true,
	"TODO.md":                                true,
	"todo/closed.md":                         true,
	"todo/exceptions-and-rejected.md":        true,
	"todo/tier1-security-correctness.md":     true,
	"todo/tier2-embedding-api.md":            true,
	"todo/tier3-tooling-dx.md":               true,
	"todo/tier4-performance.md":              true,
	"todo/tier5-tech-debt.md":                true,
	"todo/tier6-nice-to-haves.md":            true,
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
// pkg/ move), so it would need an allowlist larger than the fix. That figure was
// measured 2026-08-10; re-measure with docPathRef and looksLikeRepoPath over
// `git ls-files -- '*.md'` rather than trusting it.
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
// them, a correction that only PREFIXES the stale spelling (the chibi test.sld
// row, whose fix adds a leading pkg/) leaves the stale text still present as a
// substring, so the row would stay red forever and say nothing.
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
		name:  "CONTRIBUTING_PRIMITIVES_is_complete",
		doc:   "CONTRIBUTING.md",
		stale: "Complete primitives reference",
		why: "PRIMITIVES.md is not complete and now says so, naming the gap in its own lead. " +
			"That figure is derived by TestProseCountsMatchTheirSource/PRIMITIVES_undocumented_names; " +
			"do not copy it here, or this row rots the next time a primitive is registered. Two " +
			"tracked documents disagreeing about the same document is the drift this pass exists to close.",
	},
	{
		name:  "INDEX_PRIMITIVES_is_complete",
		doc:   "docs/INDEX.md",
		stale: "Complete reference of types and primitives",
		why: "The third tracked document making the claim CONTRIBUTING_PRIMITIVES_is_complete " +
			"condemns. Its wording differs from CONTRIBUTING's, which is why one row could not " +
			"cover both — a forbidden-phrase gate sees spellings, not claims.",
	},
	{
		name:  "CODING_STYLE_syntax_is_public",
		doc:   "CODING_STYLE.md",
		stale: `"github.com/aalpar/wile/pkg/internal/syntax"`,
		why: "Third instance of the CONTRIBUTING_syntax_is_public defect, and the worst-placed: " +
			"it sits inside the canonical import-grouping example, so it is copied rather than read. " +
			"pkg/internal/ holds bootstrap, extensions, forms, match, tokenizer and validate — no syntax.",
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
		why:   "Verbatim twin of CONTRIBUTING_stdlib_test_dir, and false for the same reason.",
	},
	{
		name:  "testREADME_regression_name_never_runs",
		doc:   "test/README.md",
		stale: "`test/regression/issue-123-macro-hygiene.scm`",
		why:   "Verbatim twin of CONTRIBUTING_regression_name_never_runs, and false for the same reason.",
	},
	{
		name:  "testREADME_regression_pattern_never_runs",
		doc:   "test/README.md",
		stale: "`test/regression/issue-<num>-<slug>.scm`",
		why: "The Adding New Tests instruction, as opposed to the naming table's example. It told " +
			"contributors to create precisely the file test/run-all.sh's `-name '*-test.scm'` can " +
			"never discover, so the regression would be silently unguarded. Pinning the example " +
			"string alone left this green.",
	},
	{
		name:  "testREADME_library_test_subdir",
		doc:   "test/README.md",
		stale: "`lib/<library>/test/<module>-test.scm`",
		why: "Two errors in one path: pkg/stdlib/lib has no test/ subdirectory at any depth (see " +
			"testREADME_stdlib_test_dir), and the root is pkg/stdlib/lib, not lib.",
	},
	{
		name:  "testREADME_discovery_root",
		doc:   "test/README.md",
		stale: "`lib/` directory (recursive)",
		why:   "test/run-all.sh searches pkg/stdlib/lib; the repository has no top-level lib/.",
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
		stale: "`memory/2026-04-12-sage-algebra-validation-design.local.md`",
		why: "memory/ files dropped their .local.md suffix on 2026-09-14 (wile-memoplans c3776a3); " +
			"the archived design is memory/2026-04-12-sage-algebra-validation-design.md.",
	},
	{
		name:  "sage_oracle_coverage_location",
		doc:   "tools/sage/README.md",
		stale: "`plans/2026-06-07-sage-oracle-coverage-design.md`",
		why:   "That design was archived: it is memory/2026-06-07-sage-oracle-coverage-design.md.",
	},
	{
		name:  "r7rs_RegisterFunc_spelling",
		doc:   "docs/reference/r7rs-differences.md",
		stale: "`wile.RegisterFunc`",
		why: "RegisterFunc is a method on Engine (pkg/wile/ffi.go), not a package-level " +
			"function, so wile.RegisterFunc does not compile. It was the tree's only such " +
			"spelling; TODO.md writes the qualified form as Engine.RegisterFunc.",
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
		doc:   "todo/tier6-nice-to-haves.md",
		stale: "import-set resolver rejects a list as a name part",
		why: "The resolver accepts a list in the final position and drops it as an R6RS version " +
			"reference; only a list elsewhere in the name is an error.",
	},
	{
		name:  "TODO_four_blind_guards",
		doc:   "todo/tier5-tech-debt.md",
		stale: "and 4 more are guarded by a test",
		why: "Three, not four: TestRunEval is not blind. runCLI re-execs the test binary through " +
			"TestMain, which calls main(), so go-flags parsing is exercised end to end.",
	},
	{
		name:  "TODO_sameImportedBinding",
		doc:   "todo/tier1-security-correctness.md",
		stale: "`sameImportedBinding`",
		why: "Deleted by 5d654b3f (2026-09-16). The live symbol is importConflicts " +
			"(pkg/machine/compilation/library_bindings.go), which compares provenance roots.",
	},
	{
		name:  "TODO_origin_rejected_for_import_conflict",
		doc:   "todo/tier1-security-correctness.md",
		stale: "REJECTED for import-conflict detection",
		why: "Provenance-root origin is what SHIPPED for import-conflict detection (5d654b3f); " +
			"importConflicts compares *OriginRef and nothing else. What PR #793 rejected was a " +
			"SOURCE-LOCATION origin, a different signal.",
	},
	{
		name:  "TODO_compileTransformerToMachineClosure",
		doc:   "todo/tier1-security-correctness.md",
		stale: "`compileTransformerToMachineClosure`",
		why: "Absent from every .go file. The live symbol is compileTransformerValue " +
			"(pkg/machine/compilation/compile_transformer.go:60), which expands and evaluates the " +
			"right-hand side instead of switching on its head symbol — measured 2026-09-26: the " +
			"macro-produced transformer, let-syntax and letrec-syntax shapes print 1, 42 and 43.",
	},
	{
		name:  "TODO_TestMachineClosureIsTwoWords",
		doc:   "todo/tier4-performance.md",
		stale: "`TestMachineClosureIsTwoWords`",
		why: "Absent from every .go file. The live pin is TestMachineClosureSize " +
			"(pkg/machine/machine_closure_test.go:44), and it asserts FIVE words, not two: " +
			"flat-closure conversion re-opened the 16 B trade deliberately, so MachineClosure is " +
			"{link, template, free} = 40 B.",
	},
	{
		name:  "TODO_applyClosure",
		doc:   "todo/tier4-performance.md",
		stale: "`applyClosure`",
		why: "Absent from every .go file. The apply path is (*MachineContext).Apply " +
			"(pkg/machine/machine_context_apply.go:28).",
	},
	{
		name:  "TODO_TestLibraryExportTakesFirstPresentPhase",
		doc:   "todo/tier1-security-correctness.md",
		stale: "`TestLibraryExportTakesFirstPresentPhase`",
		why: "Renamed by d691ded5 (2026-09-17), which retired the export-phase fork the old name " +
			"asserted. The live test is TestLibraryExportRoundTripByPhase " +
			"(pkg/wile/library_export_phase_order_test.go:115).",
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

// The two citation checks below, TestCitedSymbolsExistWhereCited and
// TestLineCitationCensusOnlyShrinks, walk `git ls-files -- '*.md'`. That set
// holds no file under plans/ or memory/, so every plan file's citations are
// outside these gates by construction, not by omission. The mechanism is the
// symlink, not the ignore rule: plans/ and memory/ are tracked as mode-120000
// symlinks, nothing beneath a symlink is in the index, and the /plans/ and
// /memory/ patterns in .gitignore are inert for a tracked path (the file's own
// comment is the authority). Every count quoted in these tests is a count over
// the tracked Markdown, not the tree.
//
// Consequence: re-measuring the Go anchors inside the plan files after each
// phase merge (the closeout audit's fault E1, remedy c) cannot be hosted here.
// A pass over plans/ off the filesystem would not run in CI, which has no such
// directory, and ratcheting 500 line numbers repeats the mistake E1 argues
// against. That measurement stays a maintainer action with a written
// specification and no gate.
//
// There is deliberately no path-existence arm. It has no red set that
// uncleanedDocs does not already exempt; its basename-suffix fallback, which
// would forgive a stale directory, is in direct tension with the directory
// drift the symbol arm exists to catch; and the whole-tree scope is infeasible
// (most path-like spans in tracked Markdown do not resolve).
//
// Unresolvable citations. A citation whose file is not tracked, or whose
// trailing path components name more than one tracked file, cannot be checked,
// and is skipped by both checks. Skipping alone would let a rename hide: the
// symbol check would see one citation fewer and the census would see one span
// fewer, and a maintainer told to "lower the ceiling" would lock in the dead
// citation. So both pins count what the grammar MATCHES, resolved or not, and
// a rename leaves them unchanged. The two pins differ on purpose in what they
// do when the count moves: a citation that stops resolving moves
// symbolCitationMatches (resolved ones) and fails it, which is the prompt to
// look; a citation deleted outright lowers both and is a legitimate edit that
// the message tells the maintainer to record. The counts cannot tell a
// deletion from a rename on their own; the resolved-symbol pin plus the
// per-citation findings are what do.

// symbolCitation matches a code-span identifier followed, within forty
// characters on one line, by a parenthesised code-span Go file:
// "`Sym` ... (`file.go`)". The parenthesis must close straight after the file,
// so a comma list of files does not match. That is the strict grammar;
// admitting lists also admits fragments and file names read as symbols.
var symbolCitation = regexp.MustCompile("`([A-Za-z_][A-Za-z0-9_]*(?:\\.[A-Za-z_][A-Za-z0-9_]*)*)`[^`\\n]{0,40}\\(`([A-Za-z0-9_./-]+\\.go)`\\)")

// lineSpanCitation matches "file.ext:N", "file.ext:N-M" and the comma list
// "file.ext:N,M,K". Unlike symbolCitation it admits lists, because a list is
// exactly where a stale line hides: every number in it is a claim about the
// file's length. A match inside a fenced code block counts like any other: a
// `file.go:12` in a transcript is still a claim about that file.
var lineSpanCitation = regexp.MustCompile(`([A-Za-z0-9_./-]+\.(?:go|scm|sh|md)):([0-9]+(?:[-,][0-9]+)*)`)

// goComments removes block and line comments from Go source so that a symbol
// named only in a comment does not count as defined. It does not parse: a "//"
// inside a string literal also cuts the rest of its line, which can only turn a
// pass into a finding, never hide one.
var goComments = regexp.MustCompile(`(?s)/\*.*?\*/|//[^\n]*`)

// lineCitationCeiling is the number of "file:N" spans in tracked Markdown,
// resolved or not (see "Unresolvable citations" above). It is the value
// TestLineCitationCensusOnlyShrinks prints, with no slack, and it may only go
// down: line numbers rot with every edit, so the habit is retired by letting
// the count fall and never rise. Cite `file.go` and a symbol instead of a line.
//
// This is the first hand-maintained exact-equality ceiling in the repo; the
// others derive their bound or regenerate it. There is no regen mechanism here
// on purpose: the number is a claim a maintainer re-reads, and the failure
// message names the value to write.
const lineCitationCeiling = 130

// symbolCitationMatches is the number of symbol citations the grammar matches
// that resolve to a tracked file, pinned exactly in both directions. A
// citation that stops resolving (a rename, a move) drops it and fails; a new
// one raises it and fails until the pin is raised. Together with the
// per-citation findings this keeps the check from going vacuous or from
// quietly shrinking.
const symbolCitationMatches = 105

// sourceIndex resolves the file paths a doc cites to tracked files and reads
// them.
type sourceIndex struct {
	tracked map[string]bool
	byBase  map[string][]string
	read    func(file string) ([]byte, bool)
}

func newSourceIndex(tracked []string, read func(file string) ([]byte, bool)) *sourceIndex {
	q := &sourceIndex{
		tracked: map[string]bool{},
		byBase:  map[string][]string{},
		read:    read,
	}
	for _, file := range tracked {
		if file == "" {
			continue
		}
		q.tracked[file] = true
		base := path.Base(file)
		q.byBase[base] = append(q.byBase[base], file)
	}
	return q
}

// resolve maps a path as a doc writes it to the one tracked file it names.
// Docs cite a file by its full path, by a trailing part of it, or by its
// basename, and the text does not say which, so a trailing-components suffix
// match is the only resolution that serves all three. That differs from
// resolveRef above, which resolves Markdown targets: those are real
// doc-relative links whose directory is part of the claim, and a wrong
// directory there is the defect. Here a wrong directory is exactly what the
// symbol check wants to survive a move, so an ambiguous or untracked path
// resolves to nothing and is counted, not guessed.
func (p *sourceIndex) resolve(written string) (string, bool) {
	written = path.Clean(written)
	if p.tracked[written] {
		return written, true
	}
	var match []string
	for _, candidate := range p.byBase[path.Base(written)] {
		if strings.HasSuffix(candidate, "/"+written) {
			match = append(match, candidate)
		}
	}
	if len(match) != 1 {
		return "", false
	}
	return match[0], true
}

// lineOf is the 1-based line of body at byte offset off.
func lineOf(body string, off int) int {
	return strings.Count(body[:off], "\n") + 1
}

// symbolCitationFindings returns how many symbol citations in body the grammar
// matched, how many of those resolved to a readable file, and one finding per
// resolved citation whose file does not define the symbol. For a qualified
// name, Type.Method, the leaf is the part that is looked for. Comments in a Go
// file do not define anything.
func symbolCitationFindings(idx *sourceIndex, doc, body string) (matched, resolved int, findings []string) {
	for _, m := range symbolCitation.FindAllStringSubmatchIndex(body, -1) {
		matched++
		sym := body[m[2]:m[3]]
		written := body[m[4]:m[5]]
		file, ok := idx.resolve(written)
		if !ok {
			continue
		}
		src, ok := idx.read(file)
		if !ok {
			continue
		}
		resolved++
		leaf := sym[strings.LastIndex(sym, ".")+1:]
		word := regexp.MustCompile(`\b` + regexp.QuoteMeta(leaf) + `\b`)
		if word.Match(goComments.ReplaceAll(src, nil)) {
			continue
		}
		findings = append(findings, doc+":"+strconv.Itoa(lineOf(body, m[0]))+": cites `"+sym+"` in "+file+", which does not define it")
	}
	return matched, resolved, findings
}

// lineCount is the number of lines in src, counting an unterminated last line.
func lineCount(src []byte) int {
	q := bytes.Count(src, []byte("\n"))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		q++
	}
	return q
}

// spanProblem reports what is wrong with one "N" or "N-M" part of a line span
// against a file of the given length, or "" when it is sound. A line below 1, a
// reversed range, an end past the file and a number too large to parse are all
// the same defect: the citation names lines the file does not have.
func spanProblem(part string, lines int) string {
	lo, hi, isRange := strings.Cut(part, "-")
	if !isRange {
		hi = lo
	}
	first, err := strconv.Atoi(lo)
	if err != nil {
		return "unreadable line " + lo
	}
	last, err := strconv.Atoi(hi)
	if err != nil {
		return "unreadable line " + hi
	}
	if first < 1 {
		return "line " + lo + " is not a line"
	}
	if last < first {
		return "range " + part + " is reversed"
	}
	if last > lines {
		return "line " + hi + " is past the end"
	}
	return ""
}

// lineSpanFindings returns how many "file:N" spans in body the grammar matched,
// how many of those resolved to a readable file, and one finding per resolved
// span that names a line the file does not have.
func lineSpanFindings(idx *sourceIndex, doc, body string) (matched, resolved int, findings []string) {
	for _, m := range lineSpanCitation.FindAllStringSubmatchIndex(body, -1) {
		matched++
		written := body[m[2]:m[3]]
		numbers := body[m[4]:m[5]]
		file, ok := idx.resolve(written)
		if !ok {
			continue
		}
		src, ok := idx.read(file)
		if !ok {
			continue
		}
		resolved++
		lines := lineCount(src)
		for part := range strings.SplitSeq(numbers, ",") {
			problem := spanProblem(part, lines)
			if problem == "" {
				continue
			}
			findings = append(findings, doc+":"+strconv.Itoa(lineOf(body, m[0]))+": cites "+written+":"+numbers+", but "+file+" has "+strconv.Itoa(lines)+" lines: "+problem)
			break
		}
	}
	return matched, resolved, findings
}

// ratchetVerdict compares a measured count with the constant that pins it, in
// both directions, and returns the failure message or "" when they are equal.
// A pin that is only checked from one side drifts from the measurement.
func ratchetVerdict(what, constant string, got, pinned int) string {
	if got > pinned {
		return strconv.Itoa(got) + " " + what + ", " + constant + " is " + strconv.Itoa(pinned) + ": if citations were added, raise " + constant + " to " + strconv.Itoa(got) + " (a symbol outlives a line number)"
	}
	if got < pinned {
		return strconv.Itoa(got) + " " + what + ", " + constant + " is " + strconv.Itoa(pinned) + ": if a citation was deleted, lower " + constant + " to " + strconv.Itoa(got) + "; if one stopped resolving, find it"
	}
	return ""
}

// trackedDoc is one tracked Markdown file and its text.
type trackedDoc struct {
	name string
	body string
}

// trackedDocs is every tracked Markdown doc that is readable in the working
// tree, in `git ls-files` order. A doc deleted in the working tree has nothing
// to cite and is left out.
func trackedDocs(t *testing.T, root string) []trackedDoc {
	t.Helper()
	var q []trackedDoc
	for doc := range strings.SplitSeq(gitOutput(t, root, "ls-files", "-z", "--", "*.md"), "\x00") {
		if doc == "" {
			continue
		}
		body, err := os.ReadFile(filepath.Join(root, doc))
		if err != nil {
			continue
		}
		q = append(q, trackedDoc{name: doc, body: string(body)})
	}
	return q
}

// citationCorpus is every tracked Markdown doc and the index of tracked files
// its citations resolve against.
func citationCorpus(t *testing.T) (*sourceIndex, []trackedDoc) {
	t.Helper()
	root := gitOutput(t, ".", "rev-parse", "--show-toplevel")
	read := func(file string) ([]byte, bool) {
		src, err := os.ReadFile(filepath.Join(root, file))
		return src, err == nil
	}
	idx := newSourceIndex(strings.Split(gitOutput(t, root, "ls-files", "-z"), "\x00"), read)
	return idx, trackedDocs(t, root)
}

// TestCitedSymbolsExistWhereCited fails when a doc cites a symbol in a file
// that does not define it: a rename or deletion the prose never heard about.
// A symbol outlives a line number, so this is the citation form worth gating.
func TestCitedSymbolsExistWhereCited(t *testing.T) {
	idx, docs := citationCorpus(t)
	matched, resolved := 0, 0
	for _, d := range docs {
		m, r, findings := symbolCitationFindings(idx, d.name, d.body)
		matched += m
		resolved += r
		for _, finding := range findings {
			t.Error(finding)
		}
	}
	verdict := ratchetVerdict("resolved symbol citations", "symbolCitationMatches", resolved, symbolCitationMatches)
	if verdict != "" {
		t.Errorf("%s (the grammar matched %d, %d of them unresolvable)", verdict, matched, matched-resolved)
	}
}

// TestLineCitationCensusOnlyShrinks has two arms. A cited line the file does
// not have is a defect and fails outright, at zero. The count of "file:N"
// citations, resolved or not, is a ratchet: see "Unresolvable citations" above.
func TestLineCitationCensusOnlyShrinks(t *testing.T) {
	idx, docs := citationCorpus(t)
	matched, resolved := 0, 0
	for _, d := range docs {
		m, r, findings := lineSpanFindings(idx, d.name, d.body)
		matched += m
		resolved += r
		for _, finding := range findings {
			t.Error(finding)
		}
	}
	verdict := ratchetVerdict("line citations", "lineCitationCeiling", matched, lineCitationCeiling)
	if verdict != "" {
		t.Errorf("%s (%d of them resolve to a tracked file)", verdict, resolved)
	}
}

// citationFixture is a synthetic tree the checker tests resolve against.
func citationFixture() *sourceIndex {
	files := map[string]string{
		// five lines, terminated: line 6 does not exist
		"pkg/a/one.go": "package a\n\nfunc Kept() {}\n\nfunc (p *T) Method() {}\n",
		// Ghost and Block are named only in comments
		"pkg/a/ghost.go": "package a\n// Ghost is only a comment\n/* Block is too */\nfunc Real() {}\n",
		"pkg/a/dup.go":   "package a\n",
		"pkg/b/dup.go":   "package b\n",
		// three lines, the last unterminated
		"pkg/b/three.go": "a\nb\nc",
		"pkg/c/x.scm":    "(a)\n(b)\n",
		"tools/x.sh":     "a\nb\n",
		"docs/y.md":      "a\nb\n",
	}
	var tracked []string
	for file := range files {
		tracked = append(tracked, file)
	}
	return newSourceIndex(tracked, func(file string) ([]byte, bool) {
		src, ok := files[file]
		return []byte(src), ok
	})
}

// TestCitationCheckersCatchTheirTargets pins the two checkers on a synthetic
// tree. A checker that finds nothing passes the real corpus vacuously, so each
// positive, each skip and each count is asserted directly, with the exact
// finding text.
func TestCitationCheckersCatchTheirTargets(t *testing.T) {
	idx := citationFixture()
	gone := func(line, sym, file string) string {
		return "doc.md:" + line + ": cites `" + sym + "` in " + file + ", which does not define it"
	}
	past := func(line, cited, file, lines, problem string) string {
		return "doc.md:" + line + ": cites " + cited + ", but " + file + " has " + lines + " lines: " + problem
	}

	symbols := []struct {
		name     string
		body     string
		matched  int
		resolved int
		want     []string
	}{
		{"present", "`Kept` (`pkg/a/one.go`)", 1, 1, nil},
		{"present by basename", "`Kept` is in (`one.go`)", 1, 1, nil},
		{"qualified, leaf present", "`T.Method` (`one.go`)", 1, 1, nil},
		{"qualified, leaf absent", "`T.Gone` (`one.go`)", 1, 1, []string{gone("1", "T.Gone", "pkg/a/one.go")}},
		{"absent", "`Gone` (`pkg/a/one.go`)", 1, 1, []string{gone("1", "Gone", "pkg/a/one.go")}},
		{"finding names its line", "x\ny\n`Gone` (`one.go`)", 1, 1, []string{gone("3", "Gone", "pkg/a/one.go")}},
		{"only a substring of a word", "`Kep` (`one.go`)", 1, 1, []string{gone("1", "Kep", "pkg/a/one.go")}},
		{"named only in a line comment", "`Ghost` (`ghost.go`)", 1, 1, []string{gone("1", "Ghost", "pkg/a/ghost.go")}},
		{"named only in a block comment", "`Block` (`ghost.go`)", 1, 1, []string{gone("1", "Block", "pkg/a/ghost.go")}},
		{"defined beside comments", "`Real` (`ghost.go`)", 1, 1, nil},
		{"dot-slash path resolves", "`Gone` (`./pkg/a/one.go`)", 1, 1, []string{gone("1", "Gone", "pkg/a/one.go")}},
		{"gap of forty characters matches", "`Gone` " + strings.Repeat("x", 38) + " (`one.go`)", 1, 1, []string{gone("1", "Gone", "pkg/a/one.go")}},
		{"gap of forty-one does not", "`Gone` " + strings.Repeat("x", 39) + " (`one.go`)", 0, 0, nil},
		{"two citations, one bad", "`Kept` (`one.go`) `Gone` (`one.go`)", 2, 2, []string{gone("1", "Gone", "pkg/a/one.go")}},
		{"a backtick in the gap pairs the nearer symbol", "`Gone` `Kept` (`one.go`)", 1, 1, nil},
		{"a newline in the gap does not match", "`Gone`\n(`one.go`)", 0, 0, nil},
		{"suffix must sit on a path boundary", "`Gone` (`g/a/one.go`)", 1, 0, nil},
		{"ambiguous basename is counted, not checked", "`Gone` (`dup.go`)", 1, 0, nil},
		{"untracked file is counted, not checked", "`Gone` (`nowhere.go`)", 1, 0, nil},
		{"a file list is outside the strict grammar", "`Gone` (`one.go`, `dup.go`)", 0, 0, nil},
	}
	for _, tc := range symbols {
		t.Run("symbol/"+tc.name, func(t *testing.T) {
			matched, resolved, got := symbolCitationFindings(idx, "doc.md", tc.body)
			if matched != tc.matched || resolved != tc.resolved || !slices.Equal(got, tc.want) {
				t.Errorf("matched %d resolved %d findings %q\nwant %d, %d, %q",
					matched, resolved, got, tc.matched, tc.resolved, tc.want)
			}
		})
	}

	lines := []struct {
		name     string
		body     string
		matched  int
		resolved int
		want     []string
	}{
		{"in range", "pkg/b/three.go:3", 1, 1, nil},
		{"unterminated last line counts", "three.go:3", 1, 1, nil},
		{"last line of a terminated file", "pkg/a/one.go:5", 1, 1, nil},
		{"one past a terminated file", "pkg/a/one.go:6", 1, 1, []string{past("1", "pkg/a/one.go:6", "pkg/a/one.go", "5", "line 6 is past the end")}},
		{"one past the end", "three.go:4", 1, 1, []string{past("1", "three.go:4", "pkg/b/three.go", "3", "line 4 is past the end")}},
		{"range end past the end", "three.go:1-9", 1, 1, []string{past("1", "three.go:1-9", "pkg/b/three.go", "3", "line 9 is past the end")}},
		{"range in range", "three.go:1-3", 1, 1, nil},
		{"reversed range", "three.go:3-1", 1, 1, []string{past("1", "three.go:3-1", "pkg/b/three.go", "3", "range 3-1 is reversed")}},
		{"line zero", "three.go:0", 1, 1, []string{past("1", "three.go:0", "pkg/b/three.go", "3", "line 0 is not a line")}},
		{"a number too large to parse", "three.go:99999999999999999999", 1, 1, []string{past("1", "three.go:99999999999999999999", "pkg/b/three.go", "3", "unreadable line 99999999999999999999")}},
		{"a range end too large to parse", "three.go:1-99999999999999999999", 1, 1, []string{past("1", "three.go:1-99999999999999999999", "pkg/b/three.go", "3", "unreadable line 99999999999999999999")}},
		{"one stale number in a comma list", "three.go:1,2,40", 1, 1, []string{past("1", "three.go:1,2,40", "pkg/b/three.go", "3", "line 40 is past the end")}},
		{"two stale numbers in a list is one finding", "three.go:1,40,50", 1, 1, []string{past("1", "three.go:1,40,50", "pkg/b/three.go", "3", "line 40 is past the end")}},
		{"a comma list in range", "three.go:1,2,3", 1, 1, nil},
		{"two spans, one stale", "three.go:4 three.go:1", 2, 2, []string{past("1", "three.go:4", "pkg/b/three.go", "3", "line 4 is past the end")}},
		{"a duplicate span counts twice", "three.go:1 three.go:1", 2, 2, nil},
		{"finding names its line", "x\nthree.go:9", 1, 1, []string{past("2", "three.go:9", "pkg/b/three.go", "3", "line 9 is past the end")}},
		{"a span inside a fenced block counts", "```\nthree.go:99\n```", 1, 1, []string{past("2", "three.go:99", "pkg/b/three.go", "3", "line 99 is past the end")}},
		{"scheme in range", "x.scm:2", 1, 1, nil},
		{"scheme past the end", "x.scm:3", 1, 1, []string{past("1", "x.scm:3", "pkg/c/x.scm", "2", "line 3 is past the end")}},
		{"shell past the end", "x.sh:3", 1, 1, []string{past("1", "x.sh:3", "tools/x.sh", "2", "line 3 is past the end")}},
		{"markdown past the end", "y.md:3", 1, 1, []string{past("1", "y.md:3", "docs/y.md", "2", "line 3 is past the end")}},
		{"ambiguous basename is counted, not checked", "dup.go:99", 1, 0, nil},
		{"untracked file is counted, not checked", "nowhere.go:99", 1, 0, nil},
		{"a URL does not resolve", "https://example.com/x/three.go:99", 1, 0, nil},
	}
	for _, tc := range lines {
		t.Run("line/"+tc.name, func(t *testing.T) {
			matched, resolved, got := lineSpanFindings(idx, "doc.md", tc.body)
			if matched != tc.matched || resolved != tc.resolved || !slices.Equal(got, tc.want) {
				t.Errorf("matched %d resolved %d findings %q\nwant %d, %d, %q",
					matched, resolved, got, tc.matched, tc.resolved, tc.want)
			}
		})
	}
}

// TestRatchetVerdictIsExactInBothDirections pins the comparison the two
// ratchets share: equal passes, and a count one off in either direction fails
// with the instruction for that direction.
func TestRatchetVerdictIsExactInBothDirections(t *testing.T) {
	tcs := []struct {
		name string
		got  int
		want string
	}{
		{"below", 104, "104 things, pin is 105: if a citation was deleted, lower pin to 104; if one stopped resolving, find it"},
		{"equal", 105, ""},
		{"above", 106, "106 things, pin is 105: if citations were added, raise pin to 106 (a symbol outlives a line number)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := ratchetVerdict("things", "pin", tc.got, 105)
			if got != tc.want {
				t.Errorf("ratchetVerdict(%d, 105) = %q, want %q", tc.got, got, tc.want)
			}
		})
	}
}
