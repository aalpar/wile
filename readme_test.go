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

package wile_test

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"time"
)

// TestREADMEGoSnippetsCompile extracts Go code blocks from README.md,
// wraps each in a compilable scaffold, and verifies they type-check
// against the current wile API. This catches signature drift between
// the documented examples and the actual public API (e.g., a missing
// context parameter, a renamed method, a changed return type).
func TestREADMEGoSnippetsCompile(t *testing.T) {
	readme, err := os.ReadFile("README.md")
	if err != nil {
		t.Fatalf("read README.md: %v", err)
	}

	blocks := extractGoCodeBlocks(string(readme))
	if len(blocks) == 0 {
		t.Fatal("no Go code blocks found in README.md")
	}

	repoRoot, err := filepath.Abs(".")
	if err != nil {
		t.Fatalf("abs path: %v", err)
	}

	tmpDir := t.TempDir()
	writeREADMETestGoMod(t, tmpDir, repoRoot)

	source := assembleREADMESource(blocks)
	srcPath := filepath.Join(tmpDir, "main.go")
	err = os.WriteFile(srcPath, []byte(source), 0o644)
	if err != nil {
		t.Fatalf("write main.go: %v", err)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	// Resolve transitive dependencies before building.
	tidyCmd := exec.CommandContext(ctx, "go", "mod", "tidy")
	tidyCmd.Dir = tmpDir
	tidyOutput, err := tidyCmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go mod tidy failed:\n%s", tidyOutput)
	}

	buildCmd := exec.CommandContext(ctx, "go", "build", ".")
	buildCmd.Dir = tmpDir
	buildOutput, err := buildCmd.CombinedOutput()
	if err != nil {
		t.Fatalf("README Go snippets failed to compile:\n%s\n\nGenerated source:\n%s", buildOutput, source)
	}

	t.Logf("verified %d Go code blocks from README.md compile", len(blocks))
}

// goCodeBlockRe matches fenced ```go code blocks in Markdown.
// The \r? allows matching both LF and CRLF line endings.
var goCodeBlockRe = regexp.MustCompile("(?s)```go\\r?\\n(.*?)\\r?\\n```")

// importLineRe matches standalone import "pkg" lines.
var importLineRe = regexp.MustCompile(`^\s*import\s+"([^"]+)"`)

// assignRe matches short variable declarations at the start of a line.
var assignRe = regexp.MustCompile(`^(\w+(?:\s*,\s*\w+)*)\s*:=`)

func extractGoCodeBlocks(readme string) []string {
	matches := goCodeBlockRe.FindAllStringSubmatch(readme, -1)
	blocks := make([]string, len(matches))
	for i, m := range matches {
		blocks[i] = m[1]
	}
	return blocks
}

func writeREADMETestGoMod(t *testing.T, dir, repoRoot string) {
	t.Helper()
	content := fmt.Sprintf(
		"module readme_check\n\ngo 1.23\n\nrequire github.com/aalpar/wile v0.0.0\n\nreplace github.com/aalpar/wile => %q\n",
		repoRoot,
	)
	err := os.WriteFile(filepath.Join(dir, "go.mod"), []byte(content), 0o644)
	if err != nil {
		t.Fatalf("write go.mod: %v", err)
	}
}

// assembleREADMESource builds a single Go source file where each README
// code block becomes its own function. Import lines are hoisted to the
// file level, a preamble provides engine/ctx when the block doesn't
// create its own, and unused top-level variables are silenced.
func assembleREADMESource(blocks []string) string {
	// Collect all imports (standard set + any found in blocks).
	imports := map[string]bool{
		`"context"`:                       true,
		`"fmt"`:                           true,
		`"log"`:                           true,
		`"github.com/aalpar/wile"`:        true,
		`"github.com/aalpar/wile/values"`: true,
		`"github.com/aalpar/wile/extensions/files"`: true,
		`"github.com/aalpar/wile/security"`:         true,
	}

	// Strip import lines from blocks and collect them.
	cleaned := make([]string, len(blocks))
	for i, block := range blocks {
		var lines []string
		for line := range strings.SplitSeq(block, "\n") {
			sub := importLineRe.FindStringSubmatch(line)
			if sub != nil {
				imports[`"`+sub[1]+`"`] = true
				continue
			}
			lines = append(lines, line)
		}
		cleaned[i] = strings.Join(lines, "\n")
	}

	var src strings.Builder

	src.WriteString("package main\n\n")

	// Imports.
	src.WriteString("import (\n")
	for imp := range imports {
		fmt.Fprintf(&src, "\t%s\n", imp)
	}
	src.WriteString(")\n\n")

	// Suppress unused import errors.
	src.WriteString("var (\n")
	src.WriteString("\t_ = fmt.Println\n")
	src.WriteString("\t_ = log.Fatal\n")
	src.WriteString("\t_ = values.NewInteger\n")
	src.WriteString("\t_ = files.Extension\n")
	src.WriteString("\t_ = security.DenyAll\n")
	src.WriteString("\t_ context.Context\n")
	src.WriteString(")\n\n")

	// Empty main — required for package main to compile.
	src.WriteString("func main() {}\n\n")

	// Each block becomes its own function.
	for i, block := range cleaned {
		fmt.Fprintf(&src, "func readme_%d() {\n", i)

		// Preamble: provide engine and ctx if the block doesn't create them.
		var preambleVars []string
		src.WriteString("\tctx := context.Background()\n")
		preambleVars = append(preambleVars, "ctx")
		if !strings.Contains(block, "wile.NewEngine(") {
			src.WriteString("\tengine, _ := wile.NewEngine(ctx)\n")
			preambleVars = append(preambleVars, "engine")
		}
		src.WriteString("\n")

		// Write the block body.
		for line := range strings.SplitSeq(block, "\n") {
			if strings.TrimSpace(line) == "" {
				src.WriteString("\n")
			} else {
				fmt.Fprintf(&src, "\t%s\n", line)
			}
		}

		// Determine which variables are declared at the block's top level
		// (brace depth 0). Variables inside closures are skipped.
		topLevelVars := readmeTopLevelVars(block)

		// Silence unused variables from both preamble and block.
		silenced := map[string]bool{}
		for _, v := range preambleVars {
			if !silenced[v] {
				fmt.Fprintf(&src, "\t_ = %s\n", v)
				silenced[v] = true
			}
		}
		for _, v := range topLevelVars {
			if !silenced[v] {
				fmt.Fprintf(&src, "\t_ = %s\n", v)
				silenced[v] = true
			}
		}

		src.WriteString("}\n\n")
	}

	return src.String()
}

// readmeTopLevelVars returns variable names declared with := at brace
// depth 0 in the block. Variables inside closures (depth > 0) are excluded.
//
// Limitation: brace counting uses strings.Count, which does not skip braces
// inside string literals or comments. This is sufficient for the current
// README snippets but would need a proper tokenizer if future examples
// contain braces in strings (e.g., fmt.Sprintf("{%s}")).
func readmeTopLevelVars(block string) []string {
	braceDepth := 0
	seen := map[string]bool{}
	var vars []string

	for line := range strings.SplitSeq(block, "\n") {
		trimmed := strings.TrimSpace(line)

		// Skip comment lines.
		if strings.HasPrefix(trimmed, "//") {
			continue
		}

		// At brace depth 0, look for short variable declarations.
		if braceDepth == 0 {
			sub := assignRe.FindStringSubmatch(trimmed)
			if sub != nil {
				for v := range strings.SplitSeq(sub[1], ",") {
					v = strings.TrimSpace(v)
					if v != "_" && !seen[v] {
						vars = append(vars, v)
						seen[v] = true
					}
				}
			}
		}

		braceDepth += strings.Count(trimmed, "{") - strings.Count(trimmed, "}")
	}

	return vars
}
