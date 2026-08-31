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

// Command cxmeasure reports cyclomatic and cognitive complexity per function,
// file, and package, so that refactoring is aimed at measured density rather
// than at whichever file someone happened to be reading.
//
// It is a REPORTER, not a gate: it always exits 0 on a successful scan and is
// deliberately not wired into `make lint`. Its three siblings under tools/cmd
// (singlelinefunclint, typeswitchlint, nestinglint) do gate the build; this one
// answers "where is the complexity" rather than "is this line legal".
//
// The measurement itself lives in the importable tools/cxmeasure package, which
// documents both metrics, the wide-structure test, and the known limits. This
// file is the flags and the formatting.
//
//	go run ./tools/cmd/cxmeasure -by func -top 25 .
//	go run ./tools/cmd/cxmeasure -arms pkg/machine/machine_context.go:Run
package main

import (
	"flag"
	"fmt"
	"os"
	"sort"
	"strings"

	cx "github.com/aalpar/wile/tools/cxmeasure"
)

func main() {
	by := flag.String("by", "func", "aggregation level: func, file, or pkg")
	top := flag.Int("top", defaultTopEntries, "how many rows to print")
	arms := flag.String("arms", "", "re-measure one function's switch arms in isolation: <file.go>:<FuncName>")
	flag.Parse()

	if *arms != "" {
		reportArms(*arms)
		return
	}

	dirs := flag.Args()
	if len(dirs) == 0 {
		dirs = []string{"."}
	}

	var stats []cx.FuncStat
	for _, dir := range dirs {
		found, skipped, err := cx.ScanDir(dir)
		if err != nil {
			fmt.Fprintf(os.Stderr, "cxmeasure: scanning %s: %v\n", dir, err)
			os.Exit(2)
		}
		for _, s := range skipped {
			fmt.Fprintf(os.Stderr, "cxmeasure: warning: skipping %s: %v\n", s.Path, s.Err)
		}
		stats = append(stats, found...)
	}

	switch *by {
	case "func":
		reportFuncs(stats, *top)
	case "file":
		reportGroups(stats, *top, "file", groupByFile, minFuncsPerFile, 0)
	case "pkg":
		reportGroups(stats, *top, "package", groupByPkg, 0, minLOCPerPackage)
	default:
		fmt.Fprintf(os.Stderr, "cxmeasure: unknown -by value %q (want func, file, or pkg)\n", *by)
		os.Exit(2)
	}
}

// reportArms formats the wide-structure test. Every number it prints is
// computed by cx.Arms; this function only decides how to say it.
func reportArms(spec string) {
	cut := strings.LastIndex(spec, ":")
	if cut < 0 {
		fmt.Fprintf(os.Stderr, "cxmeasure: -arms wants <file.go>:<FuncName>, got %q\n", spec)
		os.Exit(2)
	}
	r, err := cx.Arms(spec[:cut], spec[cut+1:])
	if err != nil {
		fmt.Fprintf(os.Stderr, "cxmeasure: %v\n", err)
		os.Exit(2)
	}

	fmt.Printf("%s (%s)\n", r.Func, r.Path)
	fmt.Printf("  as written:        cognitive %d, cyclomatic %d\n", r.Own.Cognitive, r.Own.Cyclomatic)
	fmt.Printf("  widest switch:     %d arms, accounting for %.0f%% of the function's score\n", len(r.Arms), r.Share*100)
	fmt.Printf("  arms extracted:    cognitive %d\n", r.Extracted)
	fmt.Printf("  arm distribution:  %d scoring 0, %d scoring 1-4, %d scoring 5+\n", r.Trivial, r.Modest, r.Heavy)
	switch r.Verdict {
	case cx.VerdictInconclusive:
		fmt.Printf("  verdict: INCONCLUSIVE. This switch is incidental; the function's complexity is elsewhere.\n")
		return
	case cx.VerdictWide:
		fmt.Printf("  verdict: WIDE. Every arm is trivial in isolation; splitting relocates the score.\n")
		return
	case cx.VerdictComplex:
	}
	fmt.Printf("  arms scoring 5 or more:\n")
	for _, a := range r.Arms {
		if a.Cognitive < 5 {
			continue
		}
		fmt.Printf("    cog %3d  lines %3d  %s\n", a.Cognitive, a.Lines, a.Label)
	}
}

// minFuncsPerFile and minLOCPerPackage keep a one-function file or a two-file
// package from topping a density ranking on a sample of one. Rows below the
// threshold are counted and reported in a footer rather than dropped silently.
const (
	minFuncsPerFile   = 3
	minLOCPerPackage  = 2000
	notableCognitive  = 25
	seriousCognitive  = 50
	defaultTopEntries = 25
)

// group is an aggregate over functions, used for both file and package rollups.
type group struct {
	Key       string
	Cognitive int
	Cyclo     int
	Funcs     int
	Lines     int
	Files     map[string]bool
	Notable   int
	Serious   int
	Worst     cx.FuncStat
}

// reportFuncs prints the worst individual functions, cognitive-first. The cyc
// column is there to be compared against cog: when it is the larger of the two,
// the function is wide and -arms should be run before filing it.
func reportFuncs(stats []cx.FuncStat, top int) {
	sort.Slice(stats, func(i, j int) bool {
		if stats[i].Cognitive != stats[j].Cognitive {
			return stats[i].Cognitive > stats[j].Cognitive
		}
		return stats[i].File < stats[j].File
	})

	fmt.Printf("%d functions measured\n\n", len(stats))
	fmt.Printf("%6s %6s %6s  %s\n", "cog", "cyc", "lines", "location")
	for i, s := range stats {
		if i >= top {
			break
		}
		fmt.Printf("%6d %6d %6d  %s:%d %s\n", s.Cognitive, s.Cyclomatic, s.Lines, s.File, s.Line, s.Name)
	}
	if len(stats) > top {
		fmt.Printf("\n(%d further functions not shown; raise -top to see them)\n", len(stats)-top)
	}
}

// groupByFile and groupByPkg select the rollup key.
func groupByFile(s cx.FuncStat) string {
	return s.File
}

func groupByPkg(s cx.FuncStat) string {
	return s.Pkg
}

// reportGroups rolls functions up by key and ranks by density. Files rank by
// cognitive per function (how hard the average function is); packages rank by
// cognitive per function-LOC (how tangled the package is per line it spends).
// Rows below the relevant floor are suppressed and counted, never dropped
// silently: a one-function file would otherwise top a per-function ranking.
func reportGroups(stats []cx.FuncStat, top int, label string, key func(cx.FuncStat) string, minFuncs, minLines int) {
	groups := make(map[string]*group)
	for _, s := range stats {
		k := key(s)
		g, seen := groups[k]
		if !seen {
			g = &group{Key: k, Files: make(map[string]bool)}
			groups[k] = g
		}
		g.Cognitive += s.Cognitive
		g.Cyclo += s.Cyclomatic
		g.Funcs++
		g.Lines += s.Lines
		g.Files[s.File] = true
		if s.Cognitive > notableCognitive {
			g.Notable++
		}
		if s.Cognitive > seriousCognitive {
			g.Serious++
		}
		if s.Cognitive > g.Worst.Cognitive {
			g.Worst = s
		}
	}

	var ranked []*group
	suppressed := 0
	for _, g := range groups {
		if g.Funcs < minFuncs || g.Lines < minLines {
			suppressed++
			continue
		}
		ranked = append(ranked, g)
	}

	sort.Slice(ranked, func(i, j int) bool {
		a, b := density(ranked[i], minLines > 0), density(ranked[j], minLines > 0)
		if a != b {
			return a > b
		}
		return ranked[i].Key < ranked[j].Key
	})

	fmt.Printf("%d %ss ranked by %s\n\n", len(ranked), label, densityLabel(minLines > 0))
	fmt.Printf("%7s %7s %6s %6s %8s %8s %5s %5s  %s\n",
		"cog", "cyc", "fns", "files", "fnLOC", "density", ">25", ">50", label)
	for i, g := range ranked {
		if i >= top {
			break
		}
		fmt.Printf("%7d %7d %6d %6d %8d %8.3f %5d %5d  %s  (worst: %s=%d)\n",
			g.Cognitive, g.Cyclo, g.Funcs, len(g.Files), g.Lines,
			density(g, minLines > 0), g.Notable, g.Serious, g.Key, g.Worst.Name, g.Worst.Cognitive)
	}
	if len(ranked) > top {
		fmt.Printf("\n(%d further %ss not shown; raise -top to see them)\n", len(ranked)-top, label)
	}
	if suppressed > 0 {
		fmt.Printf("(%d %ss below the ranking floor of %d functions / %d function-LOC, suppressed)\n",
			suppressed, label, minFuncs, minLines)
	}
}

// density is cognitive per function-LOC when perLine is set, else per function.
func density(g *group, perLine bool) float64 {
	if perLine {
		if g.Lines == 0 {
			return 0
		}
		return float64(g.Cognitive) / float64(g.Lines)
	}
	if g.Funcs == 0 {
		return 0
	}
	return float64(g.Cognitive) / float64(g.Funcs)
}

func densityLabel(perLine bool) string {
	if perLine {
		return "cognitive per function-LOC"
	}
	return "cognitive per function"
}
