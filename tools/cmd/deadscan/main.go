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

// Command deadscan reports exported symbols that no production code consumes.
//
// It is a REPORTER, not a gate: it exits 0 on a successful scan and is not
// wired into `make lint`.
//
// The census itself lives in the importable tools/deadscan package, which
// documents what "dead" means, the four pins a reference count cannot see, and
// the known limits. This file is the flags and the formatting.
//
//	go run ./tools/cmd/deadscan ./...
//	go run ./tools/cmd/deadscan -json ./... > syms.json
//	make -C tools deadscan
//
// Pass every workspace module whose consumers should count, or the ext column
// is zero and the dead list is overstated. The report says how many modules it
// saw so that mistake is loud rather than silent.
package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"sort"
	"strings"

	ds "github.com/aalpar/wile/tools/deadscan"
)

func main() {
	asJSON := flag.Bool("json", false, "emit every scanned symbol as JSON instead of the report")
	dir := flag.String("dir", ".", "directory to load packages from")
	flag.Parse()
	patterns := flag.Args()
	if len(patterns) == 0 {
		patterns = []string{"./..."}
	}

	r, err := ds.Load(*dir, patterns)
	if err != nil {
		fmt.Fprintln(os.Stderr, "deadscan:", err)
		os.Exit(1)
	}
	if *asJSON {
		writeJSON(r)
		return
	}
	report(r)
}

func writeJSON(r *ds.Result) {
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", " ")
	_ = enc.Encode(r.Symbols)
}

// report prints the census: the totals, the pin breakdown, and the standalone
// dead rows grouped by package. Rows that are not standalone are counted but
// not listed — they are leads for a cluster, not entries on a work list.
func report(r *ds.Result) {
	all := r.Symbols
	loc, dead, deadLOC, cluster, positional := 0, 0, 0, 0, 0
	pins := map[string]int{}
	perPkg := map[string][]*ds.Symbol{}
	for _, s := range all {
		loc += s.LOC
		if s.Pin != "" {
			pins[s.Pin]++
		}
		if s.Live {
			continue
		}
		dead++
		switch {
		case len(s.ClusterWith) > 0:
			cluster++
		case !s.Standalone():
			positional++
		default:
			deadLOC += s.LOC
			perPkg[s.PkgPath] = append(perPkg[s.PkgPath], s)
		}
	}

	fmt.Printf("modules loaded: %d %v\n", len(r.Modules), r.Modules)
	if len(r.Modules) < 2 {
		fmt.Println("WARNING: one module only — the ext column is zero and the dead list is overstated")
	}
	fmt.Printf("exported symbols: %d (%d LOC)\n", len(all), loc)
	ext := ds.ExtConsumers(all)
	if len(ext) > 0 {
		shared := ""
		n := ds.ExtShared(all)
		if n > 0 {
			shared = fmt.Sprintf(" (%d shared)", n)
		}
		fmt.Printf("ext consumers: %d symbols — %s%s\n", ds.ExtConsumed(all), renderConsumers(ext), shared)
	}
	fmt.Printf("dead: %d — standalone %d (%d LOC), cluster-only %d, iota member %d\n",
		dead, dead-cluster-positional, deadLOC, cluster, positional)
	fmt.Printf("pins: %v\n\n", pins)

	for _, pkg := range sortedPkgs(perPkg) {
		ss := perPkg[pkg]
		n := 0
		for _, s := range ss {
			n += s.LOC
		}
		fmt.Printf("%s  (%d symbols, %d LOC)\n", pkg, len(ss), n)
		for _, s := range ss {
			fmt.Printf("  %-46s %-7s prod=%d test=%d  %s\n",
				s.DisplayName(), s.Kind, s.ProdRefs, s.TestRefs, s.Pos)
		}
	}
}

func renderConsumers(cs []ds.ConsumerCount) string {
	parts := make([]string, 0, len(cs))
	for _, c := range cs {
		parts = append(parts, fmt.Sprintf("%s %d", c.Module, c.Syms))
	}
	return strings.Join(parts, ", ")
}

func sortedPkgs(m map[string][]*ds.Symbol) []string {
	q := make([]string, 0, len(m))
	for k := range m {
		q = append(q, k)
	}
	sort.Strings(q)
	return q
}
