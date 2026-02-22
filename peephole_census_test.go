package wile

import (
	"context"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	"github.com/aalpar/wile/machine"
)

// TestPeepholeCensus compiles all Gabriel benchmarks and counts instruction
// pair/triple frequencies to identify peephole optimization candidates.
// Run with: go test -v -run TestPeepholeCensus .
func TestPeepholeCensus(t *testing.T) {
	ctx := context.Background()

	files, err := filepath.Glob("examples/benchmarks/*.scm")
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatal("no benchmark files found")
	}

	pairs := map[string]int{}
	triples := map[string]int{}
	singles := map[string]int{}
	totalInstrs := 0
	totalTemplates := 0

	for _, file := range files {
		src, err := os.ReadFile(file)
		if err != nil {
			t.Logf("skip %s: %v", file, err)
			continue
		}

		engine, err := NewEngine(ctx)
		if err != nil {
			t.Fatalf("engine: %v", err)
		}

		cc, err := engine.Compile(ctx, string(src))
		if err != nil {
			t.Logf("skip %s: %v", filepath.Base(file), err)
			continue
		}

		var walk func(tmpl *machine.NativeTemplate)
		walk = func(tmpl *machine.NativeTemplate) {
			code := tmpl.Code()
			totalTemplates++
			totalInstrs += len(code)

			for i := range code {
				singles[opName(code[i])]++
			}

			for i := 0; i < len(code)-1; i++ {
				key := opName(code[i]) + " → " + opName(code[i+1])
				pairs[key]++
			}

			for i := 0; i < len(code)-2; i++ {
				key := opName(code[i]) + " → " + opName(code[i+1]) + " → " + opName(code[i+2])
				triples[key]++
			}

			// Recurse into sub-templates in literals pool
			for _, lit := range tmpl.Literals() {
				sub, ok := lit.(*machine.NativeTemplate)
				if ok {
					walk(sub)
				}
			}
		}

		walk(cc.template)
		t.Logf("compiled %s", filepath.Base(file))
	}

	t.Logf("\n=== TOTALS ===")
	t.Logf("Templates: %d", totalTemplates)
	t.Logf("Instructions: %d", totalInstrs)

	t.Logf("\n=== SINGLES (top 20) ===")
	printTopN(t, singles, 20, totalInstrs)

	t.Logf("\n=== PAIRS (top 30) ===")
	printTopN(t, pairs, 30, totalInstrs)

	t.Logf("\n=== TRIPLES (top 30) ===")
	printTopN(t, triples, 30, totalInstrs)

	// Print optimization candidates from pairs
	t.Logf("\n=== OPTIMIZATION CANDIDATES (from pairs) ===")
	sortedPairs := sortMap(pairs)
	for _, s := range sortedPairs {
		note := classifyPattern(s.key)
		if note != "" {
			pct := float64(s.count) / float64(totalInstrs) * 100
			t.Logf("  %-50s  %5d (%5.1f%%) -- %s", s.key, s.count, pct, note)
		}
	}
}

func opName(instr machine.Instruction) string {
	return instr.Op.String()
}

type kv struct {
	key   string
	count int
}

func sortMap(m map[string]int) []kv {
	sorted := make([]kv, 0, len(m))
	for k, v := range m {
		sorted = append(sorted, kv{k, v})
	}
	sort.Slice(sorted, func(i, j int) bool {
		return sorted[i].count > sorted[j].count
	})
	return sorted
}

func printTopN(t *testing.T, m map[string]int, n int, total int) {
	sorted := sortMap(m)
	if len(sorted) > n {
		sorted = sorted[:n]
	}

	maxKey := 0
	for _, s := range sorted {
		if len(s.key) > maxKey {
			maxKey = len(s.key)
		}
	}

	for i, s := range sorted {
		pct := float64(s.count) / float64(total) * 100
		bar := strings.Repeat("█", int(pct*2))
		t.Logf("  %2d. %-*s  %5d (%5.1f%%) %s", i+1, maxKey, s.key, s.count, pct, bar)
	}
}

func classifyPattern(pattern string) string {
	parts := strings.Split(pattern, " → ")
	if len(parts) != 2 {
		return ""
	}
	a, b := parts[0], parts[1]

	switch {
	// Branch chain threading
	case a == "BranchOnFalseValue" && b == "Branch":
		return "BRANCH CHAIN: cond/case fallthrough, could thread"

	// Store then immediate Load of same kind
	case a == "StoreLocal" && b == "LoadLocal":
		return "REDUNDANT LOAD: value still in value register"
	case a == "StoreGlobal" && b == "LoadGlobal":
		return "REDUNDANT LOAD: value still in value register"

	// LoadVoid before branch
	case a == "LoadVoid" && b == "BranchOnFalseValue":
		return "CONSTANT BRANCH: void is truthy, branch never taken"

	// Pop+Push roundtrip
	case a == "Pop" && b == "Push":
		return "POP+PUSH: stack→register→stack roundtrip"

	// Dead code after unconditional control transfer
	case a == "RestoreContinuation" && b != "Branch" && b != "BranchOnFalseValue":
		return "POSSIBLY DEAD CODE after return"

	// Additional fusion candidates
	case a == "LoadLiteral" && b == "BranchOnFalseValue":
		return "CONSTANT FOLD: literal value determines branch statically"
	case a == "LoadVoid" && b == "Push":
		return "FUSE CANDIDATE: PushVoid"
	case a == "Pop" && b == "StoreLocal":
		return "FUSE CANDIDATE: PopStoreLocal"
	case a == "Pop" && b == "StoreGlobal":
		return "FUSE CANDIDATE: PopStoreGlobal"

	default:
		return ""
	}
}
