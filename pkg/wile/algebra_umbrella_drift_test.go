package wile_test

import (
	"maps"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

// TestAlgebraUmbrellaCoversLeafExports guards against drift between the
// per-library (wile algebra X) manifests and the umbrella (wile algebra)
// manifest. Every identifier exported by a leaf library must also be
// exported by the umbrella, or `(import (wile algebra))` consumers will
// silently miss it.
//
// If this test fails with "missing from umbrella: ...", add the listed
// symbols to stdlib/lib/wile/algebra.sld's (export ...) clause.
func TestAlgebraUmbrellaCoversLeafExports(t *testing.T) {
	c := qt.New(t)

	// stdlib/ lives at pkg/stdlib; this package is at pkg/wile/ (sibling under pkg/).
	const umbrellaPath = "../stdlib/lib/wile/algebra.sld"
	const leafDir = "../stdlib/lib/wile/algebra"

	umbrellaExports, err := readSldExports(umbrellaPath)
	c.Assert(err, qt.IsNil, qt.Commentf("reading umbrella %s", umbrellaPath))

	umbrellaSet := values.NewStringSet(len(umbrellaExports)).SetAll(umbrellaExports...)

	entries, err := os.ReadDir(leafDir)
	c.Assert(err, qt.IsNil, qt.Commentf("listing %s", leafDir))

	missing := map[string][]string{} // leaf -> symbols not in umbrella

	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".sld") {
			continue
		}
		leafPath := filepath.Join(leafDir, e.Name())
		leafExports, err := readSldExports(leafPath)
		c.Assert(err, qt.IsNil, qt.Commentf("reading leaf %s", leafPath))

		for _, sym := range leafExports {
			ok := umbrellaSet.Get(sym)
			if !ok {
				missing[e.Name()] = append(missing[e.Name()], sym)
			}
		}
	}

	if len(missing) == 0 {
		return
	}

	leaves := slices.Sorted(maps.Keys(missing))

	var b strings.Builder
	b.WriteString("umbrella algebra.sld is missing leaf exports:\n")
	for _, leaf := range leaves {
		syms := missing[leaf]
		slices.Sort(syms)
		b.WriteString("  ")
		b.WriteString(leaf)
		b.WriteString(": ")
		b.WriteString(strings.Join(syms, " "))
		b.WriteString("\n")
	}
	t.Fatal(b.String())
}

// readSldExports extracts every identifier from the first (export ...) form
// in a define-library .sld file. Handles ;-line comments; assumes no quoted
// strings or block comments inside the export clause (true for every file
// in stdlib/lib/wile/algebra/ today).
func readSldExports(path string) ([]string, error) {
	raw, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	src := stripLineComments(string(raw))

	start := strings.Index(src, "(export")
	if start < 0 {
		return nil, nil
	}
	// Walk paren nesting starting after "(export".
	i := start + len("(export")
	depth := 1
	var body strings.Builder
	for i < len(src) && depth > 0 {
		ch := src[i]
		switch ch {
		case '(':
			depth++
			body.WriteByte(ch)
		case ')':
			depth--
			if depth > 0 {
				body.WriteByte(ch)
			}
		default:
			body.WriteByte(ch)
		}
		i++
	}

	// Collect identifier-shaped tokens. R7RS export may include (rename
	// internal external) forms; we conservatively accept every bare
	// identifier token and ignore the keyword "rename".
	tokRe := regexp.MustCompile(`[A-Za-z!$%&*+\-./<=>?@^_~][A-Za-z0-9!$%&*+\-./<=>?@^_~]*`)
	tokens := tokRe.FindAllString(body.String(), -1)

	out := make([]string, 0, len(tokens))
	for _, tok := range tokens {
		if tok == "rename" {
			continue
		}
		out = append(out, tok)
	}
	return out, nil
}

func stripLineComments(s string) string {
	var b strings.Builder
	for line := range strings.SplitSeq(s, "\n") {
		idx := strings.Index(line, ";")
		if idx >= 0 {
			line = line[:idx]
		}
		b.WriteString(line)
		b.WriteByte('\n')
	}
	return b.String()
}
