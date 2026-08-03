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

// Package coverage collects per-s-expression coverage data from
// executing Wile Scheme code. Each coverage entry corresponds to a
// unique SourceContext (file + start/end line/column), aggregated
// across all compiled NativeTemplates.
//
// Known limitations:
//   - Instructions synthesized by the peephole optimizer may drop
//     their source attribution (see machine/peephole_test.go:540).
//     Such PCs execute but produce no Entry.
//   - Instructions with no source context (synthetic infrastructure
//     ops) are skipped.
package coverage

import (
	"cmp"
	"slices"
	"sync"

	"github.com/aalpar/wile/pkg/machine"
)

// Entry is one covered (or not covered) s-expression.
type Entry struct {
	File      string
	StartLine int
	StartCol  int
	EndLine   int
	EndCol    int
	// Count is 0 if the s-expression did not execute, 1 if it did
	// (mode=set). Higher modes are a future extension.
	Count int
}

// Collector aggregates coverage data across a set of tracked templates.
// A zero Collector is not usable; construct with NewCollector.
type Collector struct {
	mu        sync.Mutex
	templates []*machine.NativeTemplate
}

// NewCollector returns a ready-to-use Collector.
func NewCollector() *Collector {
	q := &Collector{}
	return q
}

// Track enables coverage on a template and adds it to the collector's
// tracked set. Calling Track multiple times with the same template is
// a no-op (templates are deduplicated by pointer identity).
func (p *Collector) Track(tpl *machine.NativeTemplate) {
	if tpl == nil {
		return
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	if slices.Contains(p.templates, tpl) {
		return
	}
	tpl.EnableCoverage()
	p.templates = append(p.templates, tpl)
}

// Entries returns one Entry per unique SourceContext seen across all
// tracked templates. Entries are sorted lexicographically by
// (File, StartLine, StartCol, EndLine, EndCol). An s-expression is
// reported with Count=1 iff any instruction referring to its
// SourceContext executed (mode=set).
func (p *Collector) Entries() []Entry {
	p.mu.Lock()
	defer p.mu.Unlock()

	type key struct {
		file     string
		startLn  int
		startCol int
		endLn    int
		endCol   int
	}
	// hit[key] is true iff any PC referring to this source was executed.
	hit := make(map[key]bool)

	for _, tpl := range p.templates {
		exec := tpl.Executed()
		if exec == nil {
			continue
		}
		for pc := range len(exec) {
			src := tpl.SourceAt(pc)
			if src == nil || src.File == "" {
				continue
			}
			k := key{
				file:     src.File,
				startLn:  src.Start.Line(),
				startCol: src.Start.Column(),
				endLn:    src.End.Line(),
				endCol:   src.End.Column(),
			}
			prev := hit[k]
			hit[k] = prev || exec[pc]
		}
	}

	q := make([]Entry, 0, len(hit))
	for k, covered := range hit {
		entry := Entry{
			File:      k.file,
			StartLine: k.startLn,
			StartCol:  k.startCol,
			EndLine:   k.endLn,
			EndCol:    k.endCol,
		}
		if covered {
			entry.Count = 1
		}
		q = append(q, entry)
	}

	slices.SortFunc(q, compareEntry)
	return q
}

// compareEntry defines the canonical sort order for Entries: by file, then
// by start position, then by end position.
func compareEntry(a, b Entry) int {
	return cmp.Or(
		cmp.Compare(a.File, b.File),
		cmp.Compare(a.StartLine, b.StartLine),
		cmp.Compare(a.StartCol, b.StartCol),
		cmp.Compare(a.EndLine, b.EndLine),
		cmp.Compare(a.EndCol, b.EndCol),
	)
}
