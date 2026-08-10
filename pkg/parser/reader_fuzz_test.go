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

package parser

import (
	"context"
	"errors"
	"io"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
)

// fuzzSeeds is the seed corpus shared by the parser fuzz targets: a mix of
// well-formed datums (one per datum class the reader accepts) and malformed
// inputs — the confirmed crashers from reader_robustness_test.go plus other
// pathological shapes. Under plain `go test` these run as a deterministic
// corpus (so they double as regression cases); `go test -fuzz` mutates from
// here to hunt for inputs nobody thought to write down.
var fuzzSeeds = []string{
	// Well-formed, spanning the datum classes.
	"", " ", "42", "-7", "3.14", "1/2", "#xff", "1e3", "+inf.0", "+i", "2+3i",
	"#t", "#f", "#\\a", "#\\space", "#\\x41", `"hello"`, `"a\tb"`, "foo", "|odd sym|",
	"()", "(1 2 3)", "(1 . 2)", "((1 2) 3)", "#(1 2 3)", "#()", "#u8(0 255)",
	"'(a b)", "`(a ,b ,@c)", "#;9 7", "(a #;b c)", "#!fold-case 1",
	"#0=(1 . #0#)", "#0=#(1 #0#)", "(#0=(1 2) #0#)",
	// The # forms added 2026-07-31: boxes (a container introducer, cycles
	// included), the widening introducers, radix fractions, and the l precision
	// marker. Phase 5 made boxes round-trippable for the first time and Phase 4
	// changed what the writer emits for a whole type, so these are exactly the
	// inputs this target exists to police.
	"#&5", "#&#&5", "#&(1 2)", "#&#x1f", "#0=#&#0#", "(#0=#&1 #0#)",
	"#z#x1f", "#z#e#x1f", "#m#b101.101", "#z 5",
	"#x1.8", "#b101.101", "#x.f", "#b-.1", "1.5l0", "1l1000", "#m1.5",
	// Malformed members of the same families.
	"#&", "(#&)", "#b19", "#o789", "#x1.8p3", "#z1.5", "#x#z1f", "#z#m5",
	// Malformed — must surface a located *ParserError, never a panic or a host crash.
	"(foo", "#u8(1 2]", "(( . ))", "((1 . ))", "( . 5)", "#0=(1 . )", "#0=(1 2 . )",
	"#u8(1 2 3", "#0=#1#", strings.Repeat("(", 12000),
}

// fuzzSkipModes is the skipComments axis both targets sweep. The reader has two
// behaviours, not one: with skipComments false, comments, datum comments and
// directives are returned as data and the readDatumComment / readComment /
// readDirective branches are live. Running only the skipping mode left
// readDatumComment unreachable from the fuzzer despite "#;" sitting in the seed
// corpus — which is why "#;)" (a nil-Value node that panics on SchemeString)
// survived to be found by hand.
var fuzzSkipModes = [...]bool{true, false}

// readFuzzDatum reads a single datum from src with a fresh parser. Panics
// propagate to the calling fuzz body's recover guard.
func readFuzzDatum(src string, skipComments bool) (syntax.SyntaxValue, error) {
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, skipComments, strings.NewReader(src))
	return p.ReadSyntax(context.TODO())
}

// FuzzReadSyntax pins the reader's untrusted-input contract (R7RS §6.13.2, the
// #779/#780 hardening): for ANY input the reader must terminate without
// panicking or overflowing the host, consuming each top-level datum until a
// clean io.EOF or a single located *ParserError. The example-based
// reader_robustness_test.go enumerates the crashers someone anticipated; this
// generates the ones they did not.
func FuzzReadSyntax(f *testing.F) {
	for _, s := range fuzzSeeds {
		f.Add(s)
	}
	// Harvested from the parser/tokenizer example-test tables (fuzz_corpus_test.go):
	// every input some hand-written test exercises becomes a seed the mutator
	// explores around. FuzzReadWriteRoundTrip deliberately does NOT seed from these
	// (some numeric forms are not yet round-trip-clean — issue #781).
	for _, s := range corpusSeeds {
		f.Add(s)
	}
	f.Fuzz(func(t *testing.T, src string) {
		defer func() {
			r := recover()
			if r != nil {
				t.Fatalf("reader panicked on %q: %v\n(must surface a located error, never crash the host)", src, r)
			}
		}()
		for _, skipComments := range fuzzSkipModes {
			readAllFuzzDatums(t, src, skipComments)
		}
	})
}

// readAllFuzzDatums drains src through one parser and asserts the reader's
// untrusted-input contract on every datum: a value, io.EOF, or a located
// *ParserError, and termination.
func readAllFuzzDatums(t *testing.T, src string, skipComments bool) {
	t.Helper()
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, skipComments, strings.NewReader(src))
	// A clean end is io.EOF; any other error must be a located *ParserError.
	// The parser cannot be reused after an error, so stop on the first one.
	// Cap iterations as a non-advancement safety valve: a real input yields
	// at most len(src) datums, so reaching the cap signals a stuck reader.
	maxData := len(src) + 16
	for range maxData {
		v, err := p.ReadSyntax(context.TODO())
		if errors.Is(err, io.EOF) {
			return
		}
		if err != nil {
			var perr *ParserError
			if !errors.As(err, &perr) {
				t.Fatalf("reader returned %T for %q (skipComments=%v); want *ParserError or io.EOF: %v", err, src, skipComments, err)
			}
			return
		}
		if v == nil {
			t.Fatalf("ReadSyntax returned (nil, nil) for %q (skipComments=%v) — violates the value | io.EOF | *ParserError contract", src, skipComments)
		}
	}
	// A real input yields at most len(src) datums, so exhausting the cap
	// without an EOF/error/nil exit means the reader advanced past every
	// byte yet never terminated — a stuck reader, which is a failure, not a
	// silently-tolerated cap.
	t.Fatalf("reader did not terminate after %d reads on %q (skipComments=%v, possible non-advancement)", maxData, src, skipComments)
}

// FuzzReadWriteRoundTrip closes the "valid on read" loop with the writer: any
// value the reader produces must, when written, yield output the reader
// accepts again. This is exactly the invariant the writer depth bound exists to
// keep total — a value the writer can render is one the reader can re-read.
//
// Equality (read = write∘read) is intentionally NOT asserted: float, quote-form,
// and other representation round-trips are a stronger, separate property whose
// failures would be representation gaps rather than the re-readability defect
// this target hunts for. A depth-bounded value may legitimately refuse to write
// (ErrWriteDepthExceeded); that is not a re-readability failure.
//
// KNOWN GAP: the numeric tower's external representations are not yet fully
// round-trip-clean; tracked in issue #781. Both examples this comment used to
// give are closed as of 2026-07-31 — a BigFloat now writes the `l` precision
// marker, so #m1.5 renders as "1.5l0" and reads back as a BigFloat, and 1e+700
// reads and round-trips. Running under -fuzz surfaces whatever remains.
//
// It also surfaces one hazard that is not a round-trip failure: rendering a
// BigFloat whose exponent is around 10^7 spends ~11s inside big.Float.Text and
// trips the fuzzer's per-input deadline, so a -fuzz run stops there rather than
// exploring further. Pre-existing and equally reproducible on master — see
// TODO.md, "BigFloat rendering hangs on a huge exponent".
func FuzzReadWriteRoundTrip(f *testing.F) {
	for _, s := range fuzzSeeds {
		f.Add(s)
	}
	f.Fuzz(func(t *testing.T, src string) {
		defer func() {
			r := recover()
			if r != nil {
				t.Fatalf("read/write/read panicked on %q: %v", src, r)
			}
		}()
		for _, skipComments := range fuzzSkipModes {
			roundTripFuzzDatum(t, src, skipComments)
		}
	})
}

// roundTripFuzzDatum reads one datum from src, writes it, and reads the output
// back, in one skipComments mode. Both modes are swept: the preserving mode
// yields comment and directive nodes the skipping mode never produces, and
// those must be writable and re-readable like any other value.
func roundTripFuzzDatum(t *testing.T, src string, skipComments bool) {
	t.Helper()
	v, err := readFuzzDatum(src, skipComments)
	if err != nil {
		return // malformed or empty input (incl. io.EOF) is the other target's concern
	}
	if v == nil {
		t.Fatalf("readFuzzDatum returned (nil, nil) for %q (skipComments=%v) — violates the reader contract", src, skipComments)
	}
	out, writeErr := values.WriteValueToString(v.UnwrapAll())
	if writeErr != nil {
		return // depth-bounded refusal, not a re-readability failure
	}
	_, rereadErr := readFuzzDatum(out, skipComments)
	if rereadErr != nil {
		t.Fatalf("writer emitted non-re-readable output (skipComments=%v):\n  input:   %q\n  written: %q\n  reread:  %v", skipComments, src, out, rereadErr)
	}
}

// TestFuzzCorpusNonTrivial guards the harvested seed corpus against silently
// shrinking — a regen that parsed nothing (or a refactor that renamed the
// input/in/src/text test fields) would gut the fuzzer's seed diversity without
// failing any other test. The harvest yields ~900 distinct inputs today; the
// floor leaves headroom while still catching a collapse.
func TestFuzzCorpusNonTrivial(t *testing.T) {
	const minSeeds = 500
	if len(corpusSeeds) < minSeeds {
		t.Fatalf("corpusSeeds has %d entries, want >= %d — did the harvest break (renamed input fields)?", len(corpusSeeds), minSeeds)
	}
}
