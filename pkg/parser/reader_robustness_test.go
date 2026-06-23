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
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
)

// readCatching parses the first datum from src, recovering any panic so that a
// reader crash on malformed input registers as a test failure instead of
// aborting the whole test binary. The reader is on the runtime path via
// (read ...), which accepts untrusted input (R7RS §6.13.2), so a malformed
// datum must surface as a located error — never a Go panic.
func readCatching(src string) (recovered any, err error) {
	defer func() {
		recovered = recover()
	}()
	env := environment.NewNamespace().Runtime()
	p := NewParser(env, true, strings.NewReader(src))
	_, err = p.ReadSyntax(context.TODO())
	return recovered, err
}

// TestReader_MalformedInputIsLocatedErrorNotPanic pins the untrusted-read
// robustness contract: each malformed datum must yield a located *ParserError,
// never a Go panic and never a silently mis-parsed value.
//
// Each case here is a confirmed defect on the parent commit:
//   - "#u8(1 2]"  nil-deref SIGSEGV in readByteVector (close bracket read as a
//     nil element, then stx.Unwrap()).
//   - "(( . ))"   SetCdr(nil) panic: an empty dotted pair has no car, and the
//     ')' after the dot is read as a nil cdr.
//   - "((1 . ))"  SetCdr(nil) panic: the cdr datum is missing; ')' is read as a
//     nil cdr that flows into SetCdr.
//   - "( . 5)"    silently mis-parses to (#<void>), discarding the 5: a dotted
//     pair with no car is accepted instead of rejected.
func TestReader_MalformedInputIsLocatedErrorNotPanic(t *testing.T) {
	cases := []struct {
		name string
		src  string
	}{
		{"bytevector closed by bracket", "#u8(1 2]"},
		{"empty dotted pair (no car)", "(( . ))"},
		{"dotted pair missing cdr datum", "((1 . ))"},
		{"dot with no car", "( . 5)"},
		{"labeled list missing cdr datum", "#0=(1 . )"},
		{"labeled list trailing missing cdr", "#0=(1 2 . )"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			recovered, err := readCatching(tc.src)
			if recovered != nil {
				t.Fatalf("reader panicked on %q: %v\n(must return a located error, not crash the host)", tc.src, recovered)
			}
			if err == nil {
				t.Fatalf("reader accepted malformed %q (want a located *ParserError)", tc.src)
			}
			var perr *ParserError
			if !errors.As(err, &perr) {
				t.Fatalf("reader returned %T for %q, want *ParserError: %v", err, tc.src, err)
			}
		})
	}
}
