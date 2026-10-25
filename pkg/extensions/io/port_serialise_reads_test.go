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

package io_test

import (
	"context"
	"strconv"
	"strings"
	"testing"

	"github.com/aalpar/wile/extensions/threads"
	extio "github.com/aalpar/wile/pkg/extensions/io"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// serialiseRounds is how many times the alphabet repeats in the shared port.
// -race reports only races that happen in the run, so the work must be large
// enough that two unserialised readers overlap on master.
const serialiseRounds = 80

// serialiseSource is the shared port's text: one letter per line. Every token
// is one character, so every interleaving of whole primitive calls is a legal
// reading of it, and each letter is consumed by exactly one call.
func serialiseSource() string {
	var b strings.Builder
	for range serialiseRounds {
		for c := 'a'; c <= 'z'; c++ {
			b.WriteRune(c)
			b.WriteByte('\n')
		}
	}
	return b.String()
}

// TestTwoThreadsReadingOnePortSerialise pins A17: each read primitive takes
// the port's read lock for the whole call, so two SRFI-18 threads reading one
// textual port neither race on the stream and its position counter (-race) nor
// tear it. Each row pairs two step procedures over the shared port p; a step
// returns what it consumed as a string, or the eof object. The two invariants
// hold under any interleaving of whole calls: every item is a substring of the
// source (a torn read yields "ab", which is not), and every letter of the
// source is consumed exactly once across both threads (a racing peek-char
// unreads the other thread's rune, which is then read twice).
func TestTwoThreadsReadingOnePortSerialise(t *testing.T) {
	const (
		readChar   = `(lambda () (let ((c (read-char p))) (if (eof-object? c) c (string c))))`
		peekChar   = `(lambda () (let ((c (peek-char p))) (if (eof-object? c) c "")))`
		readDatum  = `(lambda () (let ((d (read p))) (if (eof-object? d) d (symbol->string d))))`
		readSyntax = `(lambda () (let ((d (read-syntax p))) (if (eof-object? d) d (symbol->string (syntax->datum d)))))`
		readLine   = `(lambda () (read-line p))`
		readString = `(lambda () (read-string 3 p))`
	)
	tests := []struct {
		name         string
		first, other string
	}{
		{"read-char and read-char", readChar, readChar},
		{"peek-char and read-char", peekChar, readChar},
		{"read-line and read-char", readLine, readChar},
		{"read-string and read-char", readString, readChar},
		{"read and read-char", readDatum, readChar},
		{"read-syntax and read-char", readSyntax, readChar},
		{"read and read-syntax", readDatum, readSyntax},
	}
	src := serialiseSource()
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(),
				wile.WithExtension(extio.Extension),
				wile.WithExtension(threads.Extension),
			)
			c.Assert(err, qt.IsNil)
			t.Cleanup(func() {
				_ = engine.Close()
			})
			code := `
(define p (open-input-string "` + strings.ReplaceAll(src, "\n", `\n`) + `"))
(define (drain step)
  (lambda ()
    (let loop ((acc '()))
      (let ((x (step)))
        (if (eof-object? x) acc (loop (cons x acc)))))))
(define t1 (make-thread (drain ` + tt.first + `)))
(define t2 (make-thread (drain ` + tt.other + `)))
(thread-start! t1)
(thread-start! t2)
(append (thread-join! t1) (thread-join! t2))`
			result, err := engine.EvalMultiple(context.Background(), code)
			c.Assert(err, qt.IsNil)
			items, ok := result.Internal().(*values.Pair)
			c.Assert(ok, qt.IsTrue, qt.Commentf("result %s", result.SchemeString()))
			counts := map[rune]int{}
			_, err = items.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v values.Value) error {
				item := v.(*values.String).Value
				c.Assert(strings.Contains(src, item), qt.IsTrue, qt.Commentf("torn read: %q", item))
				for _, r := range item {
					if r != '\n' {
						counts[r]++
					}
				}
				return nil
			})
			c.Assert(err, qt.IsNil)
			for r := 'a'; r <= 'z'; r++ {
				c.Assert(counts[r], qt.Equals, serialiseRounds, qt.Commentf("letter %q", r))
			}
			c.Assert(len(counts), qt.Equals, 26)
		})
	}
}

// TestCloseRacingARead pins that close-port, which takes no read lock (a close
// parked behind a read blocked on a pipe would never run), does not race the
// reader on the port's closed flag (-race). The reader stops on the closed
// port's error or at end of input, whichever comes first; how far it got is
// not asserted.
func TestCloseRacingARead(t *testing.T) {
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(threads.Extension),
	)
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		_ = engine.Close()
	})
	code := `
(define p (open-input-string (make-string 20000 #\a)))
(define t1 (make-thread
  (lambda ()
    (guard (e (#t 'closed))
      (let loop ()
        (if (eof-object? (read-char p)) 'eof (loop)))))))
(thread-start! t1)
(close-port p)
(memq (thread-join! t1) '(closed eof))`
	result, err := engine.EvalMultiple(context.Background(), code)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Not(qt.Equals), "#f")
}

// TestTwoThreadsReadingOneBinaryPortSerialise is the binary half of A17: each
// binary read primitive holds the port's read lock for its call, so two
// threads draining one bytevector port neither race (-race) nor read a byte
// twice or skip one. The source counts 0..255 repeatedly, so a step's result
// is checked byte for byte: every byte value occurs exactly
// serialiseRounds times across both threads.
func TestTwoThreadsReadingOneBinaryPortSerialise(t *testing.T) {
	const (
		readU8     = `(lambda () (let ((b (read-u8 p))) (if (eof-object? b) b (list b))))`
		peekU8     = `(lambda () (let ((b (peek-u8 p))) (if (eof-object? b) b '())))`
		readBV     = `(lambda () (let ((v (read-bytevector 3 p))) (if (eof-object? v) v (bv->list v (bytevector-length v)))))`
		readBVBang = `(lambda () (let* ((v (make-bytevector 3 0)) (n (read-bytevector! v p))) (if (eof-object? n) n (bv->list v n))))`
	)
	tests := []struct {
		name         string
		first, other string
	}{
		{"read-u8 and read-u8", readU8, readU8},
		{"peek-u8 and read-u8", peekU8, readU8},
		{"read-bytevector and read-u8", readBV, readU8},
		{"read-bytevector! and read-u8", readBVBang, readU8},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			engine, err := wile.NewEngine(context.Background(),
				wile.WithExtension(extio.Extension),
				wile.WithExtension(threads.Extension),
			)
			c.Assert(err, qt.IsNil)
			t.Cleanup(func() {
				_ = engine.Close()
			})
			code := `
(define src (let loop ((i (- (* 256 ` + strconv.Itoa(serialiseRounds) + `) 1)) (acc '()))
  (if (< i 0) acc (loop (- i 1) (cons (modulo i 256) acc)))))
(define p (open-input-bytevector (apply bytevector src)))
(define (bv->list v n)
  (let loop ((i (- n 1)) (acc '()))
    (if (< i 0) acc (loop (- i 1) (cons (bytevector-u8-ref v i) acc)))))
(define (drain step)
  (lambda ()
    (let loop ((acc '()))
      (let ((x (step)))
        (if (eof-object? x) acc (loop (append x acc)))))))
(define t1 (make-thread (drain ` + tt.first + `)))
(define t2 (make-thread (drain ` + tt.other + `)))
(thread-start! t1)
(thread-start! t2)
(let ((counts (make-vector 256 0)))
  (for-each (lambda (b) (vector-set! counts b (+ 1 (vector-ref counts b))))
            (append (thread-join! t1) (thread-join! t2)))
  (vector->list counts))`
			result, err := engine.EvalMultiple(context.Background(), code)
			c.Assert(err, qt.IsNil)
			want := strings.TrimSuffix(strings.Repeat(strconv.Itoa(serialiseRounds)+" ", 256), " ")
			c.Assert(result.SchemeString(), qt.Equals, "("+want+")")
		})
	}
}
