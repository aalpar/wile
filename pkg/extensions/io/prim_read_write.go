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

package io

import (
	"errors"
	"io"

	"github.com/aalpar/wile/pkg/internal/tokenizer"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

const (
	// MaxReadStringBytes is the maximum memory that read-string can allocate
	// for the character buffer (100 MB). Assumes 4 bytes per rune (worst case).
	MaxReadStringBytes = 100 * 1024 * 1024 // 100 MB
)

// extractPort extracts a *PortObject from a rest-argument list.
// Returns (port, tuple, true, nil) when a port is found.
// Returns (nil, tuple, false, nil) when the list is empty — the caller
// resolves the default. Returns (nil, nil, false, error) on type
// mismatch or malformed input. The expected-type phrase is read from
// the sentinel via errSentinel.TypeName().
//
// errSentinel MUST be constructed via werr.NewTypeSentinel — otherwise
// TypeName() returns "" and the type mismatch message degrades to
// "expected  but got *Foo".
//
// This helper only validates the value is a *PortObject; per-caller
// capability checks (input/output/textual/binary) happen at the call
// site via AsReader/AsWriter/AsRuneReader/AsByteReader accessors.
func extractPort(
	o values.Value,
	name string,
	errSentinel *werr.StaticError,
) (*values.PortObject, values.Tuple, bool, error) {
	prefix := fmtPrefix(name)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return nil, nil, false, werr.WrapForeignErrorf(
			werr.ErrNotAList, "%sexpected a list but got %T", prefix, o)
	}
	if !tuple.IsList() {
		return nil, nil, false, werr.WrapForeignErrorf(
			werr.ErrNotAList, "%sexpected a list but got %s", prefix, tuple.SchemeString())
	}
	if tuple.IsEmptyList() {
		return nil, tuple, false, nil
	}
	p, ok := tuple.Car().(*values.PortObject)
	if !ok {
		return nil, nil, false, werr.WrapForeignErrorf(
			errSentinel, "%sexpected %s but got %T", prefix, errSentinel.TypeName(), tuple.Car())
	}
	return p, tuple, true, nil
}

func fmtPrefix(name string) string {
	if name == "" {
		return ""
	}
	return name + ": "
}

// getOptionalOutputPort extracts an optional output port from a
// variadic argument list. If the list is empty, returns the current
// output port. Otherwise, extracts and validates the port from the
// list's car, plus asserts the port has writer capability.
func getOptionalOutputPort(mc machine.CallContext, argIndex int) (*values.PortObject, error) {
	p, _, found, err := extractPort(
		mc.Arg(argIndex), "", werr.ErrNotAnOutputPort)
	if err != nil {
		return nil, err
	}
	if !found {
		return resolveCurrentOutputPort(mc), nil
	}
	_, ok := p.AsWriter()
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAnOutputPort,
			"expected an output port, got %s", p.PortKind())
	}
	return p, nil
}

// requireTextualOutput rejects ports that carry a byte-writer slot
// (i.e., binary output ports). It is the single source of truth for
// the "this output port must be textual" predicate — shared by
// getOptionalTextualOutputPort and by PrimWriteString, which resolves
// the port itself in order to parse the rest-tuple for start/end.
func requireTextualOutput(p *values.PortObject) error {
	_, isBinary := p.AsByteWriter()
	if !isBinary {
		return nil
	}
	return werr.WrapForeignErrorf(werr.ErrNotATextualPort,
		"expected a textual output port, got %s", p.PortKind())
}

// getOptionalTextualOutputPort extracts an optional textual output port,
// rejecting binary output ports. Used for textual operations (write,
// display, newline, etc.) that must not accept binary ports.
// flush-output-port uses getOptionalOutputPort directly.
func getOptionalTextualOutputPort(mc machine.CallContext, argIndex int) (*values.PortObject, error) {
	p, err := getOptionalOutputPort(mc, argIndex)
	if err != nil {
		return nil, err
	}
	err = requireTextualOutput(p)
	if err != nil {
		return nil, err
	}
	return p, nil
}

// getOptionalInputPort extracts an optional textual input port from a
// variadic argument list. If the list is empty, returns the current
// input port. All current callers (read, read-char, peek-char,
// read-line, read-string) require textual input, so the helper
// validates rune-read capability and returns both the port (for cache
// keying) and the resolved io.RuneReader (for the I/O call).
//
// runeReader is also a values.RuneUnreader (set on every textual
// port — see port_constructors.go), so peek-char's UnreadRune call
// goes via port.AsRuneUnreader() at the call site.
func getOptionalInputPort(mc machine.CallContext, argIndex int) (*values.PortObject, io.RuneReader, error) {
	p, _, found, err := extractPort(
		mc.Arg(argIndex), "", werr.ErrNotAnInputPort)
	if err != nil {
		return nil, nil, err
	}
	if !found {
		port := resolveCurrentInputPort(mc)
		rr, _ := port.AsRuneReader()
		return port, rr, nil
	}
	rr, hasRR := p.AsRuneReader()
	if !hasRR {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotATextualPort,
			"expected a textual input port, got %s", p.PortKind())
	}
	return p, rr, nil
}

// getRequiredBinaryInputPort extracts a required binary input port
// from a variadic argument list. Returns an error if the list is empty
// (no default port for binary I/O) or if the port lacks byte-read
// capability.
func getRequiredBinaryInputPort(o values.Value, name string) (*values.PortObject, values.Tuple, error) {
	p, tuple, found, err := extractPort(o, name, werr.ErrNotAByteInputPort)
	if err != nil {
		return nil, nil, err
	}
	if !found {
		return nil, nil, werr.WrapForeignErrorf(
			werr.ErrNotAByteInputPort, "%s: no binary input port specified", name)
	}
	_, ok := p.AsByteReader()
	if !ok {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAByteInputPort,
			"%s: port is not binary", name)
	}
	return p, tuple, nil
}

// getRequiredBinaryOutputPort extracts a required binary output port
// from a variadic argument list. Returns an error if the list is empty
// or if the port lacks byte-write capability.
func getRequiredBinaryOutputPort(o values.Value, name string) (*values.PortObject, values.Tuple, error) {
	p, tuple, found, err := extractPort(o, name, werr.ErrNotAByteOutputPort)
	if err != nil {
		return nil, nil, err
	}
	if !found {
		return nil, nil, werr.WrapForeignErrorf(
			werr.ErrNotAByteOutputPort, "%s: no binary output port specified", name)
	}
	_, ok := p.AsByteWriter()
	if !ok {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAByteOutputPort,
			"%s: port is not binary", name)
	}
	return p, tuple, nil
}

// PrimRead implements the (read) primitive.
// Reads a Scheme datum from port.
// Reads from the current input port if no port is specified.
// R7RS §6.13.2: read uses datum labels to handle circular and shared structures.
func PrimRead(mc machine.CallContext) error {
	port, rr, err := getOptionalInputPort(mc, 0)
	if err != nil {
		return err
	}
	st, err := stateFrom(mc)
	if err != nil {
		return err
	}

	mkParser := func() *parser.Parser {
		return parser.NewParser(mc.EnvironmentFrame(), true, rr)
	}
	syn, err := st.readSyntaxCached(mc.Context(), port, mkParser)
	if err != nil {
		if errors.Is(err, io.EOF) {
			// Port is exhausted; evict the cached parser.
			evictPortCache(st, port)
			mc.SetValue(values.EOFObject)
			return nil
		}
		return werr.WrapForeignReadErrorf(err, "error reading from input port")
	}
	// Use UnwrapAllShared to preserve object identity for datum labels (R7RS §2.4)
	// and handle circular structures
	cache := make(map[syntax.SyntaxValue]values.Value)
	q := syntax.UnwrapAllShared(syn, cache)
	mc.SetValue(q)
	return nil
}

// PrimReadToken implements the (read-token) primitive.
// Reads a single token from port.
// Reads from the current input port if no port is specified.
func PrimReadToken(mc machine.CallContext) error {
	port, rr, err := getOptionalInputPort(mc, 0)
	if err != nil {
		return err
	}
	st, err := stateFrom(mc)
	if err != nil {
		return err
	}

	mkTokenizer := func() *tokenizer.Tokenizer {
		return tokenizer.NewTokenizer(rr, false)
	}
	q, err := st.nextTokenCached(port, mkTokenizer)
	if errors.Is(err, io.EOF) {
		// Port is exhausted; evict the cached tokenizer.
		evictPortCache(st, port)
		mc.SetValue(values.EOFObject)
		return nil
	}
	if err != nil {
		return werr.WrapForeignReadErrorf(err, "error reading token")
	}
	mc.SetValue(q.(values.Value))
	return nil
}

// PrimReadSyntax implements the (read-syntax) primitive.
// Reads datum with source information.
// Reads from the current input port if no port is specified.
func PrimReadSyntax(mc machine.CallContext) error {
	port, rr, err := getOptionalInputPort(mc, 0)
	if err != nil {
		return err
	}
	st, err := stateFrom(mc)
	if err != nil {
		return err
	}

	mkParser := func() *parser.Parser {
		return parser.NewParser(mc.EnvironmentFrame(), true, rr)
	}
	q, err := st.readSyntaxCached(mc.Context(), port, mkParser)
	if err != nil {
		if errors.Is(err, io.EOF) {
			// Port is exhausted; evict the cached parser.
			evictPortCache(st, port)
			mc.SetValue(values.EOFObject)
			return nil
		}
		return werr.WrapForeignReadErrorf(err, "error reading syntax from input port")
	}
	mc.SetValue(q)
	return nil
}

// PrimReadChar implements the read-char primitive.
// R7RS §6.13.2: (read-char [port])
// Reads and returns a single character from the input port.
func PrimReadChar(mc machine.CallContext) error {
	_, rr, err := getOptionalInputPort(mc, 0)
	if err != nil {
		return err
	}

	r, _, err := rr.ReadRune()
	if errors.Is(err, io.EOF) {
		mc.SetValue(values.EOFObject)
		return nil
	}
	if err != nil {
		return werr.WrapForeignReadErrorf(err, "read-char: error reading character")
	}
	mc.SetValue(values.NewCharacter(r))
	return nil
}

// PrimPeekChar implements the peek-char primitive.
// R7RS §6.13.2: (peek-char [port])
// Reads and returns a single character from the input port without consuming it.
func PrimPeekChar(mc machine.CallContext) error {
	port, rr, err := getOptionalInputPort(mc, 0)
	if err != nil {
		return err
	}

	r, _, err := rr.ReadRune()
	if errors.Is(err, io.EOF) {
		mc.SetValue(values.EOFObject)
		return nil
	}
	if err != nil {
		return werr.WrapForeignReadErrorf(err, "peek-char: error reading character")
	}
	// Unread the character so it can be read again. Every textual
	// input port carries a RuneUnreader slot — port_constructors.go
	// pairs the rdr/rr/urr slots together.
	urr, _ := port.AsRuneUnreader()
	err = urr.UnreadRune()
	if err != nil {
		return werr.WrapForeignReadErrorf(err, "peek-char: error unreading character")
	}
	mc.SetValue(values.NewCharacter(r))
	return nil
}

// PrimReadLine implements the read-line primitive.
// R7RS §6.13.2: (read-line [port])
// Reads a line of text from the input port, not including the line ending.
func PrimReadLine(mc machine.CallContext) error {
	port, rr, err := getOptionalInputPort(mc, 0)
	if err != nil {
		return err
	}
	urr, _ := port.AsRuneUnreader()

	var line []rune
	for {
		r, _, err := rr.ReadRune()
		if errors.Is(err, io.EOF) {
			if len(line) == 0 {
				mc.SetValue(values.EOFObject)
				return nil
			}
			break
		}
		if err != nil {
			return werr.WrapForeignReadErrorf(err, "read-line: error reading line")
		}
		if r == '\n' {
			break
		}
		// Handle \r\n by discarding \r if followed by \n.
		// Treat io.EOF after \r as line-ends-with-bare-\r at EOF; propagate
		// any other read error (the unconsumed character is lost otherwise).
		// If the lookahead char is non-\n, push it back; an UnreadRune
		// failure corrupts the read state and must be surfaced as a
		// read-error per R7RS §6.11.
		if r == '\r' {
			nextR, _, err := rr.ReadRune()
			if err != nil && !errors.Is(err, io.EOF) {
				return werr.WrapForeignReadErrorf(err, "read-line: error reading after carriage return")
			}
			if err == nil && nextR != '\n' {
				err = urr.UnreadRune()
				if err != nil {
					return werr.WrapForeignReadErrorf(err, "read-line: error unreading character after carriage return")
				}
			}
			break
		}
		line = append(line, r)
	}

	mc.SetValue(values.NewMutableString(string(line)))
	return nil
}

// PrimCharReadyQ implements the char-ready? primitive.
// R7RS §6.13.2: (char-ready? [port])
// Returns #t if a character is ready on the input port, #f otherwise.
func PrimCharReadyQ(mc machine.CallContext) error {
	// For now, we assume a character is always ready for string input ports
	// and the character input port (stdin may block, but we can't easily check)
	// A more accurate implementation would need non-blocking I/O
	mc.SetValue(values.TrueValue)
	return nil
}

// PrimReadString implements the read-string primitive.
// R7RS §6.13.2: (read-string k [port])
// Reads up to k characters from the input port and returns them as a string.
func PrimReadString(mc machine.CallContext) error {
	k, err := helpers.RequireArg[*values.Integer](mc, 0, werr.ErrNotAnInteger, "read-string")
	if err != nil {
		return err
	}
	if k.Value < 0 {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "read-string: k must be non-negative")
	}
	// R7RS §6.13.2: (read-string 0 port) returns "", not eof-object.
	if k.Value == 0 {
		mc.SetValue(values.NewString(""))
		return nil
	}

	// Check allocation limit (assume 4 bytes per rune worst case)
	const bytesPerRune = 4
	if k.Value > 0 && k.Value*bytesPerRune > MaxReadStringBytes {
		return werr.WrapForeignErrorf(werr.ErrAllocationLimitExceeded,
			"read-string: requested allocation (%d characters, ~%d MB) exceeds maximum (%d MB)",
			k.Value,
			(k.Value*bytesPerRune)/(1024*1024),
			MaxReadStringBytes/(1024*1024),
		)
	}

	_, rr, err := getOptionalInputPort(mc, 1)
	if err != nil {
		return err
	}

	// Read up to k characters
	chars := make([]rune, 0, k.Value)
	for i := int64(0); i < k.Value; i++ {
		r, _, err := rr.ReadRune()
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			return werr.WrapForeignReadErrorf(err, "read-string: error reading string")
		}
		chars = append(chars, r)
	}

	if len(chars) == 0 {
		mc.SetValue(values.EOFObject)
		return nil
	}

	mc.SetValue(values.NewMutableString(string(chars)))
	return nil
}
