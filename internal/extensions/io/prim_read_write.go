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

	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/internal/tokenizer"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

const (
	// MaxReadStringBytes is the maximum memory that read-string can allocate
	// for the character buffer (100 MB). Assumes 4 bytes per rune (worst case).
	MaxReadStringBytes = 100 * 1024 * 1024 // 100 MB
)

// extractPort extracts a port of type T from a rest-argument list.
// Returns (port, tuple, true, nil) when a port is found in the list.
// Returns (zero, tuple, false, nil) when the list is empty — the caller
// resolves the default. Returns (zero, nil, false, error) on type mismatch
// or malformed input.
func extractPort[T any](
	o values.Value,
	name string,
	errSentinel *werr.StaticError,
	portDesc string,
) (T, values.Tuple, bool, error) {
	var zero T
	prefix := fmtPrefix(name)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return zero, nil, false, werr.WrapForeignErrorf(
			werr.ErrNotAList, "%sexpected a list but got %T", prefix, o)
	}
	if !tuple.IsList() {
		return zero, nil, false, werr.WrapForeignErrorf(
			werr.ErrNotAList, "%sexpected a list but got %s", prefix, tuple.SchemeString())
	}
	if tuple.IsEmptyList() {
		return zero, tuple, false, nil
	}
	p, ok := tuple.Car().(T)
	if !ok {
		return zero, nil, false, werr.WrapForeignErrorf(
			errSentinel, "%sexpected %s but got %T", prefix, portDesc, tuple.Car())
	}
	return p, tuple, true, nil
}

func fmtPrefix(name string) string {
	if name == "" {
		return ""
	}
	return name + ": "
}

// getOptionalOutputPort extracts an optional output port from a variadic argument list.
// If the list is empty, returns the current output port.
// Otherwise, extracts and validates the port from the list's car.
func getOptionalOutputPort(mc machine.CallContext, argIndex int) (values.OutputPort, error) {
	p, _, found, err := extractPort[values.OutputPort](
		mc.Arg(argIndex), "", werr.ErrNotAnOutputPort, "an output port",
	)
	if err != nil {
		return nil, err
	}
	if !found {
		return resolveCurrentOutputPort(mc), nil
	}
	return p, nil
}

// getOptionalTextualOutputPort extracts an optional textual output port, rejecting
// binary-only output ports. Use for textual operations (write, display, newline, etc.)
// that must not accept binary ports. flush-output-port uses getOptionalOutputPort directly.
func getOptionalTextualOutputPort(mc machine.CallContext, argIndex int) (values.OutputPort, error) {
	p, err := getOptionalOutputPort(mc, argIndex)
	if err != nil {
		return nil, err
	}
	_, isBinary := p.(values.BinaryWriter)
	if isBinary {
		return nil, werr.WrapForeignErrorf(werr.ErrNotATextualPort,
			"expected a textual output port, got binary port")
	}
	return p, nil
}

// getOptionalInputPort extracts an optional input port from a variadic argument list.
// If the list is empty, returns the current input port.
// Otherwise, extracts and validates the port from the list's car.
func getOptionalInputPort(mc machine.CallContext, argIndex int) (values.TextualReader, error) {
	p, _, found, err := extractPort[values.TextualReader](
		mc.Arg(argIndex), "", werr.ErrNotAnInputPort, "an input port",
	)
	if err != nil {
		return nil, err
	}
	if !found {
		return resolveCurrentInputPort(mc), nil
	}
	return p, nil
}

// getRequiredBinaryInputPort extracts a required binary input port from a variadic argument list.
// Returns an error if the list is empty (no default port for binary I/O).
// Also returns the validated tuple for callers that need to extract further arguments.
func getRequiredBinaryInputPort(o values.Value, name string) (values.BinaryReader, values.Tuple, error) {
	p, tuple, found, err := extractPort[values.BinaryReader](
		o, name, werr.ErrNotAByteInputPort, "a binary input port",
	)
	if err != nil {
		return nil, nil, err
	}
	if !found {
		return nil, nil, werr.WrapForeignErrorf(
			werr.ErrNotAByteInputPort, "%s: no binary input port specified", name)
	}
	return p, tuple, nil
}

// getRequiredBinaryOutputPort extracts a required binary output port from a variadic argument list.
// Returns an error if the list is empty (no default port for binary I/O).
// Also returns the validated tuple for callers that need to extract further arguments.
func getRequiredBinaryOutputPort(o values.Value, name string) (values.BinaryWriter, values.Tuple, error) {
	p, tuple, found, err := extractPort[values.BinaryWriter](
		o, name, werr.ErrNotAByteOutputPort, "a binary output port",
	)
	if err != nil {
		return nil, nil, err
	}
	if !found {
		return nil, nil, werr.WrapForeignErrorf(
			werr.ErrNotAByteOutputPort, "%s: no binary output port specified", name)
	}
	return p, tuple, nil
}

// PrimRead implements the (read) primitive.
// Reads a Scheme datum from port.
// Reads from the current input port if no port is specified.
// R7RS §6.13.2: read uses datum labels to handle circular and shared structures.
func PrimRead(mc machine.CallContext) error {
	port, err := getOptionalInputPort(mc, 0)
	if err != nil {
		return err
	}

	// Get or create parser under lock to prevent TOCTOU races
	cacheMu.Lock()
	prss, ok := Parsers[port]
	if !ok || prss == nil {
		prss = parser.NewParser(mc.EnvironmentFrame(), true, port)
		Parsers[port] = prss
	}
	cacheMu.Unlock()

	syn, err := prss.ReadSyntax(mc.Context())
	if err != nil {
		if errors.Is(err, io.EOF) {
			// Port is exhausted; evict the cached parser.
			evictPortCache(port)
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
	port, err := getOptionalInputPort(mc, 0)
	if err != nil {
		return err
	}

	// Get or create tokenizer under lock to prevent TOCTOU races
	cacheMu.Lock()
	tknz, ok := Tokenizers[port]
	if !ok || tknz == nil {
		tknz = tokenizer.NewTokenizer(port, false)
		Tokenizers[port] = tknz
	}
	cacheMu.Unlock()

	q, err := tknz.Next()
	if errors.Is(err, io.EOF) {
		// Port is exhausted; evict the cached tokenizer.
		evictPortCache(port)
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
	port, err := getOptionalInputPort(mc, 0)
	if err != nil {
		return err
	}

	// Get or create parser under lock to prevent TOCTOU races
	cacheMu.Lock()
	prss, ok := Parsers[port]
	if !ok || prss == nil {
		prss = parser.NewParser(mc.EnvironmentFrame(), true, port)
		Parsers[port] = prss
	}
	cacheMu.Unlock()

	q, err := prss.ReadSyntax(mc.Context())
	if err != nil {
		if errors.Is(err, io.EOF) {
			// Port is exhausted; evict the cached parser.
			evictPortCache(port)
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
	reader, err := getOptionalInputPort(mc, 0)
	if err != nil {
		return err
	}

	r, _, err := reader.ReadRune()
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
	reader, err := getOptionalInputPort(mc, 0)
	if err != nil {
		return err
	}

	r, _, err := reader.ReadRune()
	if errors.Is(err, io.EOF) {
		mc.SetValue(values.EOFObject)
		return nil
	}
	if err != nil {
		return werr.WrapForeignReadErrorf(err, "peek-char: error reading character")
	}
	// Unread the character so it can be read again
	err = reader.UnreadRune()
	if err != nil {
		return werr.WrapForeignErrorf(err, "peek-char: error unreading character")
	}
	mc.SetValue(values.NewCharacter(r))
	return nil
}

// PrimReadLine implements the read-line primitive.
// R7RS §6.13.2: (read-line [port])
// Reads a line of text from the input port, not including the line ending.
func PrimReadLine(mc machine.CallContext) error {
	reader, err := getOptionalInputPort(mc, 0)
	if err != nil {
		return err
	}

	var line []rune
	for {
		r, _, err := reader.ReadRune()
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
		// Handle \r\n by discarding \r if followed by \n
		if r == '\r' {
			nextR, _, err := reader.ReadRune()
			if err == nil && nextR != '\n' {
				reader.UnreadRune() //nolint:errcheck
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
	k, err := helpers.RequireArg[*values.Integer](mc, 0, werr.ErrNotANumber, "read-string")
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

	reader, err := getOptionalInputPort(mc, 1)
	if err != nil {
		return err
	}

	// Read up to k characters
	chars := make([]rune, 0, k.Value)
	for i := int64(0); i < k.Value; i++ {
		r, _, err := reader.ReadRune()
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
