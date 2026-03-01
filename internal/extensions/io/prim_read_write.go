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
	"unicode/utf8"

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

	// MaxReadBytevectorBytes is the maximum size of bytevector that
	// read-bytevector can allocate (100 MB).
	MaxReadBytevectorBytes = 100 * 1024 * 1024 // 100 MB
)

// getOptionalOutputPort extracts an optional output port from a variadic argument list.
// If the list is empty, returns the current output port.
// Otherwise, extracts and validates the port from the list's car.
func getOptionalOutputPort(mc *machine.MachineContext, argIndex int) (values.OutputPort, error) {
	o := mc.Arg(argIndex)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList, "expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList, "expected a list but got %s", tuple.SchemeString())
	}
	if tuple.IsEmptyList() {
		return GetCurrentOutputPort(), nil
	}
	p, ok := tuple.Car().(values.OutputPort)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAnOutputPort, "expected an output port but got %T", tuple.Car())
	}
	return p, nil
}

// getOptionalTextualOutputPort extracts an optional textual output port, rejecting
// binary-only output ports. Use for textual operations (write, display, newline, etc.)
// that must not accept binary ports. flush-output-port uses getOptionalOutputPort directly.
func getOptionalTextualOutputPort(mc *machine.MachineContext, argIndex int) (values.OutputPort, error) {
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
func getOptionalInputPort(mc *machine.MachineContext, argIndex int) (values.TextualReader, error) {
	o := mc.Arg(argIndex)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList, "expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAList, "expected a list but got %s", tuple.SchemeString())
	}
	if tuple.IsEmptyList() {
		return GetCurrentInputPort(), nil
	}
	p, ok := tuple.Car().(values.TextualReader)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAnInputPort, "expected an input port but got %T", tuple.Car())
	}
	return p, nil
}

// getRequiredBinaryInputPort extracts a required binary input port from a variadic argument list.
// Returns an error if the list is empty (no default port for binary I/O).
// Also returns the validated tuple for callers that need to extract further arguments.
func getRequiredBinaryInputPort(o values.Value, name string) (values.BinaryReader, values.Tuple, error) {
	tuple, ok := o.(values.Tuple)
	if !ok {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAList, "%s: expected a list but got %T", name, o)
	}
	if !tuple.IsList() {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAList, "%s: expected a list but got %s", name, tuple.SchemeString())
	}
	if tuple.IsEmptyList() {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAByteInputPort, "%s: no binary input port specified", name)
	}
	p, ok := tuple.Car().(values.BinaryReader)
	if !ok {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAnInputPort, "%s: expected a binary input port but got %T", name, tuple.Car())
	}
	return p, tuple, nil
}

// getRequiredBinaryOutputPort extracts a required binary output port from a variadic argument list.
// Returns an error if the list is empty (no default port for binary I/O).
// Also returns the validated tuple for callers that need to extract further arguments.
func getRequiredBinaryOutputPort(o values.Value, name string) (values.BinaryWriter, values.Tuple, error) {
	tuple, ok := o.(values.Tuple)
	if !ok {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAList, "%s: expected a list but got %T", name, o)
	}
	if !tuple.IsList() {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAList, "%s: expected a list but got %s", name, tuple.SchemeString())
	}
	if tuple.IsEmptyList() {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAByteOutputPort, "%s: no binary output port specified", name)
	}
	p, ok := tuple.Car().(values.BinaryWriter)
	if !ok {
		return nil, nil, werr.WrapForeignErrorf(werr.ErrNotAnOutputPort, "%s: expected a binary output port but got %T", name, tuple.Car())
	}
	return p, tuple, nil
}

// PrimRead implements the (read) primitive.
// Reads a Scheme datum from port.
// Reads from the current input port if no port is specified.
// R7RS §6.13.2: read uses datum labels to handle circular and shared structures.
func PrimRead(mc *machine.MachineContext) error {
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
func PrimReadToken(mc *machine.MachineContext) error {
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
func PrimReadSyntax(mc *machine.MachineContext) error {
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

// PrimWrite implements the write primitive.
// Writes a machine-readable representation of an object to the current output port or to the specified port.
// R7RS §6.13.3: write uses datum labels to handle circular and shared structures.
func PrimWrite(mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	writer, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	// Use cycle-aware writer to handle circular structures
	_, err = writer.Write([]byte(values.WriteValueToString(obj)))
	if err != nil {
		return werr.WrapForeignErrorf(err, "error writing to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "error flushing output port")
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimWriteChar implements the write-char primitive.
// Writes a character to the current output port or to the specified output port.
func PrimWriteChar(mc *machine.MachineContext) error {
	ch, err := helpers.RequireArg[*values.Character](mc, 0, werr.ErrNotACharacter, "write-char")
	if err != nil {
		return err
	}
	o := mc.Arg(1)
	tuple, ok := o.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "expected a list but got %T", o)
	}
	if !tuple.IsList() {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "expected a list but got %s", tuple.SchemeString())
	}
	var writer values.OutputPort
	if tuple.IsEmptyList() {
		writer = GetCurrentOutputPort()
	} else {
		p, ok := tuple.Car().(values.OutputPort)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAnOutputPort, "expected an output port but got %T", tuple.Car())
		}
		writer = p
	}
	_, isBinaryWriter := writer.(values.BinaryWriter)
	if isBinaryWriter {
		return werr.WrapForeignErrorf(werr.ErrNotATextualPort,
			"write-char: expected a textual output port, got binary port")
	}
	buf := make([]byte, 0, utf8.UTFMax)
	_, err = writer.Write(utf8.AppendRune(buf, ch.Value))
	if err != nil {
		return werr.WrapForeignErrorf(err, "error writing character to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "error flushing output port")
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimDisplay implements the (display) primitive.
// Writes a human-readable representation of an object to an output port.
// R7RS §6.13.3: display uses datum labels to handle circular and shared structures.
func PrimDisplay(mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	writer, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	// Use cycle-aware writer to handle circular structures
	_, err = writer.Write([]byte(values.DisplayValueToString(obj)))
	if err != nil {
		return werr.WrapForeignErrorf(err, "error writing to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "error flushing output port")
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimNewline implements the newline primitive.
// Writes a newline character to the output port.
func PrimNewline(mc *machine.MachineContext) error {
	writer, err := getOptionalTextualOutputPort(mc, 0)
	if err != nil {
		return err
	}
	_, err = writer.Write([]byte("\n"))
	if err != nil {
		return werr.WrapForeignErrorf(err, "error writing newline to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "error flushing output port")
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimWriteSimple implements the write-simple primitive (R7RS).
// Writes a machine-readable representation of an object without using datum labels
// for shared or circular structure. This is the same as write for non-circular data.
// (write-simple obj) or (write-simple obj port)
func PrimWriteSimple(mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	writer, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	_, err = writer.Write([]byte(obj.SchemeString()))
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-simple: error writing to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-simple: error flushing output port")
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimWriteShared implements the write-shared primitive (R7RS).
// Writes a machine-readable representation of an object using datum labels
// (#n= and #n#) for shared and circular structure.
// R7RS §6.13.3: write-shared always uses datum labels for shared structure.
//
// (write-shared obj) or (write-shared obj port)
func PrimWriteShared(mc *machine.MachineContext) error {
	obj := mc.Arg(0)
	writer, err := getOptionalTextualOutputPort(mc, 1)
	if err != nil {
		return err
	}
	// Use cycle-aware writer with datum labels for all shared structure
	_, err = writer.Write([]byte(values.WriteSharedValueToString(obj)))
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-shared: error writing to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-shared: error flushing output port")
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimReadChar implements the read-char primitive.
// R7RS §6.13.2: (read-char [port])
// Reads and returns a single character from the input port.
func PrimReadChar(mc *machine.MachineContext) error {
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
func PrimPeekChar(mc *machine.MachineContext) error {
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
func PrimReadLine(mc *machine.MachineContext) error {
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
func PrimCharReadyQ(mc *machine.MachineContext) error {
	// For now, we assume a character is always ready for string input ports
	// and the character input port (stdin may block, but we can't easily check)
	// A more accurate implementation would need non-blocking I/O
	mc.SetValue(values.TrueValue)
	return nil
}

// PrimReadString implements the read-string primitive.
// R7RS §6.13.2: (read-string k [port])
// Reads up to k characters from the input port and returns them as a string.
func PrimReadString(mc *machine.MachineContext) error {
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

// PrimWriteString implements the write-string primitive.
// R7RS §6.13.3: (write-string string [port [start [end]]])
// Writes the characters of string (optionally between start and end) to port.
func PrimWriteString(mc *machine.MachineContext) error {
	rest := mc.Arg(1)

	str, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "write-string")
	if err != nil {
		return err
	}

	runes := str.Runes()
	length := len(runes)
	start := 0
	end := length

	// Parse optional arguments: [port [start [end]]]
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "write-string: expected a list but got %T", rest)
	}
	if !tuple.IsList() {
		return werr.WrapForeignErrorf(werr.ErrNotAList, "write-string: expected a list but got %s", tuple.SchemeString())
	}

	var writer values.OutputPort
	if tuple.IsEmptyList() {
		writer = GetCurrentOutputPort()
	} else {
		p, ok := tuple.Car().(values.OutputPort)
		if !ok {
			return werr.WrapForeignErrorf(werr.ErrNotAnOutputPort, "write-string: expected an output port but got %T", tuple.Car())
		}
		writer = p

		start, end, err = helpers.ParseSubrange(tuple.Cdr(), length, "write-string")
		if err != nil {
			return err
		}
	}

	// WriteByte the substring
	_, err = writer.Write([]byte(string(runes[start:end])))
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-string: error writing to output port")
	}
	err = writer.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-string: error flushing output port")
	}
	mc.SetValue(values.Void)
	return nil
}

// PrimWriteU8 implements the write-u8 primitive.
// R7RS §6.13.3: (write-u8 byte [port])
// Writes byte to the given binary output port and returns an unspecified value.
func PrimWriteU8(mc *machine.MachineContext) error {
	byteVal := mc.Arg(0)

	// Validate byte argument (must be exact integer 0-255)
	var b byte
	switch v := byteVal.(type) {
	case *values.Integer:
		err := values.ValidateByteValue(v, "write-u8", "byte")
		if err != nil {
			return err
		}
		b = byte(v.Value)
	default:
		return werr.WrapForeignErrorf(werr.ErrNotANumber, "write-u8: expected an exact integer but got %T", byteVal)
	}

	p, _, err := getRequiredBinaryOutputPort(mc.Arg(1), "write-u8")
	if err != nil {
		return err
	}
	err = p.WriteByte(b)
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-u8: error writing byte")
	}

	mc.SetValue(values.Void)
	return nil
}

// PrimReadU8 implements the read-u8 primitive.
// R7RS §6.13.3: (read-u8 [port])
// Reads the next byte from the given binary input port and returns it as an exact integer.
// Returns eof-object at end of file.
func PrimReadU8(mc *machine.MachineContext) error {
	p, _, err := getRequiredBinaryInputPort(mc.Arg(0), "read-u8")
	if err != nil {
		return err
	}
	b, err := p.ReadByte()
	if errors.Is(err, io.EOF) {
		mc.SetValue(values.EOFObject)
		return nil
	}
	if err != nil {
		return werr.WrapForeignReadErrorf(err, "read-u8: error reading byte")
	}
	mc.SetValue(values.NewInteger(int64(b)))

	return nil
}

// PrimPeekU8 implements the peek-u8 primitive.
// R7RS §6.13.3: (peek-u8 [port])
// Like read-u8, but does not consume the byte from the port.
func PrimPeekU8(mc *machine.MachineContext) error {
	p, _, err := getRequiredBinaryInputPort(mc.Arg(0), "peek-u8")
	if err != nil {
		return err
	}
	b, err := p.ReadByte()
	if errors.Is(err, io.EOF) {
		mc.SetValue(values.EOFObject)
		return nil
	}
	if err != nil {
		return werr.WrapForeignReadErrorf(err, "peek-u8: error reading byte")
	}
	// Unread the byte so it can be read again
	err = p.UnreadByte()
	if err != nil {
		return werr.WrapForeignErrorf(err, "peek-u8: error unreading byte")
	}
	mc.SetValue(values.NewInteger(int64(b)))

	return nil
}

// PrimU8ReadyQ implements the u8-ready? primitive.
// R7RS §6.13.3: (u8-ready? [port])
// Returns #t if a byte is available for reading from the binary input port.
func PrimU8ReadyQ(mc *machine.MachineContext) error {
	// For now, assume a byte is always ready for bytevector input ports
	// A more accurate implementation would need non-blocking I/O
	mc.SetValue(values.TrueValue)
	return nil
}

// PrimReadBytevector implements the read-bytevector primitive.
// R7RS §6.13.3: (read-bytevector k [port])
// Reads the next k bytes from port into a newly allocated bytevector.
// Returns eof-object if no bytes are available before end of file.
func PrimReadBytevector(mc *machine.MachineContext) error {
	k, err := helpers.RequireArg[*values.Integer](mc, 0, werr.ErrNotANumber, "read-bytevector")
	if err != nil {
		return err
	}
	if k.Value < 0 {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "read-bytevector: k must be non-negative")
	}

	// Check allocation limit
	if k.Value > MaxReadBytevectorBytes {
		return werr.WrapForeignErrorf(werr.ErrAllocationLimitExceeded,
			"read-bytevector: requested allocation (%d bytes, %d MB) exceeds maximum (%d MB)",
			k.Value,
			k.Value/(1024*1024),
			MaxReadBytevectorBytes/(1024*1024),
		)
	}

	p, _, err := getRequiredBinaryInputPort(mc.Arg(1), "read-bytevector")
	if err != nil {
		return err
	}

	// Read exactly k bytes using io.ReadFull to avoid short reads from buffered ports.
	// R7RS §6.13.3: read-bytevector returns a bytevector of exactly k bytes if available.
	buf := make([]byte, k.Value)
	n, err := io.ReadFull(p, buf)
	switch {
	case err == nil:
		// Full read: proceed with buf as-is.
	case errors.Is(err, io.ErrUnexpectedEOF):
		// Partial read at EOF: return the bytes we got.
		buf = buf[:n]
	case errors.Is(err, io.EOF):
		// No bytes at all: return eof-object.
		mc.SetValue(values.EOFObject)
		return nil
	default:
		return werr.WrapForeignReadErrorf(err, "read-bytevector: error reading from port")
	}

	bv := make(values.ByteVector, len(buf))
	for i := range buf {
		bv[i] = &values.Byte{Value: buf[i]}
	}
	mc.SetValue(&bv)
	return nil
}

// PrimReadBytevectorBang implements the read-bytevector! primitive.
// R7RS §6.13.3: (read-bytevector! bytevector [port [start [end]]])
// Reads bytes from port into an existing bytevector.
// Returns the number of bytes read, or eof-object if no bytes available.
func PrimReadBytevectorBang(mc *machine.MachineContext) error {
	bv, err := helpers.RequireArg[*values.ByteVector](mc, 0, werr.ErrNotAByteVector, "read-bytevector!")
	if err != nil {
		return err
	}

	p, tuple, err := getRequiredBinaryInputPort(mc.Arg(1), "read-bytevector!")
	if err != nil {
		return err
	}

	// Extract optional start/end arguments
	start, end, err := helpers.ParseSubrange(tuple.Cdr(), len(*bv), "read-bytevector!")
	if err != nil {
		return err
	}

	// Read exactly (end-start) bytes using io.ReadFull to avoid short reads from buffered ports.
	// R7RS §6.13.3: read-bytevector! fills the range [start,end) if bytes are available.
	buf := make([]byte, end-start)
	n, err := io.ReadFull(p, buf)
	if err != nil {
		if errors.Is(err, io.ErrUnexpectedEOF) {
			// Partial read at EOF: copy what we got and return the count.
			for i := range n {
				(*bv)[start+i] = values.NewByte(buf[i])
			}
			mc.SetValue(values.NewInteger(int64(n)))
			return nil
		}
		if errors.Is(err, io.EOF) {
			mc.SetValue(values.EOFObject)
			return nil
		}
		return werr.WrapForeignReadErrorf(err, "read-bytevector!: error reading from port")
	}

	for i := range n {
		(*bv)[start+i] = values.NewByte(buf[i])
	}
	mc.SetValue(values.NewInteger(int64(n)))
	return nil
}

// PrimWriteBytevector implements the write-bytevector primitive.
// R7RS §6.13.3: (write-bytevector bytevector [port [start [end]]])
// Writes the bytes of bytevector to port.
func PrimWriteBytevector(mc *machine.MachineContext) error {
	bv, err := helpers.RequireArg[*values.ByteVector](mc, 0, werr.ErrNotAByteVector, "write-bytevector")
	if err != nil {
		return err
	}

	p, tuple, err := getRequiredBinaryOutputPort(mc.Arg(1), "write-bytevector")
	if err != nil {
		return err
	}

	// Extract optional start/end arguments
	start, end, err := helpers.ParseSubrange(tuple.Cdr(), len(*bv), "write-bytevector")
	if err != nil {
		return err
	}

	data := bv.AsBytes(start, end)
	_, err = p.Write(data)
	if err != nil {
		return werr.WrapForeignErrorf(err, "write-bytevector: error writing to port")
	}

	mc.SetValue(values.Void)
	return nil
}

// PrimFlushOutputPort implements the flush-output-port primitive.
// R7RS §6.13.3: (flush-output-port [port])
// Flushes any buffered output to the underlying output device.
func PrimFlushOutputPort(mc *machine.MachineContext) error {
	port, err := getOptionalOutputPort(mc, 0)
	if err != nil {
		return err
	}

	err = port.Flush()
	if err != nil {
		return werr.WrapForeignErrorf(err, "flush-output-port: error flushing port")
	}

	mc.SetValue(values.Void)
	return nil
}
