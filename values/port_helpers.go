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

package values

import (
	"errors"
	"io"
)

// Unexported interfaces for backing-type methods not in the stdlib.

type byteUnreader interface {
	UnreadByte() error
}

type runeUnreader interface {
	UnreadRune() error
}

type runeWriter interface {
	WriteRune(rune) (int, error)
}

type flusher interface {
	Flush() error
}

// Guard-and-delegate helpers. Each guards on portBase.closed,
// then delegates to the backing type via the narrowest interface.

func guardedRead(b *portBase, r io.Reader, bs []byte) (int, error) {
	err := b.guardClosed()
	if err != nil {
		return 0, err
	}
	return r.Read(bs)
}

func guardedReadByte(b *portBase, r io.ByteReader) (byte, error) {
	err := b.guardClosed()
	if err != nil {
		return 0, err
	}
	return r.ReadByte()
}

func guardedUnreadByte(b *portBase, r byteUnreader) error {
	err := b.guardClosed()
	if err != nil {
		return err
	}
	return r.UnreadByte()
}

func guardedReadRune(b *portBase, r io.RuneReader) (rune, int, error) {
	err := b.guardClosed()
	if err != nil {
		return 0, 0, err
	}
	return r.ReadRune()
}

func guardedUnreadRune(b *portBase, r runeUnreader) error {
	err := b.guardClosed()
	if err != nil {
		return err
	}
	return r.UnreadRune()
}

func guardedWrite(b *portBase, w io.Writer, bs []byte) (int, error) {
	err := b.guardClosed()
	if err != nil {
		return 0, err
	}
	return w.Write(bs)
}

func guardedWriteByte(b *portBase, w io.ByteWriter, c byte) error {
	err := b.guardClosed()
	if err != nil {
		return err
	}
	return w.WriteByte(c)
}

func guardedWriteString(b *portBase, w io.StringWriter, s string) (int, error) {
	err := b.guardClosed()
	if err != nil {
		return 0, err
	}
	return w.WriteString(s)
}

func guardedWriteRune(b *portBase, w runeWriter, rn rune) (int, error) {
	err := b.guardClosed()
	if err != nil {
		return 0, err
	}
	return w.WriteRune(rn)
}

func guardedFlush(b *portBase, f flusher) error {
	err := b.guardClosed()
	if err != nil {
		return err
	}
	return f.Flush()
}

// flushThenClose flushes buffered data then closes the port.
// If both flush and close fail, both errors are preserved via errors.Join.
func flushThenClose(f flusher, b *portBase) error {
	flushErr := f.Flush()
	closeErr := b.Close()
	if closeErr != nil {
		return errors.Join(closeErr, flushErr)
	}
	return flushErr
}
