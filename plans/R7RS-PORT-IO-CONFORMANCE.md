# R7RS Conformance: Ports & I/O (sections 6.7-6.13)

## Implementation Architecture

Wile's port system uses a clean type hierarchy rooted in Go interfaces:

```
Port (Close, IsClosed)
  InputPort (Read)
    TextualReader (ReadRune, UnreadRune)  -- for read-char, peek-char, read-line, etc.
    BinaryReader  (ReadByte, UnreadByte)  -- for read-u8, peek-u8, etc.
  OutputPort (Write, Flush)
    TextualWriter (WriteRune)             -- for write-char
    BinaryWriter  (WriteByte)             -- for write-u8, write-bytevector
  InputOutputPort (InputPort + OutputPort)
```

Concrete port types and their backing storage:

| Type | Role | Backing Store | Uses bufio? |
|------|------|---------------|-------------|
| `CharacterInputPort` | Textual input (files, stdin) | `*bufio.Reader` | Yes |
| `CharacterOutputPort` | Textual output (files, stdout) | `*bufio.Writer` | Yes |
| `BinaryInputPort` | Binary input (files) | `*bufio.Reader` | Yes |
| `BinaryOutputPort` | Binary output (files) | `*bufio.Writer` | Yes |
| `StringInputPort` | `open-input-string` | `*bytes.Buffer` | No |
| `StringOutputPort` | `open-output-string` | `*bytes.Buffer` | No |
| `ByteVectorInputPort` | `open-input-bytevector` | `*bufio.Reader` wrapping `*bytes.Reader` | Yes |
| `ByteVectorBufferedOutputPort` | `open-output-bytevector` | `*bytes.Buffer` | No |
| `ByteVectorOutputPort` | Binary output (files) | `*bufio.Writer` | Yes |
| `ByteVectorInputOutputPort` | Combined binary I/O | `*bytes.Buffer` | No |

Key design choice: `bufio.Reader`/`bufio.Writer` wrap file-backed I/O for performance. In-memory ports
(`StringInputPort`, `StringOutputPort`, `ByteVectorBufferedOutputPort`, `ByteVectorInputOutputPort`)
use `bytes.Buffer` directly -- no `bufio` layer.

Exception: `ByteVectorInputPort` (from `open-input-bytevector`) wraps `bytes.Reader` in `bufio.Reader`.
This adds unnecessary overhead for in-memory reads and introduces the short-read issue documented below.

## bufio-Specific Findings

### Finding 1 (HIGH): `read-bytevector` / `read-bytevector!` short reads on file ports

**Root cause:** Both operations use a single `p.Read(buf)` call, which delegates to
`bufio.Reader.Read()`. Per Go's `io.Reader` contract, `Read` may return fewer bytes
than requested even when more data is available. Specifically, `bufio.Reader.Read()`
returns only what is currently in its internal buffer (default 4096 bytes) without
refilling.

**Reproduction:**
```scheme
;; Write 8192 bytes to file, then read-bytevector 4000 twice
;; Expected: (4000 4000)
;; Actual:   (4000 96)
```

The first `read-bytevector 4000` triggers a 4096-byte fill, returns 4000, leaving
96 in the buffer. The second `read-bytevector 4000` returns only the 96 buffered
bytes without refilling.

**R7RS spec (section 6.13.3):** "Reads the next k bytes, or as many as are available
before the end of file, whichever is fewer." The 4096 remaining bytes are available;
returning 96 violates the spec.

**Fix:** Replace `p.Read(buf)` with `io.ReadFull(p, buf)` or `io.ReadAtLeast(p, buf, 1)`
in a loop. Handle `io.ErrUnexpectedEOF` to return partial results at actual EOF.

**Affected code:**
- `internal/extensions/io/prim_read_write.go`: `PrimReadBytevector` (single `p.Read(buf)` call)
- `internal/extensions/io/prim_read_write.go`: `PrimReadBytevectorBang` (same pattern)

**Not affected:**
- `read-string` -- reads rune-by-rune in a loop, so each `ReadRune()` works correctly
- `read-char`, `peek-char` -- read single runes
- `read-u8`, `peek-u8` -- read single bytes
- In-memory bytevector ports -- `bytes.Reader` returns all available data in one `Read`

### Finding 2: `ByteVectorInputPort` unnecessarily wraps `bytes.Reader` in `bufio.Reader`

`open-input-bytevector` creates a `ByteVectorInputPort` whose internal `rdr` is
`bufio.NewReader(bytes.NewReader(data))`. Since `bytes.Reader` already provides
efficient `ReadByte` and `Read`, the `bufio.Reader` wrapper adds:
- Memory overhead (4096-byte internal buffer)
- The short-read behavior from Finding 1 (though less likely to manifest for
  small bytevectors)

This is a performance nit, not a correctness issue for typical use. It only becomes
a conformance issue combined with Finding 1.

### Finding 3: Flush-on-close works correctly

`CharacterOutputPort.Close()` and `BinaryOutputPort.Close()` both call
`flushThenClose(p.wrt, &p.portBase)`, which flushes `bufio.Writer` before closing
the underlying stream. This is correct.

### Finding 4: peek-char / peek-u8 work correctly with bufio

`peek-char` uses `ReadRune()` + `UnreadRune()` on `bufio.Reader`, which correctly
puts the rune back into the buffer. `peek-u8` uses `ReadByte()` + `UnreadByte()`.
Multiple consecutive peeks return the same value. Interleaving peek and read
works correctly, including with multibyte UTF-8 characters.

### Finding 5: String/buffer ports bypass bufio entirely

`StringInputPort`, `StringOutputPort`, `ByteVectorBufferedOutputPort`, and
`ByteVectorInputOutputPort` all use `bytes.Buffer` directly. No `bufio`-related
issues apply to these types. This is the correct design for in-memory ports.

## Non-bufio Conformance Issues

### Issue 1 (HIGH): `current-input-port` / `current-output-port` parameterize crash

**Bug:** `GetCurrentInputPort()` returns `*values.CharacterInputPort` (concrete type)
and does a hard type assertion: `currentInputPortParam.Value().(*values.CharacterInputPort)`.
When `current-input-port` is parameterized to a `StringInputPort`, this panics.

Same for `GetCurrentOutputPort()` with `*values.CharacterOutputPort`.

**Reproduction:**
```scheme
(let ((p (open-input-string "hello")))
  (parameterize ((current-input-port p))
    (read-char)))
;; => Exception: interface conversion: *values.StringInputPort, not *values.CharacterInputPort
```

**R7RS spec (section 6.13.1):** `current-input-port` is a parameter object. `parameterize` must
work with any textual input port.

**Fix:** Change return types to interfaces:
- `GetCurrentInputPort() -> values.TextualReader`
- `GetCurrentOutputPort() -> values.TextualWriter`
- Or use `values.InputPort` / `values.OutputPort` if the callers don't need rune-level methods

**Affected code:** `internal/extensions/io/state.go`: `GetCurrentInputPort`, `GetCurrentOutputPort`

### Issue 2 (MEDIUM): Textual output ops accept binary ports without error

`write-char`, `write-string`, `display`, `newline`, `write`, `write-simple`, and
`write-shared` all use `getOptionalOutputPort()` which checks for `values.OutputPort`.
Since `BinaryWriter` embeds `OutputPort`, binary ports satisfy the type check and
these textual operations silently write UTF-8 bytes to binary ports.

**R7RS spec (section 6.13.2):** "If port is omitted, it defaults to the value returned
by current-output-port. It is an error to write to a port that is not a textual port."

**Reproduction:**
```scheme
(let ((p (open-output-bytevector)))
  (write-char #\A p)
  (get-output-bytevector p))
;; => #u8(65) -- should be an error
```

**Note:** The reverse direction (binary ops on textual ports) IS correctly enforced.
`read-u8`, `peek-u8`, `write-u8`, `read-bytevector`, `write-bytevector` all check
for `BinaryReader`/`BinaryWriter` and reject textual ports.

**Fix:** Change `getOptionalOutputPort` to return `values.TextualWriter` instead
of `values.OutputPort`, or add explicit binary-port rejection. Operations like
`write-char` should check for `TextualWriter`, not just `OutputPort`.

**Affected code:** `internal/extensions/io/prim_read_write.go`:
`getOptionalOutputPort`, `PrimWriteChar`, `PrimWriteString`, `PrimDisplay`,
`PrimNewline`, `PrimWrite`, `PrimWriteSimple`, `PrimWriteShared`

### Issue 3 (LOW): `close-input-port` / `close-output-port` don't validate port direction

Both are mapped to `PrimClosePort`, which accepts any `Port`. R7RS says
`close-input-port` should only accept input ports and `close-output-port` should
only accept output ports.

**Reproduction:**
```scheme
(let ((p (open-output-string)))
  (close-input-port p))  ;; Silently succeeds, should error
```

**R7RS spec (section 6.13.1):** "Closes the resource associated with port, rendering
the port incapable of delivering or accepting characters." The naming implies
type restriction.

**Fix:** Add type checks: `close-input-port` should require `InputPort`,
`close-output-port` should require `OutputPort`.

### Issue 4 (LOW): `read-string 0` returns EOF instead of empty string

**Reproduction:**
```scheme
(let ((p (open-input-string "hello")))
  (read-string 0 p))
;; => #!eof (should be "")
```

**R7RS spec (section 6.13.2):** "Reads the next k characters... returns a string
of those characters." For k=0, zero characters are read, result should be `""`.

Note: `read-bytevector 0` correctly returns `#u8()` (empty bytevector), making
this inconsistent.

**Fix:** In `PrimReadString`, handle `k == 0` as a special case returning empty string.

### Issue 5 (LOW): `char-ready?` / `u8-ready?` always return `#t`

Both return `#t` unconditionally. For in-memory ports and at EOF, this is correct
per R7RS (EOF doesn't block). For stdin or network-backed ports, it should check
whether data is available without blocking.

**R7RS spec (section 6.13.2):** "Returns #t if a character is ready on the textual
input port and returns #f otherwise."

For `bufio.Reader`, `Buffered() > 0` would be a reasonable approximation. However,
this is a common simplification in Scheme implementations and unlikely to cause
real-world issues since the main use case is interactive I/O.

### Issue 6 (INFO): Mixing `read` and `read-char` on the same port

After `(read p)` on `(open-input-string "42 hello")`, `(read-char p)` returns
`#!eof` instead of `#\space` or `#\h`. This is because the cached tokenizer/parser
consumes the entire remaining input.

R7RS does not explicitly define what happens when mixing datum-level and
character-level I/O on the same port. Most Scheme implementations document this
as implementation-defined behavior. The tokenizer is cached per port for
correctness of sequential `read` calls.

## Spec Requirements & Status

| Requirement | R7RS Section | Status | Notes |
|-------------|-------------|--------|-------|
| `port?` | 6.13.1 | PASS | Interface-based check |
| `input-port?` | 6.13.1 | PASS | |
| `output-port?` | 6.13.1 | PASS | |
| `textual-port?` | 6.13.1 | PASS | Checks `TextualReader` or `TextualWriter` |
| `binary-port?` | 6.13.1 | PASS | Checks `BinaryReader` or `BinaryWriter` |
| `input-port-open?` | 6.13.1 | PASS | |
| `output-port-open?` | 6.13.1 | PASS | |
| `close-port` | 6.13.1 | PASS | Flushes bufio.Writer, idempotent |
| `close-input-port` | 6.13.1 | PASS | Fixed: `PrimCloseInputPort` validates direction (PR #364) |
| `close-output-port` | 6.13.1 | PASS | Fixed: `PrimCloseOutputPort` validates direction (PR #364) |
| `call-with-port` | 6.13.1 | PASS | Closes port on exit, returns value |
| `open-input-string` | 6.13.1 | PASS | Uses bytes.Buffer directly |
| `open-output-string` | 6.13.1 | PASS | Uses bytes.Buffer directly |
| `get-output-string` | 6.13.1 | PASS | Works after close |
| `open-input-bytevector` | 6.13.1 | PASS | Uses bufio.Reader over bytes.Reader |
| `open-output-bytevector` | 6.13.1 | PASS | Uses bytes.Buffer directly |
| `get-output-bytevector` | 6.13.1 | PASS | Works after close |
| `open-input-file` | 6.13.1 | PASS | CharacterInputPort from os.File |
| `open-output-file` | 6.13.1 | PASS | CharacterOutputPort from os.File |
| `open-binary-input-file` | 6.13.1 | PASS | BinaryInputPort from os.File |
| `open-binary-output-file` | 6.13.1 | PASS | BinaryOutputPort from os.File |
| `eof-object` | 6.13.1 | PASS | Singleton |
| `eof-object?` | 6.13.1 | PASS | Identity comparison |
| `read-char` | 6.13.2 | PASS | Rejects binary ports correctly |
| `peek-char` | 6.13.2 | PASS | ReadRune + UnreadRune; multibyte correct |
| `read-line` | 6.13.2 | PASS | Handles \n, \r\n, bare \r |
| `read-string` | 6.13.2 | PASS | Fixed: k=0 returns `""` (PR #364) |
| `char-ready?` | 6.13.2 | PARTIAL | Always returns #t (Issue 5) |
| `write-char` | 6.13.2 | PASS | Fixed: `getOptionalTextualOutputPort` rejects binary ports (PR #364) |
| `write-string` | 6.13.2 | PASS | Fixed: `getOptionalTextualOutputPort` rejects binary ports (PR #364) |
| `newline` | 6.13.2 | PASS | Fixed: `getOptionalTextualOutputPort` rejects binary ports (PR #364) |
| `display` | 6.13.3 | PASS | Fixed: `getOptionalTextualOutputPort` rejects binary ports (PR #364) |
| `write` | 6.13.3 | PASS | Fixed: `getOptionalTextualOutputPort` rejects binary ports (PR #364) |
| `write-simple` | 6.13.3 | PASS | Fixed: `getOptionalTextualOutputPort` rejects binary ports (PR #364) |
| `write-shared` | 6.13.3 | PASS | Fixed: `getOptionalTextualOutputPort` rejects binary ports (PR #364) |
| `read-u8` | 6.13.3 | PASS | Rejects textual ports correctly |
| `peek-u8` | 6.13.3 | PASS | ReadByte + UnreadByte |
| `u8-ready?` | 6.13.3 | PARTIAL | Always returns #t (Issue 5) |
| `write-u8` | 6.13.3 | PASS | Rejects textual ports correctly |
| `read-bytevector` | 6.13.3 | PASS | Fixed: `io.ReadFull` replaces single `Read` call (PR #364) |
| `read-bytevector!` | 6.13.3 | PASS | Fixed: `io.ReadFull` replaces single `Read` call (PR #364) |
| `write-bytevector` | 6.13.3 | PASS | Rejects textual ports correctly |
| `flush-output-port` | 6.13.3 | PASS | Delegates to bufio.Writer.Flush |
| `read` | 6.13.2 | PASS | Cached parser per port |
| `current-input-port` parameterize | 6.13.1 | PASS | Fixed: returns `TextualReader` interface (PR #364) |
| `current-output-port` parameterize | 6.13.1 | PASS | Fixed: returns `OutputPort` interface (PR #364) |

## Edge Case Results

| Expression | Expected (R7RS) | Actual (Wile) | Verdict |
|-----------|-----------------|---------------|---------|
| `(let ((p (open-input-string "abc"))) (list (peek-char p) (peek-char p) (read-char p) (read-char p)))` | `(#\a #\a #\a #\b)` | `(#\a #\a #\a #\b)` | PASS |
| `(let ((p (open-input-string "hello\nworld"))) (list (read-line p) (read-line p) (read-line p)))` | `("hello" "world" #!eof)` | `("hello" "world" #!eof)` | PASS |
| `(let ((p (open-input-bytevector #u8(1 2 3)))) (list (read-u8 p) (peek-u8 p) (read-u8 p)))` | `(1 2 2)` | `(1 2 2)` | PASS |
| `(let ((p (open-output-string))) (write-string "hello" p) (close-port p) (get-output-string p))` | `"hello"` | `"hello"` | PASS |
| `(let ((p (open-input-string "hello"))) (guard (e (#t 'error)) (read-u8 p)))` | error | error | PASS |
| `(let ((p (open-input-bytevector #u8(65)))) (guard (e (#t 'error)) (read-char p)))` | error | error | PASS |
| `(let ((p (open-input-bytevector #u8(1 2 3 4 5)))) (read-bytevector 3 p))` | `#u8(1 2 3)` | `#u8(1 2 3)` | PASS |
| `(let ((p (open-input-string "hi"))) (list (port? p) (input-port? p) (output-port? p) (textual-port? p) (binary-port? p)))` | `(#t #t #f #t #f)` | `(#t #t #f #t #f)` | PASS |
| `(let ((p (open-input-string "hello world"))) (read-string 5 p))` | `"hello"` | `"hello"` | PASS |
| `(let ((p (open-output-string))) (write-string "test" p) (flush-output-port p) (get-output-string p))` | `"test"` | `"test"` | PASS |
| `(let ((p (open-input-string "hi"))) (let ((b (input-port-open? p))) (close-port p) (list b (input-port-open? p))))` | `(#t #f)` | `(#t #f)` | PASS |
| `(let ((p (open-input-string "hi"))) (close-port p) (guard (e (#t 'error)) (read-char p)))` | error | error | PASS |
| `(let ((p (open-output-bytevector))) (write-bytevector #u8(1 2 3 4 5) p 1 4) (get-output-bytevector p))` | `#u8(2 3 4)` | `#u8(2 3 4)` | PASS |
| `(let ((p (open-input-string "hi"))) (call-with-port p (lambda (port) (read-char port))) (input-port-open? p))` | `#f` | `#f` | PASS |
| `(let ((p (open-output-bytevector))) (guard (e (#t 'error)) (write-char #\A p)))` | error | error | PASS (fixed PR #364) |
| `(let ((p (open-output-string))) (guard (e (#t 'error)) (close-input-port p)))` | error | error | PASS (fixed PR #364) |
| `(let ((p (open-input-string "hello"))) (read-string 0 p))` | `""` | `""` | PASS (fixed PR #364) |
| `(parameterize ((current-input-port (open-input-string "hi"))) (read-char))` | `#\h` | `#\h` | PASS (fixed PR #364) |
| `(parameterize ((current-output-port (open-output-string))) (write-char #\A))` | (void) | (void) | PASS (fixed PR #364) |
| 8192-byte file, two `read-bytevector 4000` calls | `(4000 4000)` | `(4000 4000)` | PASS (fixed PR #364) |
| `(let ((p (open-input-string "a\r\nb\rc"))) (list (read-line p) (read-line p) (read-line p)))` | `("a" "b" "c")` | `("a" "b" "c")` | PASS |

## Issues Found (Prioritized)

### 1. [HIGH] `read-bytevector` / `read-bytevector!` return fewer bytes than available (bufio short read) — FIXED

- **Severity:** High -- incorrect behavior for file-backed binary ports
- **Spec:** R7RS 6.13.3: "Reads the next k bytes, or as many as are available before the end of file"
- **Cause:** Single `p.Read(buf)` call; `bufio.Reader.Read()` returns only buffered data
- **Location:** `internal/extensions/io/prim_read_write.go`, `PrimReadBytevector` and `PrimReadBytevectorBang`
- **Resolution:** Replaced with `io.ReadFull` (PR #364)

### 2. [HIGH] `current-input-port` / `current-output-port` crash when parameterized to non-Character port — FIXED

- **Severity:** High -- crashes the interpreter
- **Spec:** R7RS 6.13.1: parameter objects work with any port of appropriate direction
- **Cause:** Hard type assertion to `*CharacterInputPort` / `*CharacterOutputPort`
- **Location:** `internal/extensions/io/state.go`, `GetCurrentInputPort` and `GetCurrentOutputPort`
- **Resolution:** Returns `TextualReader` / `OutputPort` interfaces (PR #364)

### 3. [MEDIUM] Textual output operations accept binary ports — FIXED

- **Severity:** Medium -- silently produces UTF-8 bytes in binary ports
- **Spec:** R7RS 6.13.2: "It is an error" to use textual ops on binary ports
- **Cause:** `getOptionalOutputPort` checks `OutputPort` not `TextualWriter`
- **Location:** `internal/extensions/io/prim_read_write.go`
- **Affected ops:** `write-char`, `write-string`, `display`, `newline`, `write`, `write-simple`, `write-shared`
- **Resolution:** Added `getOptionalTextualOutputPort` with explicit binary rejection (PR #364)

### 4. [LOW] `close-input-port` / `close-output-port` accept wrong port direction — FIXED

- **Severity:** Low -- harmless but non-conformant
- **Spec:** R7RS 6.13.1 implies direction-specific behavior
- **Location:** `internal/extensions/io/register.go` (both map to `PrimClosePort`)
- **Resolution:** Separate `PrimCloseInputPort` / `PrimCloseOutputPort` with direction checks (PR #364)

### 5. [LOW] `read-string 0` returns EOF instead of empty string — FIXED

- **Severity:** Low -- edge case
- **Spec:** R7RS 6.13.2: zero characters read should return `""`
- **Location:** `internal/extensions/io/prim_read_write.go`, `PrimReadString`
- **Resolution:** Early return for `k == 0` (PR #364)

### 6. [LOW] `char-ready?` / `u8-ready?` always return `#t` — WONTFIX

- **Severity:** Low -- only affects interactive/network I/O
- **Spec:** R7RS 6.13.2/6.13.3: should check for non-blocking availability
- **Location:** `internal/extensions/io/prim_read_write.go`
- **Note:** Documented semantic difference (L7). Common simplification across Scheme implementations.
