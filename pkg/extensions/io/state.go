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
	"bytes"
	"context"
	"os"
	"strings"
	"sync"
	"sync/atomic"

	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// State is the per-engine I/O port + cache state. One per Namespace, reached by
// primitives via stateFrom(cc). Replaces the former package globals so two
// Engines in one process do not share port parameters or cache (staff-sweep #8).
//
// The parsers cache is keyed by port value; keeping it in the same per-engine
// struct as the port parameters is what makes its isolation hold — two engines
// reading their default current-input-port now key on distinct default port
// objects, so their read positions no longer collide.
type State struct {
	inPort, outPort, errPort *machine.Parameter

	// mu protects the parsers cache map — get-or-create and evict only. It is
	// never held across a read: a read runs under the port's own read lock
	// (values.PortObject.LockReads), so a blocking read on one port neither
	// serialises other ports nor freezes the engine's I/O. The order is the
	// port's read lock, then mu.
	mu sync.Mutex
	// parsers caches parsers per input port. Entries are evicted via
	// evictPortCache() on port close or EOF. Ports abandoned without close or EOF
	// retain their entries until the engine is collected.
	parsers map[values.Value]*parserEntry
	// deferred counts the entries whose parser holds a reader fault deferred
	// past the datum it returned (parserEntry.deferred), so the character
	// readers skip the cache when it is zero, the common case, without taking
	// mu. It changes only under mu, together with an entry's deferred flag.
	// A stale read is harmless in both directions: a nonzero count from
	// another port costs one cache lookup, and a deferral on THIS port was
	// counted under the port's read lock, which the reader checking it now
	// holds, so the lock orders the increment before the load.
	deferred atomic.Int64
}

// parserEntry is a port's cached parser and the state that decides whether it
// may continue. Its fields are touched only under the port's read lock, which
// every read primitive holds for its whole call (readSyntaxLocked), so reads
// on one port serialise and reads on DIFFERENT ports never contend. The parser
// is constructed lazily under that lock (its tokenizer reads the first rune on
// the first read, so construction is itself I/O that must stay off State.mu).
type parserEntry struct {
	parser *parser.Parser
	mk     func() *parser.Parser // constructor; nil once parser is built

	// held is set when the port refused to take the lookahead rune back: the
	// tokenizer then holds the only copy of that rune. heldAt is the port's
	// position at that moment. While the port still stands there the held
	// rune is still the port's next rune and the parser is the right one to
	// continue with; once the port has moved it is not, and the parser is
	// replaced.
	held   bool
	heldAt values.SourceIndexes

	// deferred says the parser holds a fault past its last datum and is
	// counted in State.deferred. Guarded by State.mu, not the port's lock:
	// eviction (close-port) clears it without the read lock.
	deferred bool
}

// NewState builds a fresh per-engine State: three port parameters over the
// process std streams, and an empty cache. Fully constructed here — there is no
// lazy initialization, unlike the former package-global InitState.
//
// auth gates each host stream. This is the only gate the three pre-opened ports
// have: they are capability objects the engine hands to Scheme at construction,
// so the question "may this engine touch the host's stdin?" is answered once,
// here, rather than on every read. A nil authorizer allows, per the open-by-
// default convention in security.CheckWithAuthorizer. Before this gate existed a
// WithProfile(Console) engine under WithAuthorizer(DenyAll()) still read the
// host's stdin and wrote its stdout, with the authorizer recording no requests
// at all (reviews/2026-08-07/REVIEW.md 2.1.1).
func NewState(auth security.Authorizer) *State {
	return &State{
		inPort:  machine.NewParameter(gatedInputPort(auth, security.StreamStdin, os.Stdin), nil, machine.MutableBase),
		outPort: machine.NewParameter(gatedOutputPort(auth, security.StreamStdout, os.Stdout), nil, machine.MutableBase),
		errPort: machine.NewParameter(gatedOutputPort(auth, security.StreamStderr, os.Stderr), nil, machine.MutableBase),
		parsers: map[values.Value]*parserEntry{},
	}
}

// streamLabel is the name a host stream's port carries: "<stdin>", matching the
// CLI's own "<eval>" and "<stdin>" spelling. The angle brackets keep it from
// colliding with a file that happens to be called stdin, and it has no path
// separator, so include resolution never mistakes it for a location on disk.
func streamLabel(target string) string {
	return "<" + target + ">"
}

// gatedInputPort returns a textual input port over stream when auth permits
// stream:read on it, and a refused port otherwise.
func gatedInputPort(auth security.Authorizer, target string, stream *os.File) *values.PortObject {
	err := security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceStream,
		Action:   security.ActionRead,
		Target:   target,
	})
	if err != nil {
		return refusedPort(values.NewCharacterInputPortFromReaderWithName(strings.NewReader(""), streamLabel(target)))
	}
	return values.NewCharacterInputPortFromReaderWithName(stream, streamLabel(target))
}

// gatedOutputPort returns a textual output port over stream when auth permits
// stream:write on it, and a refused port otherwise.
func gatedOutputPort(auth security.Authorizer, target string, stream *os.File) *values.PortObject {
	err := security.CheckWithAuthorizer(auth, security.AccessRequest{
		Resource: security.ResourceStream,
		Action:   security.ActionWrite,
		Target:   target,
	})
	if err != nil {
		return refusedPort(values.NewCharacterOutputPortFromWriterWithName(&bytes.Buffer{}, streamLabel(target)))
	}
	return values.NewCharacterOutputPortFromWriterWithName(stream, streamLabel(target))
}

// refusedPort marks a port closed, which is how a stream denial surfaces to
// Scheme: every operation raises ErrPortClosed at once, and
// (output-port-open? (current-output-port)) reports #f, so the refusal is
// observable rather than silent. The port is built over an empty in-memory
// buffer, never the host stream, so no descriptor is behind it even if some
// future path bypasses the closed guard. Neither backing type is an io.Closer,
// so Close() closes nothing real.
func refusedPort(port *values.PortObject) *values.PortObject {
	_ = port.Close()
	return port
}

// SetInputPort sets this engine's base input port. Used by tests and by
// embedders configuring an engine's ports before running. Per R7RS parameter
// semantics the value is not validated here; a non-textual port surfaces as a
// Scheme error when a default-port read resolves it (resolveCurrentInputPort).
func (p *State) SetInputPort(port *values.PortObject) {
	p.inPort.SetValue(port)
}

// SetOutputPort sets this engine's base output port. Not validated here (see
// SetInputPort); a non-textual port errors at resolve time.
func (p *State) SetOutputPort(port *values.PortObject) {
	p.outPort.SetValue(port)
}

// GetInputPort returns this engine's base input port, or a wrapped sentinel if
// the parameter does not hold a textual input port. It serves non-VM callers
// (tests, embedders) that read the base value directly, without going through
// the resolve-time error contract of resolveCurrentInputPort.
func (p *State) GetInputPort() (*values.PortObject, error) {
	return currentTextualInputPort("current-input-port", p.inPort.Value())
}

// GetOutputPort returns this engine's base output port, or a wrapped sentinel if
// the parameter does not hold a textual output port.
func (p *State) GetOutputPort() (*values.PortObject, error) {
	return currentTextualOutputPort("current-output-port", p.outPort.Value())
}

// stateFrom pulls the per-engine State off the CallContext's namespace, or
// returns a wrapped sentinel when the context has no namespace/State (e.g. a bare
// sub-context). The State lives on the engine's Namespace (root-delegated, like
// the file resolver); this reaches it via the already-typed CallContext surface
// rather than a dedicated interface method, keeping the opaque `any` slot off the
// public contract.
func stateFrom(cc machine.CallContext) (*State, error) {
	env := cc.EnvironmentFrame()
	if env == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrNoIOState,
			"io: context has no environment frame")
	}
	ns := env.Namespace()
	if ns == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrNoIOState,
			"io: context has no namespace")
	}
	st, ok := ns.IOState().(*State)
	if !ok || st == nil {
		return nil, werr.WrapForeignErrorf(werr.ErrNoIOState,
			"io: no per-engine I/O state on this context")
	}
	return st, nil
}

// currentTextualInputPort asserts v is a *PortObject with rune-read
// capability. Wraps the result with a sentinel-bearing error
// otherwise, so callers can match via errors.Is.
func currentTextualInputPort(name string, v values.Value) (*values.PortObject, error) {
	p, ok := v.(*values.PortObject)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAnInputPort,
			"%s: value is %T, not a port", name, v)
	}
	_, hasRR := p.AsRuneReader()
	if !hasRR {
		return nil, werr.WrapForeignErrorf(werr.ErrNotATextualPort,
			"%s: port is not textual", name)
	}
	return p, nil
}

// currentTextualOutputPort asserts v is a *PortObject with rune-write
// capability.
func currentTextualOutputPort(name string, v values.Value) (*values.PortObject, error) {
	p, ok := v.(*values.PortObject)
	if !ok {
		return nil, werr.WrapForeignErrorf(werr.ErrNotAnOutputPort,
			"%s: value is %T, not a port", name, v)
	}
	_, hasRW := p.AsRuneWriter()
	if !hasRW {
		return nil, werr.WrapForeignErrorf(werr.ErrNotATextualPort,
			"%s: port is not textual", name)
	}
	return p, nil
}

// resolveCurrentOutputPort returns the effective current output port, checking
// continuation marks (from parameterize) before falling back to the base value.
// Returns a sentinel-wrapped error (errors.Is-matchable) if the resolved value is
// not a textual output port, so the calling primitive surfaces it as a
// guard-catchable Scheme condition rather than an uncatchable host-boundary panic.
func resolveCurrentOutputPort(cc machine.CallContext) (*values.PortObject, error) {
	mc, err := machine.RequireMachineContext(cc, "current-output-port")
	if err != nil {
		return nil, err
	}
	st, err := stateFrom(cc)
	if err != nil {
		return nil, err
	}
	v := mc.ResolveParameterValue(st.outPort)
	return currentTextualOutputPort("current-output-port", v)
}

// resolveCurrentInputPort returns the effective current input port, checking
// continuation marks (from parameterize) before falling back to the base value.
// Returns a sentinel-wrapped error (errors.Is-matchable) if the resolved value is
// not a textual input port, so the calling primitive surfaces it as a
// guard-catchable Scheme condition rather than an uncatchable host-boundary panic.
func resolveCurrentInputPort(cc machine.CallContext) (*values.PortObject, error) {
	mc, err := machine.RequireMachineContext(cc, "current-input-port")
	if err != nil {
		return nil, err
	}
	st, err := stateFrom(cc)
	if err != nil {
		return nil, err
	}
	v := mc.ResolveParameterValue(st.inPort)
	return currentTextualInputPort("current-input-port", v)
}

// readSyntaxCached returns the next datum from the port's cached parser. The
// caller holds the port's read lock (readSyntaxLocked), so the entry, the
// stream and the port's position move only under it. It get-or-creates the
// port's entry under State.mu (no I/O); the parser is built lazily outside
// State.mu (its tokenizer reads the first rune on the first read), so State.mu
// is never held across a read.
//
// The cache holds reader state that outlives one datum (fold-case mode, source
// positions, the dead flag), and no input: after each datum the tokenizer's
// lookahead rune goes back to the port, so read-char and friends continue from
// the datum's end (R7RS §6.13.2). The one exception is a reader fault on that
// lookahead rune, past a complete datum: the datum is returned and the parser
// keeps the fault, which the port's next input operation reports
// (takeDeferredFault for the character readers, the next read here).
func (p *State) readSyntaxCached(ctx context.Context, port *values.PortObject, mk func() *parser.Parser) (syntax.SyntaxValue, error) {
	p.mu.Lock()
	entry, ok := p.parsers[port]
	if !ok || entry == nil {
		entry = &parserEntry{mk: mk}
		p.parsers[port] = entry
	}
	p.mu.Unlock()

	if entry.parser == nil {
		entry.parser = entry.mk()
		entry.mk = nil
	}
	if !seekToPortPosition(port, entry.parser) {
		// The tokenizer still holds a lookahead rune, so it cannot be repositioned.
		// That is right only if the port refused the rune back and has not moved
		// since: the held rune is then still the next one and is the only copy.
		// Otherwise the port has moved past it (read-char and friends took
		// characters since), and the parser's positions are stale; reader state
		// that outlives a datum (fold-case mode) is lost with it.
		at, _ := port.ReadPosition()
		if !entry.held || at != entry.heldAt {
			entry.parser = mk()
			seekToPortPosition(port, entry.parser)
		}
	}
	entry.held = false
	q, err := entry.parser.ReadSyntax(ctx)
	if err != nil {
		return nil, err
	}
	err = releaseLookahead(port, entry.parser)
	if err != nil {
		entry.held = true
		entry.heldAt, _ = port.ReadPosition()
		return nil, err
	}
	if entry.parser.PendingFault() != nil {
		p.mu.Lock()
		if !entry.deferred {
			entry.deferred = true
			p.deferred.Add(1)
		}
		p.mu.Unlock()
	}
	return q, nil
}

// takeDeferredFault returns a reader fault port's cached parser holds past the
// last datum it returned, and evicts the parser so the read after the report
// starts on whatever the stream yields next, as a reader fault evicts it in
// readSyntaxLocked. nil when there is none. The caller holds the port's read
// lock. A character reader calls it before reading: the fault came before any
// character past it, so racket's read-char reports it there too.
func (p *State) takeDeferredFault(port *values.PortObject) error {
	if p.deferred.Load() == 0 {
		return nil
	}
	p.mu.Lock()
	entry := p.parsers[port]
	held := entry != nil && entry.deferred
	p.mu.Unlock()
	if !held {
		return nil
	}
	err := entry.parser.PendingFault()
	evictPortCache(p, port)
	return err
}

// seekToPortPosition points the cached parser at where the port stands now. The
// port owns the one read position R7RS §6.13.2 gives read, read-char and
// read-syntax; the parser's tokenizer counts only what it scanned, so without
// this a read-syntax after a read-char reports where the tokenizer last stopped,
// not where the port is. Before the first read it sets where the tokenizer
// starts; afterwards it repositions the tokenizer that released its lookahead.
// It reports whether the parser now stands at the port's position.
func seekToPortPosition(port *values.PortObject, p *parser.Parser) bool {
	at, prev := port.ReadPosition()
	return p.SeekTo(at, prev)
}

// releaseLookahead hands a reader's one-rune lookahead back to port. Every
// textual input port pairs its rune reader with a rune unreader
// (port_constructors.go), as peek-char relies on.
func releaseLookahead(port *values.PortObject, rdr *parser.Parser) error {
	urr, _ := port.AsRuneUnreader()
	err := rdr.ReleaseLookahead(urr)
	if err != nil {
		return wrapReadFault(err, "error returning lookahead character to port")
	}
	return nil
}
